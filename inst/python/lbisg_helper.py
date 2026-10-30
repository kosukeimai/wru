"""lBISG helper: list scores, clustering and the geographic solve for wru.

This module provides the Python-side machinery for the list-powered BISG
(lBISG) model in the wru R package (Chasalow, Dasanaike and Imai). It is
imported from R via reticulate
(`reticulate::import_from_path("lbisg_helper", path = ...)`) and is not
typically run directly. Name embeddings are produced separately by
ebisg_helper.embed_names().

Pipeline (Algorithm 1 of the paper, per name field):
  1. name_folds(): randomly partition the distinct names into J folds with
     approximately equal numbers of people.
  2. list_scores_oof(): for each fold, fit a separate neural network for
     membership in each list on the names outside the fold and score the
     names in the fold, so a name's score never sees its own list label.
     The number of training epochs is chosen by cross-validation within the
     training folds before the model is fit to all of them.
  3. select_K() (Algorithm 2): choose the number of clusters K by the
     held-out log-likelihood ratio Q(K) over population-balanced folds of
     the geographic units.
  4. fit_posterior(): K-means on the log-odds list scores, cluster
     proportions within each geography, weighted least squares for
     P(B = b | R = r), and Bayes' rule with the geographic prior.

Also provided:
  - combine_fields(): first-name and surname posteriors combined as
    P_F * P_S / P(R | G) (lBIFSG, Section 2.5).
  - recover_prevalence(): the geographic prior recovered from the list rates
    when it is unavailable (Section 3.2) or available only for coarse groups
    (Section 3.3), with the lambda sensitivity analysis of Appendix C.
  - list_quality(): the label-free list quality measure of Section 4.3.

Numerical details that are not in the paper: the weighted least squares adds
1e-9 to the diagonal of M'WM, and negative estimates are floored at 1e-9
rather than 0, so that P(B | G) is never exactly zero; K-means centres with no
member among the fitting observations are dropped, so every cluster used has
members.

To run standalone for debugging:
  >>> import numpy as np
  >>> from lbisg_helper import lbisg_field
  >>> res = lbisg_field(emb_unique, idx, onmat_unique, geo, prior)
  >>> res["posterior"].shape  # (N, R)
"""
from __future__ import annotations

from typing import Any, Iterator, Sequence

import numpy as np
import numpy.typing as npt
import torch
import torch.nn as nn
from scipy import sparse
from scipy.optimize import nnls
from sklearn.cluster import MiniBatchKMeans
from sklearn.metrics import pairwise_distances_argmin

EPS_Z = 1e-6     # clip for the log-odds of the list scores (Algorithm 1)
FLOOR = 1e-9     # floor for P(B = b | R = r)
RIDGE = 1e-9     # added to the diagonal of M'WM
DEFAULT_GRID = [10, 15, 20, 30, 50, 75, 100, 150, 200, 300, 450, 600, 999,
                1500, 2000, 3000, 5000, 7500, 10000, 15000, 20000]

Array = npt.NDArray[np.floating[Any]]
IntArray = npt.NDArray[np.int64]
BoolArray = npt.NDArray[np.bool_]
# P(B_f = b | R = r) of each name field, keyed by the fold it is used for
Phis = dict[int, list[Array]]


def _log(verbose: bool, *args: object) -> None:
    if verbose:
        print(*args, flush=True)


def _device() -> torch.device:
    return torch.device("cuda" if torch.cuda.is_available() else "cpu")


# ---------------------------------------------------------------------------
# Algorithm 1, Steps 2-6: out-of-fold list scores
# ---------------------------------------------------------------------------
def name_folds(counts: npt.ArrayLike, J: int = 5,
               seed: int = 2026) -> IntArray:
    """Randomly partition the distinct names into J folds.

    Names are taken in random order and each is put into the fold with the
    fewest people so far, so the folds hold approximately equal numbers of
    people (Algorithm 1, Step 2).

    Args:
        counts: (n_names,) number of people carrying each name.
        J: number of folds.
        seed: random seed for the order of the names.

    Returns:
        (n_names,) integer array with the fold of each name.
    """
    counts = np.asarray(counts, np.float64)
    order = np.random.default_rng(seed).permutation(len(counts))
    load = np.zeros(J)
    fold = np.empty(len(counts), np.int64)
    for i in order:
        f = int(np.argmin(load))
        fold[i] = f
        load[f] += counts[i]
    return fold


def _make_net(input_dim: int, hidden_dims: Sequence[int] = (256, 128),
              dropout: float = 0.3) -> nn.Sequential:
    """Linear -> ReLU -> Dropout per hidden layer, then one output logit."""
    layers: list[nn.Module] = []
    prev = input_dim
    for h in hidden_dims:
        layers += [nn.Linear(prev, h), nn.ReLU(), nn.Dropout(dropout)]
        prev = h
    layers.append(nn.Linear(prev, 1))
    return nn.Sequential(*layers)


class PerListModel:
    """One separate network for each list (Algorithm 1, Step 4).

    The networks share no parameters and each has its own optimizer, so
    fitting them together is the same as fitting them one at a time.

    Args:
        input_dim: dimensionality of the name embedding.
        n_lists: number of lists.
        seed: torch seed for the initial weights.
        device: torch device.
    """

    def __init__(self, input_dim: int, n_lists: int, seed: int,
                 device: torch.device) -> None:
        torch.manual_seed(seed)
        self.device = device
        self.nets = [_make_net(input_dim).to(device) for _ in range(n_lists)]
        self.opts = [
            torch.optim.Adam(net.parameters(), 1e-3, weight_decay=1e-5)
            for net in self.nets
        ]
        self.bce = nn.BCEWithLogitsLoss(reduction="none")

    def _logits(self, X: torch.Tensor, j: torch.Tensor) -> torch.Tensor:
        return torch.cat([net(X[j]) for net in self.nets], 1)

    def train_epoch(self, X: torch.Tensor, Y: torch.Tensor,
                    W: torch.Tensor, idx: IntArray,
                    rng: np.random.RandomState, batch_size: int) -> None:
        """One pass over the names `idx` in random order.

        Each name's loss is weighted by the number of people carrying it.
        """
        for net in self.nets:
            net.train()
        perm = idx[rng.permutation(len(idx))]
        for start in range(0, len(perm), batch_size):
            i = torch.as_tensor(perm[start:start + batch_size],
                                device=self.device)
            xb, wb = X[i], W[i]
            for k, (net, opt) in enumerate(zip(self.nets, self.opts)):
                opt.zero_grad(set_to_none=True)
                loss = (self.bce(net(xb)[:, 0], Y[i, k]) * wb).mean()
                loss.backward()
                opt.step()

    def predict(self, X: torch.Tensor,
                idx: IntArray) -> npt.NDArray[np.float32]:
        """Predicted probability of membership in each list for names `idx`."""
        out = np.zeros((len(idx), len(self.nets)), np.float32)
        for net in self.nets:
            net.eval()
        with torch.no_grad():
            for start in range(0, len(idx), 16384):
                j = torch.as_tensor(idx[start:start + 16384],
                                    device=self.device)
                out[start:start + len(j)] = (
                    torch.sigmoid(self._logits(X, j)).cpu().numpy())
        return out

    def heldout_bce(self, X: torch.Tensor, Y: torch.Tensor,
                    W: torch.Tensor, idx: IntArray) -> float:
        """People-weighted binary cross-entropy on the names `idx`."""
        num = 0.0
        den = 0.0
        for net in self.nets:
            net.eval()
        with torch.no_grad():
            for start in range(0, len(idx), 16384):
                j = torch.as_tensor(idx[start:start + 16384],
                                    device=self.device)
                w = W[j]
                loss = self.bce(self._logits(X, j), Y[j]).mean(1)
                num += float((loss * w).sum())
                den += float(w.sum())
        return num / max(den, 1e-12)


def list_scores_oof(
        emb: npt.ArrayLike, onmat: npt.ArrayLike, counts: npt.ArrayLike,
        J: int = 5, max_epochs: int = 100, patience: int = 5,
        batch_size: int = 512, seed: int = 42, fold_seed: int = 2026,
        verbose: bool = True,
) -> tuple[npt.NDArray[np.float32], dict[int, dict[str, Any]]]:
    """Out-of-fold list scores on the distinct names (Algorithm 1, Steps 2-6).

    For each outer fold k, the inner folds are the other J - 1 name folds,
    each held out in turn. The inner models are trained in lockstep, and the
    number of epochs is the minimum of their mean held-out loss; training
    stops once that mean has not improved for `patience` epochs, or at
    `max_epochs`. One model is then fit on all the training folds for that
    many epochs and scores the names in fold k.

    Args:
        emb: (n_names, D) name embeddings.
        onmat: (n_names, L) 0/1 list membership of each name.
        counts: (n_names,) number of people carrying each name, used to
            weight the loss so the fit is the same as fitting on people.
        J: number of name folds.
        max_epochs, patience: limits for choosing the number of epochs.
        batch_size: minibatch size in distinct names.
        seed: seed for the network weights and minibatch order.
        fold_seed: seed for the name folds.
        verbose: print progress.

    Returns:
        (scores, info): scores is an (n_names, L) array of P(L_l = 1 | name);
        info maps each fold to its selected epochs and mean held-out loss.
    """
    device = _device()
    X = torch.as_tensor(np.ascontiguousarray(emb, np.float32), device=device)
    Y = torch.as_tensor(np.ascontiguousarray(onmat, np.float32),
                        device=device)
    W = torch.as_tensor(np.maximum(np.asarray(counts, np.float32), 1e-4),
                        device=device)
    fold = name_folds(counts, J, fold_seed)
    input_dim, n_lists = X.shape[1], Y.shape[1]
    scores = np.full((len(fold), n_lists), np.nan, np.float32)
    info: dict[int, dict[str, Any]] = {}

    for k in range(J):
        inner: list[dict[str, Any]] = []
        for j in range(J):
            if j == k:
                continue
            s = seed + 10 * k + j
            inner.append(dict(
                model=PerListModel(input_dim, n_lists, s, device),
                rng=np.random.RandomState(s),
                train=np.flatnonzero((fold != k) & (fold != j)),
                valid=np.flatnonzero(fold == j),
            ))
        curve: list[float] = []
        best_loss = np.inf
        best_epochs = 0
        for epoch in range(1, max_epochs + 1):
            losses = []
            for m in inner:
                m["model"].train_epoch(X, Y, W, m["train"], m["rng"],
                                       batch_size)
                losses.append(m["model"].heldout_bce(X, Y, W, m["valid"]))
            curve.append(float(np.mean(losses)))
            if curve[-1] < best_loss:
                best_loss = curve[-1]
                best_epochs = epoch
            elif epoch - best_epochs >= patience:
                break
        del inner

        train = np.flatnonzero(fold != k)
        test = np.flatnonzero(fold == k)
        model = PerListModel(input_dim, n_lists, seed + k, device)
        rng = np.random.RandomState(seed + k)
        for _ in range(best_epochs):
            model.train_epoch(X, Y, W, train, rng, batch_size)
        scores[test] = model.predict(X, test)
        info[k] = dict(selected_epochs=best_epochs, inner_mean_bce=curve)
        _log(verbose, f"  list scores: fold {k + 1} of {J}, "
                      f"{best_epochs} epochs")
        del model
        if device.type == "cuda":
            torch.cuda.empty_cache()

    assert np.isfinite(scores).all()
    return scores, info


# ---------------------------------------------------------------------------
# Algorithm 1, Steps 7-10, and Algorithm 2
# ---------------------------------------------------------------------------
def logodds(F: npt.ArrayLike) -> Array:
    """Log-odds of the list scores, clipped at EPS_Z."""
    F = np.clip(np.asarray(F, float), EPS_Z, 1 - EPS_Z)
    return np.log(F) - np.log(1 - F)


def kmeans_occupied(Z: Array, rows: npt.ArrayLike, K: int,
                    seed: int = 0) -> tuple[IntArray, int]:
    """K-means on the log-odds scores (Algorithm 1, Step 7).

    The K centres are fit on the observations `rows`. Centres that no
    observation in `rows` is nearest to are dropped, and every observation
    is assigned to the nearest remaining centre. At a near-tie in distance,
    the reassignment can move an observation in `rows` to another centre and
    leave a remaining centre with none of them. The function therefore
    repeats the dropping until every centre used has at least one
    observation in `rows`. Without this step, such a cluster has
    P(B = b) = 0 among the fitting observations, and any held-out
    observation in that cluster makes Q(K) infinite.

    Args:
        Z: (N, L) log-odds list scores.
        rows: indices of the observations used to fit the centres.
        K: number of centres.
        seed: random seed.

    Returns:
        (labels, K_used): the (N,) cluster of each observation and the
        number of clusters actually used.
    """
    rows = np.asarray(rows, np.int64)
    uniq, inv = np.unique(Z, axis=0, return_inverse=True)
    inv = np.asarray(inv).ravel()
    km = MiniBatchKMeans(K, random_state=seed, n_init=3,
                         compute_labels=False).fit(Z[rows])
    centers = km.cluster_centers_
    labels_u = pairwise_distances_argmin(uniq, centers)
    while True:
        kept = np.unique(labels_u[inv[rows]])
        if len(kept) == len(centers):
            break
        centers = centers[kept]
        labels_u = pairwise_distances_argmin(uniq, centers)
    return labels_u[inv], len(centers)


def _geo_setup(
        geo: npt.ArrayLike, prior: npt.ArrayLike,
) -> tuple[IntArray, Array, int, Array, Array]:
    """People per geography and the geography-level prior matrix M."""
    geo = np.asarray(geo, np.int64)
    prior = np.asarray(prior, float)
    n_geo = int(geo.max()) + 1
    w = np.bincount(geo, minlength=n_geo).astype(float)
    M = np.column_stack([
        np.bincount(geo, weights=prior[:, r], minlength=n_geo)
        / np.maximum(w, 1)
        for r in range(prior.shape[1])
    ])
    return geo, prior, n_geo, w, M


def wls_pi(C: sparse.csr_matrix, M: Array, w: Array, take: Array) -> Array:
    """P(B = b | R = r) by weighted least squares (Algorithm 1, Step 9).

    Args:
        C: (G, K) sparse counts of people by geography and cluster.
        M: (G, R) prior P(R = r | G = g).
        w: (G,) people per geography (the weights W).
        take: (G,) 1 for the geographies used in the fit, 0 otherwise.

    Returns:
        (R, K) array; each row is normalized to sum to one.
    """
    R = M.shape[1]
    XtWX = M.T @ ((w * take)[:, None] * M)
    XtWY = np.asarray(C.T @ (M * take[:, None])).T
    Pi = np.linalg.solve(XtWX + RIDGE * np.eye(R), XtWY)
    Pi = np.maximum(Pi, FLOOR)
    return Pi / Pi.sum(1, keepdims=True)


def geo_folds(geo: IntArray, J: int) -> IntArray:
    """J folds of geographic units with approximately equal numbers of people.

    The units are taken from largest to smallest and dealt round-robin
    (Algorithm 2, Step 1).
    """
    n_geo = int(np.max(geo)) + 1
    w = np.bincount(geo, minlength=n_geo).astype(float)
    fold = np.zeros(n_geo, int)
    fold[np.argsort(-w, kind="stable")] = np.arange(n_geo) % J
    return fold


def fold_term(Z: Array, geo: IntArray, M: Array, w: Array, n_geo: int,
              K: int, j: int, fold: IntArray,
              seed: int = 0) -> tuple[float, int]:
    """One fold's contribution to Q(K) (Algorithm 2, Steps 4-6)."""
    obs_fold = fold[geo]
    train = np.flatnonzero(obs_fold != j)
    test = np.flatnonzero(obs_fold == j)
    bins, K_used = kmeans_occupied(Z, train, K, seed)
    C = sparse.coo_matrix(
        (np.ones(len(train)), (geo[train], bins[train])),
        shape=(n_geo, K_used)).tocsr()
    Pi = wls_pi(C, M, w, (fold != j).astype(float))
    p_b_given_g = np.einsum("ir,ri->i", M[geo[test]], Pi[:, bins[test]])
    p_b = np.bincount(bins[train], minlength=K_used).astype(float) / len(train)
    log_ratio = np.log(p_b_given_g) - np.log(p_b[bins[test]])
    return float(log_ratio.sum()), K_used


def select_K(F: npt.ArrayLike, geo: npt.ArrayLike, prior: npt.ArrayLike,
             grid: Sequence[int] | None = None, J: int = 5, seed: int = 0,
             extend: bool = True,
             verbose: bool = True) -> tuple[int, list[tuple[int, float]]]:
    """Choose the number of clusters by Q(K) (Algorithm 2).

    The candidate grid is restricted to R <= K < the number of distinct score
    vectors among the training observations. While the maximum of Q(K) sits
    at the top of the grid, the grid is extended upward by a factor of 1.5.

    Args:
        F: (N, L) list scores per person.
        geo: (N,) 0-based geography of each person.
        prior: (N, R) P(R = r | G) per person; rows sum to one.
        grid: candidate values of K (default DEFAULT_GRID).
        J: number of geographic folds.
        seed: random seed for K-means.
        extend: extend the grid upward while the maximum is at its top.
        verbose: print Q(K) as it is computed.

    Returns:
        (K_star, curve): the selected K and a sorted list of (K, Q(K)).
    """
    Z = logodds(F)
    geo, prior, n_geo, w, M = _geo_setup(geo, prior)
    R = M.shape[1]
    fold = geo_folds(geo, J)
    if len(np.unique(fold)) < J:
        raise ValueError(f"Algorithm 2 needs at least J = {J} geographic units")
    limit = min(len(np.unique(Z[fold[geo] != j], axis=0))
                for j in range(J)) - 1
    grid = sorted({int(k) for k in (grid or DEFAULT_GRID)
                   if R <= int(k) <= limit})
    if not grid:
        grid = [max(R, min(limit, 10))]
    curve: list[tuple[int, float]] = []
    done: set[int] = set()

    def run(candidates: Sequence[int]) -> None:
        for K in candidates:
            if K in done:
                continue
            terms = [fold_term(Z, geo, M, w, n_geo, K, j, fold, seed)
                     for j in range(J)]
            curve.append((int(K), sum(t[0] for t in terms) / len(Z)))
            done.add(K)
            _log(verbose, f"  Q(K = {K}) = {curve[-1][1]:.6f}")

    run(grid)
    best = max(curve, key=lambda kq: kq[1])[0]
    while extend and best == max(k for k, _ in curve) and best < limit:
        new = min(int(best * 1.5), limit)
        if new <= best:
            break
        run([new])
        best = max(curve, key=lambda kq: kq[1])[0]
    return best, sorted(curve)


def fit_posterior(
        F: npt.ArrayLike, geo: npt.ArrayLike, prior: npt.ArrayLike, K: int,
        seed: int = 0,
) -> tuple[Array, IntArray, int, Array]:
    """Algorithm 1, Steps 7-10, on all observations.

    Returns:
        (posterior, bins, K_used, Pi): the (N, R) posterior, the cluster of
        each person, the number of clusters used, and the (R, K_used) matrix
        of P(B = b | R = r).
    """
    Z = logodds(F)
    geo, prior, n_geo, w, M = _geo_setup(geo, prior)
    bins, K_used = kmeans_occupied(Z, np.arange(len(Z)), K, seed)
    C = sparse.coo_matrix((np.ones(len(Z)), (geo, bins)),
                          shape=(n_geo, K_used)).tocsr()
    Pi = wls_pi(C, M, w, np.ones(n_geo))
    post = prior * Pi[:, bins].T
    post = post / np.maximum(post.sum(1, keepdims=True), 1e-300)
    return post, bins, K_used, Pi


def lbisg_field(emb_unique: npt.ArrayLike, idx: npt.ArrayLike,
                onmat_unique: npt.ArrayLike, geo: npt.ArrayLike,
                prior: npt.ArrayLike, K: int | None = None,
                grid: Sequence[int] | None = None, n_folds: int = 5,
                geo_folds_J: int = 5, max_epochs: int = 100,
                patience: int = 5, batch_size: int = 512, seed: int = 42,
                verbose: bool = True) -> dict[str, Any]:
    """lBISG for one name field (Algorithm 1, with Algorithm 2 for K).

    Args:
        emb_unique: (n_names, D) embeddings of the distinct names.
        idx: (N,) 0-based index of each person's name into emb_unique.
        onmat_unique: (n_names, L) 0/1 list membership of each name.
        geo: (N,) 0-based geography of each person.
        prior: (N, R) P(R = r | G) per person; rows sum to one.
        K: number of clusters, or None to choose it by Algorithm 2.
        grid: candidate values of K for Algorithm 2.
        n_folds: number of name folds for the list scores.
        geo_folds_J: number of geographic folds for Algorithm 2.
        max_epochs, patience, batch_size, seed: see list_scores_oof().
        verbose: print progress.

    Returns:
        dict with posterior (N, R), K, K_used, Q (list of (K, Q(K)) or None),
        scores (N, L) and the epochs selected in each name fold.
    """
    idx = np.asarray(idx, np.int64)
    geo = np.asarray(geo, np.int64)
    emb_unique = np.asarray(emb_unique)
    counts = np.bincount(idx, minlength=len(emb_unique)).astype(float)
    used = np.flatnonzero(counts > 0)
    remap = np.full(len(emb_unique), -1, np.int64)
    remap[used] = np.arange(len(used))

    _log(verbose, f"  fitting list scores on {len(used)} distinct names")
    scores, info = list_scores_oof(
        emb_unique[used], np.asarray(onmat_unique)[used],
        counts[used], J=int(n_folds), max_epochs=int(max_epochs),
        patience=int(patience), batch_size=int(batch_size), seed=int(seed),
        verbose=verbose)
    F = scores[remap[idx]]

    curve: list[tuple[int, float]] | None = None
    if K is None:
        K, curve = select_K(F, geo, prior, grid=grid, J=int(geo_folds_J),
                            verbose=verbose)
        _log(verbose, f"  selected K = {K}")
    post, _, K_used, _ = fit_posterior(F, geo, prior, int(K))
    return dict(posterior=post, K=int(K), K_used=int(K_used), Q=curve,
                scores=F,
                epochs=[info[k]["selected_epochs"] for k in sorted(info)])


def combine_fields(posteriors: Sequence[npt.ArrayLike],
                   prior: npt.ArrayLike) -> Array:
    """Combine name fields under the lBIFSG assumption (Section 2.5).

    P(R | B_F, B_S, G) is proportional to
    P(R | B_F, G) * P(R | B_S, G) / P(R | G).

    Args:
        posteriors: list of (N, R) posteriors, one per name field.
        prior: (N, R) P(R = r | G) per person.

    Returns:
        (N, R) combined posterior; rows sum to one.
    """
    prior = np.asarray(prior, float)
    out = np.ones_like(prior)
    for p in posteriors:
        out = out * np.asarray(p, float)
    out = out / np.maximum(prior, 1e-300) ** (len(posteriors) - 1)
    out[prior <= 0] = 0
    return out / np.maximum(out.sum(1, keepdims=True), 1e-300)


# ---------------------------------------------------------------------------
# Downstream inference: the cross-fitted estimator of E[Y | R = r, G = g]
# ---------------------------------------------------------------------------
def _fold_stats(bins: IntArray, geo: IntArray, M: Array, w: Array,
                fold_g: IntArray, V: int, K: int) -> tuple[Array, Array]:
    """Terms of the weighted least squares for P(B = b | R = r), by fold.

    The least squares of Algorithm 1, Step 9, on any set of folds is
    solve(sum of XtWX, sum of XtWY) over those folds. The cross-fitted and
    jackknife fits therefore need only these sums.

    Args:
        bins: (N,) cluster of each person.
        geo: (N,) 0-based geography of each person.
        M: (G, R) prior P(R = r | G = g).
        w: (G,) people per geography.
        fold_g: (G,) fold of each geography.
        V: number of folds.
        K: number of clusters.

    Returns:
        (XtWX, XtWY): (V, R, R) and (V, R, K) arrays.
    """
    n_geo, R = M.shape
    C = sparse.coo_matrix((np.ones(len(geo)), (geo, bins)),
                          shape=(n_geo, K)).tocsr()
    XtWX = np.zeros((V, R, R))
    XtWY = np.zeros((V, R, K))
    for v in range(V):
        take = (fold_g == v).astype(float)
        XtWX[v] = M.T @ ((w * take)[:, None] * M)
        XtWY[v] = np.asarray(C.T @ (M * take[:, None])).T
    return XtWX, XtWY


def _phi_from_sums(XtWX: Array, XtWY: Array) -> Array:
    """P(B = b | R = r) from summed least squares terms, as in wls_pi().

    Returns:
        (R, K) array; each row is normalized to sum to one.
    """
    R = XtWX.shape[0]
    Pi = np.linalg.solve(XtWX + RIDGE * np.eye(R), XtWY)
    Pi = np.maximum(Pi, FLOOR)
    return Pi / Pi.sum(1, keepdims=True)


def _posteriors(i: IntArray, geo: IntArray, bins: IntArray, M: Array,
                fold: IntArray, phis: Phis) -> Array:
    """Cross-fitted posteriors of the people `i`.

    The posterior of a person in fold v uses the P(B = b | R = r) fit
    without fold v, pi proportional to M_g * prod_f phi^f_{B_f}.

    Returns:
        (len(i), R) array; rows sum to one.
    """
    post = M[geo[i]].astype(float)
    # compare against paper once proof is extended to include this setting
    for v in np.unique(fold[i]):
        s = fold[i] == v
        for Phi, b in zip(phis[v], bins[:, i[s]]):
            post[s] *= Phi[:, b].T
    return post / np.maximum(post.sum(1, keepdims=True), 1e-300)


def _pool_batches(
        people: IntArray, geo: IntArray, pool: IntArray, chunk: int,
) -> Iterator[tuple[IntArray, IntArray, IntArray]]:
    """Batches of people that each hold whole pooling units.

    Yields:
        (i, starts, ends): the people of the batch, sorted by pooling unit,
        and the start and end of each unit within the batch.
    """
    people = people[np.argsort(pool[geo[people]], kind="stable")]
    p = pool[geo[people]]
    starts = np.flatnonzero(np.r_[True, p[1:] != p[:-1]])
    ends = np.r_[starts[1:], len(p)]
    first = 0
    while first < len(starts):
        last = first
        while (last + 1 < len(starts)
               and ends[last + 1] - starts[first] <= chunk):
            last += 1
        i = people[starts[first]:ends[last]]
        yield (i, starts[first:last + 1] - starts[first],
               ends[first:last + 1] - starts[first])
        first = last + 1


def _pool_grams(people: IntArray, geo: IntArray, bins: IntArray, M: Array,
                Y: Array, pool: IntArray, n_pool: int, fold: IntArray,
                phis: Phis, chunk: int = 50000) -> tuple[Array, Array, Array]:
    """Sums of pi pi', pi Y' and pi over the people of each pooling unit.

    Args:
        people: indices of the people to include.
        geo: (N,) 0-based geography of each person.
        bins: (n_fields, N) cluster of each person in each field.
        M: (G, R) prior P(R = r | G = g).
        Y: (N, P) outcome.
        pool: (G,) pooling unit of each geography.
        n_pool: number of pooling units.
        fold: (N,) fold of each person.
        phis: dict from fold to the list of (R, K_f) matrices of
            P(B_f = b | R = r) used for the people of that fold.
        chunk: people per block.

    Returns:
        (A, B, mass): (n_pool, R, R), (n_pool, R, P) and (n_pool, R) arrays.
    """
    R, P = M.shape[1], Y.shape[1]
    A = np.zeros((n_pool, R, R))
    B = np.zeros((n_pool, R, P))
    mass = np.zeros((n_pool, R))
    for i, starts, ends in _pool_batches(people, geo, pool, chunk):
        post = _posteriors(i, geo, bins, M, fold, phis)
        y = Y[i]
        for s, e in zip(starts, ends):
            c = pool[geo[i[s]]]
            A[c] += post[s:e].T @ post[s:e]
            B[c] += post[s:e].T @ y[s:e]
            mass[c] += post[s:e].sum(0)
    return A, B, mass


def _em_means(post: Array, counts: Array, max_iter: int = 1000,
              tol: float = 1e-7) -> Array:
    """Maximum likelihood group means of a categorical outcome, by EM.

    Each person's outcome category is drawn from the means of the person's
    group, and the group is drawn from the posterior.

    Args:
        post: (c, R) posterior of each cell of people with the same posterior.
        counts: (c, P) people in each outcome category in each cell.
        max_iter, tol: iteration limit and convergence tolerance.

    Returns:
        (R, P) array; each row sums to one.
    """
    R = post.shape[1]
    mu = np.tile(counts.sum(0) / max(counts.sum(), 1), (R, 1))
    for _ in range(max_iter):
        expected = post @ mu
        ratio = counts / np.maximum(expected, 1e-300)
        num = mu * (post.T @ ratio)
        mu_new = num / np.maximum(num.sum(1, keepdims=True), 1e-300)
        if np.max(np.abs(mu_new - mu)) < tol:
            return mu_new
        mu = mu_new
    return mu


def _em_pools(people: IntArray, geo: IntArray, bins: IntArray, M: Array,
              Y: Array, pool: IntArray, n_pool: int, fold: IntArray,
              phis: Phis, solve: BoolArray, chunk: int = 50000) -> Array:
    """EM group means in each pooling unit, over cells of identical people.

    People with the same geography and clusters have the same posterior, so
    the EM runs on those cells with the outcome counted by category.

    Args:
        solve: (n_pool, R) True for the groups to estimate in each unit.
        Other arguments as in _pool_grams().

    Returns:
        (n_pool, R, P) array, nan for the groups not estimated.
    """
    R, P = M.shape[1], Y.shape[1]
    mu = np.full((n_pool, R, P), np.nan)
    for i, starts, ends in _pool_batches(people, geo, pool, chunk):
        post = _posteriors(i, geo, bins, M, fold, phis)
        cat = Y[i].argmax(1)
        for s, e in zip(starts, ends):
            c = pool[geo[i[s]]]
            k = np.flatnonzero(solve[c])
            if len(k) == 0:
                continue
            key = np.column_stack([geo[i[s:e]], *bins[:, i[s:e]]])
            uniq, first, cell = np.unique(key, axis=0, return_index=True,
                                          return_inverse=True)
            cell = np.asarray(cell).ravel()
            counts = np.zeros((len(uniq), P))
            np.add.at(counts, (cell, cat[s:e]), 1.0)
            p = post[s:e][first][:, k]
            p = p / np.maximum(p.sum(1, keepdims=True), 1e-300)
            mu[c, k] = _em_means(p, counts)
    return mu


def _simplex_rows(X: Array) -> Array:
    """Euclidean projection of each row of X onto the probability simplex."""
    U = -np.sort(-X, axis=1)
    css = np.cumsum(U, 1) - 1
    k = np.arange(1, X.shape[1] + 1)
    rho = (U - css / k > 0).sum(1)
    tau = css[np.arange(len(X)), rho - 1] / rho
    return np.maximum(X - tau[:, None], 0)


def _project(X: Array, constraint: str | None) -> Array:
    """Projection onto the constraint on the group means."""
    if constraint == "simplex":
        return _simplex_rows(X)
    if constraint == "box":
        return np.clip(X, 0.0, 1.0)
    return X


def _solve_constrained(A: Array, B: Array, constraint: str | None,
                       max_iter: int = 5000, tol: float = 1e-8) -> Array:
    """Minimize tr(mu' A mu) - 2 tr(B' mu) subject to the constraint.

    The unconstrained minimizer is the least squares solution. With a
    constraint, the minimizer is found by accelerated projected gradient
    descent (FISTA), started from the projected least squares solution.

    Args:
        A: (R, R) positive semidefinite matrix, sum of pi pi'.
        B: (R, P) matrix, sum of pi Y'.
        constraint: "simplex" (each group's means over the P outcomes lie on
            the simplex), "box" (each mean lies in [0, 1]) or None.

    Returns:
        (R, P) array of group means.
    """
    x = np.linalg.lstsq(A, B, rcond=None)[0]
    if constraint is None:
        return x
    x = _project(x, constraint)
    L = 2 * float(np.max(np.linalg.eigvalsh(A)))
    if L <= 0:
        return x
    z, t = x.copy(), 1.0
    for _ in range(max_iter):
        x_new = _project(z - 2 * (A @ z - B) / L, constraint)
        if np.max(np.abs(x_new - x)) < tol:
            return x_new
        t_new = (1 + np.sqrt(1 + 4 * t * t)) / 2
        z = x_new + (t - 1) / t_new * (x_new - x)
        x, t = x_new, t_new
    return x


def _solve_pools(A: Array, B: Array, solve: BoolArray,
                 constraint: str | None) -> Array:
    """Least squares group means in each pooling unit.

    Returns:
        (n_pool, R, P) array, nan for the groups not estimated.
    """
    n_pool, R, P = B.shape
    mu = np.full((n_pool, R, P), np.nan)
    for c in range(n_pool):
        k = np.flatnonzero(solve[c])
        if len(k) == 0:
            continue
        mu[c, k] = _solve_constrained(A[c][np.ix_(k, k)], B[c, k],
                                      constraint)
    return mu


def _aggregate(mu: Array, weight: Array, by_pool: IntArray,
               n_agg: int) -> tuple[Array, Array]:
    """Mean outcome among the members of each group, by aggregate.

    Returns:
        (theta, N): the (H, R, P) estimates and the (H, R) group members used
        as weights.
    """
    N = np.zeros((n_agg, weight.shape[1]))
    np.add.at(N, by_pool, weight)
    total = np.zeros((n_agg,) + mu.shape[1:])
    np.add.at(total, by_pool, weight[:, :, None] * np.nan_to_num(mu))
    with np.errstate(divide="ignore", invalid="ignore"):
        theta = total / N[:, :, None]
    return theta, N


def downstream_crossfit(
        F: npt.ArrayLike | Sequence[npt.ArrayLike], geo: npt.ArrayLike,
        prior: npt.ArrayLike, Y: npt.ArrayLike,
        fold: npt.ArrayLike | None = None, pool: npt.ArrayLike | None = None,
        by: npt.ArrayLike | None = None,
        K: int | Sequence[int] | None = None, V: int = 5,
        bins: npt.ArrayLike | None = None,
        fold_stats: Sequence[tuple[Array, Array]] | None = None,
        constraint: str | None = "simplex", method: str = "ls",
        weights: str | None = None, jackknife: bool = True,
        jack_groups: npt.ArrayLike | None = None, seed: int = 0,
        chunk: int = 50000, verbose: bool = True,
) -> dict[str, Any]:
    """Cross-fitted estimator of the mean outcome of each group.

    Under the lBISG assumptions and the BIRDiE assumption that the outcome is
    independent of the name given the group and geography,
    E[Y | B, G] = sum_r mu_r P(R = r | B, G). Within each pooling unit c (for
    example the county), the group means mu_c are estimated from the outcome
    and the posterior probabilities, either by least squares or by maximum
    likelihood. The least squares solves the moment E[Y 1{B = b} | G = g] =
    sum_r mu_r P(B = b | R = r) P(R = r | G = g), with each cluster's moment
    weighted by 1 / P(B = b | G = g). The posterior of each person uses the
    P(B = b | R = r) fit on the other folds of geographies (for example the
    other states), with the clusters fit on all observations.

    With many groups, groups with similar names and geography make the least
    squares close to singular, and the means are therefore constrained to
    the simplex (for an outcome with mutually exclusive categories) or to
    [0, 1]. The maximum likelihood estimates, for a categorical outcome,
    satisfy the simplex constraint by construction. Standard errors come
    from a delete-one-group jackknife over groups of geographies, which
    refits P(B = b | R = r) without the deleted group.

    With several name fields, the clusters of the fields are taken as
    independent given the group and geography (the lBIFSG assumption), and
    the posterior is proportional to P(R = r | G) prod_f P(B_f = b_f | R = r).

    Args:
        F: (N, L) list scores per person (from lbisg_field()), or a list of
            such matrices, one per name field.
        geo: (N,) 0-based geography of each person, at the prior's level.
        prior: (N, R) P(R = r | G) per person; rows sum to one.
        Y: (N,) or (N, P) outcome; the columns are estimated jointly. With
            method "em", Y holds one indicator column per category.
        fold: (n_geo,) 0-based fold of each geography, or None for V folds
            of geographies with approximately equal numbers of people.
        pool: (n_geo,) 0-based pooling unit of each geography, nested within
            `by`, or None for one pooling unit per aggregate.
        by: (n_geo,) 0-based aggregate of each geography, or None for a
            single aggregate over all geographies.
        K: number of clusters, or a list with one per name field; required
            unless `bins` is given.
        V: number of folds when `fold` is None.
        bins: optional (n_fields, N) clusters of each person in each field,
            to reuse an existing clustering.
        fold_stats: optional list, one per field, of the (XtWX, XtWY) terms of
            _fold_stats() over a larger set of folds (for example every
            state), so that P(B = b | R = r) is fit on data outside Y.
            `fold` then indexes these folds, and `bins` is required.
        constraint: "simplex", "box" or None, for method "ls".
        method: "ls" (constrained least squares) or "em" (maximum likelihood
            for a categorical outcome).
        weights: "posterior" (the sum of the posteriors of the unit's people)
            or "prior" (the sum of their priors), the group members of each
            pooling unit used to combine units into aggregates, or None for
            "prior" with method "ls" and "posterior" with method "em".
        jackknife: compute the delete-one-group jackknife covariance.
        jack_groups: (n_geo,) 0-based jackknife group of each geography, or
            None for the folds. More groups than folds (for example groups of
            counties within each state) give a more stable variance, and an
            aggregate needs geographies in several groups.
        seed: random seed for K-means.
        chunk: people per block.
        verbose: print progress.

    Returns:
        dict with
          mu (n_pool, R, P): group means in each pooling unit, nan for groups
            with no prior mass there;
          theta (H, R, P): the mean of Y among the members of r in each
            aggregate, combining pooling units by `weights`;
          vcov (H, R * P, R * P): jackknife covariance of theta_h, indexed
            (r, p) row-major, or None without jackknife;
          N (H, R): group members in each aggregate, by `weights`;
          K (n_fields,): clusters used in each name field.
    """
    fields: list[Any] = list(F) if isinstance(F, (list, tuple)) else [F]
    n_fields = len(fields)
    geo, prior_a, n_geo, w, M = _geo_setup(geo, prior)
    Y = np.asarray(Y, float)
    if Y.ndim == 1:
        Y = Y[:, None]
    n, R = prior_a.shape
    if method not in ("ls", "em"):
        raise ValueError("method must be 'ls' or 'em'")
    if weights is None:
        weights = "prior" if method == "ls" else "posterior"
    if weights not in ("posterior", "prior"):
        raise ValueError("weights must be 'posterior' or 'prior'")
    if method == "em" and np.any(np.abs(Y.sum(1) - 1) > 1e-8):
        raise ValueError("method 'em' requires one indicator per category")
    if fold_stats is not None and (fold is None or bins is None):
        raise ValueError("fold_stats requires fold and bins")
    fold_g = geo_folds(geo, V) if fold is None else np.asarray(fold, np.int64)
    fold_p = fold_g[geo]
    if len(np.unique(fold_p)) < 2 and fold_stats is None:
        raise ValueError("cross-fitting needs at least two folds")
    by = (np.zeros(n_geo, np.int64) if by is None
          else np.asarray(by, np.int64))
    n_agg = int(by.max()) + 1
    pool = by.copy() if pool is None else np.asarray(pool, np.int64)
    n_pool = int(pool.max()) + 1
    by_pool = np.zeros(n_pool, np.int64)
    by_pool[pool] = by
    if np.any(by_pool[pool] != by):
        raise ValueError("each pooling unit must lie within one aggregate")

    # Algorithm 1, Step 7, on all observations
    if bins is None:
        K_fields: list[Any] = (list(K) if isinstance(K, (list, tuple))
                               else [K] * n_fields)
        if len(K_fields) != n_fields or any(k is None for k in K_fields):
            raise ValueError("K must have one value per name field")
        bins_a = np.empty((n_fields, n), np.int64)
        K_used = np.zeros(n_fields, np.int64)
        for f in range(n_fields):
            bins_a[f], K_used[f] = kmeans_occupied(
                logodds(fields[f]), np.arange(n), int(K_fields[f]), seed)
            _log(verbose, f"  field {f + 1}: K = {K_fields[f]} "
                          f"({K_used[f]} used)")
    else:
        bins_a = np.asarray(bins, np.int64).reshape(n_fields, n)
        K_used = bins_a.max(1) + 1

    # Step 9 terms for each pair of fold and jackknife group. The terms of
    # any training set are sums of these, so the cross-fitted and jackknife
    # fits need no further pass over the data.
    jack_g = fold_g if jack_groups is None else np.asarray(jack_groups,
                                                           np.int64)
    pairs, pair_g = np.unique(np.column_stack([fold_g, jack_g]), axis=0,
                              return_inverse=True)
    pair_g = np.asarray(pair_g).ravel()
    pair_stats = [_fold_stats(bins_a[f], geo, M, w, pair_g, len(pairs),
                              int(K_used[f])) for f in range(n_fields)]
    if fold_stats is None:
        n_fold = int(np.max(fold_g)) + 1
        fold_tot: list[tuple[Array, Array]] = []
        for XtWX, XtWY in pair_stats:
            tX = np.zeros((n_fold,) + XtWX.shape[1:])
            tY = np.zeros((n_fold,) + XtWY.shape[1:])
            np.add.at(tX, pairs[:, 0], XtWX)
            np.add.at(tY, pairs[:, 0], XtWY)
            fold_tot.append((tX, tY))
    else:
        fold_tot = list(fold_stats)
        n_fold = fold_tot[0][0].shape[0]
    folds = np.arange(n_fold)

    def phis_without(v: int, group: int | None = None) -> list[Array]:
        """P(B = b | R = r) fit without fold v and jackknife group `group`."""
        train = folds != v
        out = []
        for (tX, tY), (pX, pY) in zip(fold_tot, pair_stats):
            X, Yt = tX[train].sum(0), tY[train].sum(0)
            if group is not None:
                drop = (pairs[:, 1] == group) & (pairs[:, 0] != v)
                X, Yt = X - pX[drop].sum(0), Yt - pY[drop].sum(0)
            out.append(_phi_from_sums(X, Yt))
        return out

    def fit(people: IntArray,
            group: int | None = None) -> tuple[Array, Array, Array]:
        """Group means and aggregates from the people included."""
        phis = {int(v): phis_without(int(v), group)
                for v in np.unique(fold_p[people])}
        in_people = np.zeros(n_geo, bool)
        in_people[geo[people]] = True
        prior_mass = np.zeros((n_pool, R))
        np.add.at(prior_mass, pool[in_people],
                  (w[:, None] * M)[in_people])
        solve = prior_mass > 0
        A, B, post_mass = _pool_grams(people, geo, bins_a, M, Y, pool,
                                      n_pool, fold_p, phis, chunk)
        if method == "ls":
            mu = _solve_pools(A, B, solve, constraint)
        else:
            mu = _em_pools(people, geo, bins_a, M, Y, pool, n_pool, fold_p,
                           phis, solve, chunk)
        weight = post_mass if weights == "posterior" else prior_mass
        theta, N = _aggregate(mu, weight * solve, by_pool, n_agg)
        return mu, theta, N

    mu, theta, N = fit(np.arange(n))

    vcov = None
    if jackknife:
        # delete one group, refitting P(B = b | R = r) without it
        rep_list = []
        jack_p = jack_g[geo]
        for j in np.unique(jack_p):
            _, theta_j, _ = fit(np.flatnonzero(jack_p != j), int(j))
            rep_list.append(theta_j.reshape(n_agg, -1))
            _log(verbose, f"  jackknife: group {j} deleted")
        # A group with no members once a fold is deleted has no estimate in
        # that replicate, and the replicate adds nothing to its variance.
        reps = np.stack(rep_list)
        vcov = np.full((n_agg, R * Y.shape[1], R * Y.shape[1]), np.nan)
        for h in range(n_agg):
            m = np.isfinite(reps[:, h]).sum(0)
            with np.errstate(invalid="ignore"):
                d = np.nan_to_num(reps[:, h] - np.nanmean(reps[:, h], 0))
            m_pair = np.minimum.outer(m, m)
            with np.errstate(divide="ignore", invalid="ignore"):
                vcov[h] = (np.where(m_pair > 1, (m_pair - 1) / m_pair, np.nan)
                           * (d.T @ d))
    return dict(mu=mu, theta=theta, vcov=vcov, N=N, K=K_used)


# ---------------------------------------------------------------------------
# Section 3: recovering geographic prevalence
# ---------------------------------------------------------------------------
def _list_rates(onmat: Array, geo: IntArray,
                n_geo: int) -> tuple[Array, Array]:
    """Share of people in each geography with a name on each list."""
    w = np.bincount(geo, minlength=n_geo).astype(float)
    rho = np.column_stack([
        np.bincount(geo, weights=onmat[:, r], minlength=n_geo)
        for r in range(onmat.shape[1])
    ])
    return rho / np.maximum(w, 1)[:, None], w


def _recover_block(rho: Array, target: Array, weights: Array,
                   lam: float = 0.0) -> tuple[Array, Array]:
    """Recover the group shares within one block of groups.

    With lam = 0 (exclusive lists), the inverse coverages u = 1 / pi solve a
    non-negative least squares problem weighted by `weights` (Appendix C.3).
    With lam > 0 (homogeneous non-separation, Appendix C.1-C.2),
    a = (B'WB)^{-1} B'W target, pi_r = lam + (1 - lam * sum(a)) / a_r and
    Theta = B Pi^{-1}, with negative entries set to zero. The shares are then
    normalized to `target` in each geography; geographies whose estimates
    are all zero get the overall mix.

    Args:
        rho: (G, Rc) list rates of the groups in the block.
        target: (G,) total share of the block in each geography.
        weights: (G,) weight of each geography.
        lam: assumed cross-group list rate.

    Returns:
        (Theta, coverage): the (G, Rc) recovered shares and the estimated
        coverage P(L_r = 1 | R = r) of each list.
    """
    sw = np.sqrt(np.maximum(weights, 0))
    if lam == 0:
        u, _ = nnls(rho * sw[:, None], target * sw)
        theta = rho * u[None, :]
        coverage = np.where(u > 0, 1 / np.maximum(u, 1e-300), np.nan)
    else:
        a = np.linalg.lstsq(rho * sw[:, None], target * sw, rcond=None)[0]
        pi = lam + (1 - lam * a.sum()) / a
        Pi = np.diag(pi - lam) + lam * np.ones((len(pi), len(pi)))
        theta = np.maximum(rho @ np.linalg.inv(Pi), 0)
        coverage = pi
    total = theta.sum(1)
    positive = total > 1e-12
    if positive.any():
        mix = theta[positive].sum(0) / max(theta[positive].sum(), 1e-12)
    else:
        mix = np.full(theta.shape[1], 1 / theta.shape[1])
    theta = np.where(positive[:, None],
                     theta / np.maximum(total, 1e-12)[:, None],
                     mix[None, :])
    return theta * target[:, None], coverage


def recover_prevalence(
        onmat: npt.ArrayLike, geo: npt.ArrayLike,
        coarse_prior: npt.ArrayLike | None = None,
        coarse_of: npt.ArrayLike | None = None, lam: float = 0.0,
) -> tuple[Array, Array]:
    """Recover the geographic prevalence of each group from the list rates.

    Without a coarse prior (Section 3.2), the lists must be exhaustive and
    exclusive; each geography's shares sum to one and geographies are
    weighted by their number of people. With a coarse prior (Section 3.3),
    the subgroup shares are recovered within each coarse group with more
    than one subgroup, weighting each geography by its expected number of
    coarse group members; a coarse group with one subgroup keeps its coarse
    share.

    Args:
        onmat: (N, L) 0/1 list membership per person.
        geo: (N,) 0-based geography of each person.
        coarse_prior: optional (G, C) coarse shares P(C = c | G) per
            geography.
        coarse_of: (L,) 0-based coarse group of each list's group.
        lam: assumed cross-group list rate (0 for exclusive lists).

    Returns:
        (Theta, coverage): the (G, L) recovered shares per geography and the
        estimated coverage of each list.
    """
    onmat = np.asarray(onmat, float)
    geo = np.asarray(geo, np.int64)
    n_geo = int(geo.max()) + 1
    rho, n = _list_rates(onmat, geo, n_geo)
    if coarse_prior is None:
        return _recover_block(rho, np.ones(n_geo), n, lam)

    Mc = np.asarray(coarse_prior, float)
    if coarse_of is None:
        raise ValueError("coarse_prior requires coarse_of")
    coarse_of = np.asarray(coarse_of, np.int64)
    theta = np.zeros((n_geo, onmat.shape[1]))
    coverage = np.full(onmat.shape[1], np.nan)
    for c in np.unique(coarse_of):
        cols = np.flatnonzero(coarse_of == c)
        if len(cols) == 1:
            # Nothing to recover: the group's share is the coarse share, and
            # its coverage is not estimated (it stays NaN).
            theta[:, cols[0]] = Mc[:, c]
            continue
        theta[:, cols], coverage[cols] = _recover_block(
            rho[:, cols], Mc[:, c], n * Mc[:, c], lam)
    return theta, coverage


# ---------------------------------------------------------------------------
# Section 4.3: list quality when geographic prevalence is available
# ---------------------------------------------------------------------------
def list_quality(onmat: npt.ArrayLike, geo: npt.ArrayLike,
                 prior: npt.ArrayLike,
                 list_group: npt.ArrayLike | None = None,
                 min_geo_n: int = 1) -> dict[str, Any]:
    """Label-free list quality (Section 4.3).

    Pi[r, l] = P(L_l = 1 | R = r) solves
    P(L_l = 1 | G) = sum_r Pi[r, l] P(R = r | G) by least squares weighted by
    the people in each geography, clipped to [0, 1].

    Args:
        onmat: (N, L) 0/1 list membership per person.
        geo: (N,) 0-based geography of each person.
        prior: (N, R) P(R = r | G) per person; rows sum to one.
        list_group: (L,) 0-based group each list targets (default: list l
            targets group l).
        min_geo_n: geographies with fewer people are left out of the solve.

    Returns:
        dict with Pi (R, L), the diagonal dominance D, and the coverage and
        precision of each list, plus the group shares q.
    """
    onmat = np.asarray(onmat, float)
    geo, prior, n_geo, w, M = _geo_setup(geo, prior)
    rho, _ = _list_rates(onmat, geo, n_geo)
    keep = w >= max(min_geo_n, 1)
    sw = np.sqrt(w[keep])
    Pi = np.linalg.lstsq(M[keep] * sw[:, None], rho[keep] * sw[:, None],
                         rcond=None)[0]
    Pi = np.clip(Pi, 0, 1)

    n_lists = onmat.shape[1]
    if list_group is None:
        list_group = np.arange(n_lists)
    list_group = np.asarray(list_group, np.int64)
    q = (M * w[:, None]).sum(0) / w.sum()

    # D(Pi): each listed group's own-list rate minus its mean rate on the
    # other lists, averaged over groups. In the binary case it is pi_1 - pi_0.
    block = Pi[list_group, :]
    if n_lists == 1 and M.shape[1] == 2:
        D = float(Pi[list_group[0], 0] - Pi[1 - list_group[0], 0])
    elif n_lists == 1:
        D = float(block[0, 0])
    else:
        D = float(np.mean([block[i, i] - np.delete(block[i], i).mean()
                           for i in range(n_lists)]))

    coverage = np.array([Pi[list_group[l], l] for l in range(n_lists)])
    precision = np.array([
        q[list_group[l]] * Pi[list_group[l], l]
        / max((q * Pi[:, l]).sum(), 1e-300)
        for l in range(n_lists)
    ])
    return dict(Pi=Pi, D=D, coverage=coverage, precision=precision, q=q)
