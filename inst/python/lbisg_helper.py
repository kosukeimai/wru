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
    return _stats_from_counts(_counts(bins, geo, M.shape[0], K), M, w,
                              fold_g, V)


def _counts(bins: IntArray, geo: IntArray, n_geo: int,
            K: int) -> sparse.csr_matrix:
    """(G, K) sparse counts of people by geography and cluster."""
    return sparse.coo_matrix((np.ones(len(geo)), (geo, bins)),
                             shape=(n_geo, K)).tocsr()


def _stats_from_counts(C: sparse.csr_matrix, M: Array, w: Array,
                       fold_g: IntArray, V: int) -> tuple[Array, Array]:
    """_fold_stats() from the counts C and the people per geography w."""
    R, K = M.shape[1], C.shape[1]
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


def _pool_grams(
        people: IntArray, geo: IntArray, bins: IntArray, M: Array, Y: Array,
        pool: IntArray, n_pool: int, fold: IntArray, phis: Phis,
        chunk: int = 50000, fulls: Phis | None = None,
) -> tuple[Array, Array, Array, Array, Array]:
    """Sums of pi pi', pi Y' and pi over the people of each pooling unit,
    and the terms of the orthogonal correction.

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
        fulls: optional dict like `phis` holding, for each fold, the fit of
            P(B_f = b | R = r) on all folds. With dpi the posterior under
            `fulls` minus the posterior under `phis`, the correction terms are
            C = sum (dpi pi' + pi dpi') and c0 = sum dpi Y' (zero without
            `fulls`). The difference equals the derivative of the posterior
            in the direction fulls - phis to first order, and unlike the
            derivative it stays bounded when a cross-fitted P(B_f = b | R = r)
            is at its floor.

    Returns:
        (A, B, mass, C, c0): (n_pool, R, R), (n_pool, R, P), (n_pool, R),
        (n_pool, R, R) and (n_pool, R, P) arrays.
    """
    R, P = M.shape[1], Y.shape[1]
    A = np.zeros((n_pool, R, R))
    B = np.zeros((n_pool, R, P))
    mass = np.zeros((n_pool, R))
    C = np.zeros((n_pool, R, R))
    c0 = np.zeros((n_pool, R, P))
    for i, starts, ends in _pool_batches(people, geo, pool, chunk):
        post = _posteriors(i, geo, bins, M, fold, phis)
        dpi = (None if fulls is None else
               _posteriors(i, geo, bins, M, fold, fulls) - post)
        y = Y[i]
        for s, e in zip(starts, ends):
            c = pool[geo[i[s]]]
            A[c] += post[s:e].T @ post[s:e]
            B[c] += post[s:e].T @ y[s:e]
            mass[c] += post[s:e].sum(0)
            if dpi is not None:
                cross = dpi[s:e].T @ post[s:e]
                C[c] += cross + cross.T
                c0[c] += dpi[s:e].T @ y[s:e]
    return A, B, mass, C, c0


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


def _solve_pools(A: Array, B: Array, solve: BoolArray) -> Array:
    """Least squares group means in each pooling unit, without pooling.

    Args:
        A, B: (n_pool, R, R) and (n_pool, R, P) terms of each unit's
            equation A mu = B.
        solve: (n_pool, R) True for the groups to estimate in each unit.

    Returns:
        (n_pool, R, P) array, nan for the groups not estimated.
    """
    n_pool, R, P = B.shape
    mu = np.full((n_pool, R, P), np.nan)
    for c in range(n_pool):
        k = np.flatnonzero(solve[c])
        if len(k):
            mu[c, k] = np.linalg.lstsq(A[c][np.ix_(k, k)], B[c, k],
                                       rcond=None)[0]
    return mu


def _eb_pools(A: Array, B: Array, yy: Array, solve: BoolArray,
              by_pool: IntArray, n_agg: int, constraint: str | None,
              init: dict[str, Any] | None = None, max_iter: int = 500,
              tol: float = 1e-5) -> tuple[Array, dict[str, Any]]:
    """Empirical-Bayes group means in each pooling unit, pooled toward the
    group means of the unit's aggregate.

    Model: within pooling unit c of aggregate s, y_j = mu_c' pi_j + e_j with
    Var(e_j) = sigma^2 for each outcome column, and mu_{c, r} = m_{s, r} +
    u_{c, r} with u_{c, r} ~ N(0, tau_r^2); tau and sigma are shared over the
    outcome columns. m, tau and sigma are fit by EM on the sums of each unit.
    The estimate of mu_c is the posterior mean, the ridge
    (A_c + Lambda) mu_c = B_c + Lambda m_s with Lambda = diag(sigma^2 /
    tau_r^2), which pulls each group's means toward the aggregate in
    proportion to how little the unit's own data identify them.

    With a constraint, tau_r^2 is at most the mean over the outcome columns
    and the aggregates with the group of m (1 - m), m projected onto the
    constraint: a quantity in [0, 1] with mean m has variance at most
    m (1 - m). Without the bound, groups that the units barely identify can
    get an implausibly large tau and almost no pooling.

    Args:
        A, B: (n_pool, R, R) and (n_pool, R, P) sums of pi pi' and pi Y'.
        yy: (n_pool, P) sums of Y^2.
        solve: (n_pool, R) True for the groups to estimate in each unit.
        by_pool: (n_pool,) aggregate of each unit.
        n_agg: number of aggregates.
        constraint: "simplex", "box" or None (no bound on tau).
        init: optional fit (m, tau2, sigma2) to start from.
        max_iter, tol: EM iteration limit and tolerance (on log tau,
            log sigma^2 and m).

    Returns:
        (mu, fit): the (n_pool, R, P) posterior means, nan where `solve` is
        False, and a dict with m (n_agg, R, P), tau2 (R,), sigma2 and iters.
    """
    n_pool, R, P = B.shape
    KS = [np.flatnonzero(solve[c]) for c in range(n_pool)]
    present = np.stack([solve[by_pool == s].any(0) for s in range(n_agg)])
    cnt = solve.sum(0)
    n_obs = max(float(yy.sum()), 1e-300)
    if init is None:
        m = np.zeros((n_agg, R, P))
        for s in range(n_agg):
            cs = by_pool == s
            k = np.flatnonzero(present[s])
            if len(k):
                m[s, k] = _project(np.linalg.lstsq(
                    A[cs].sum(0)[np.ix_(k, k)], B[cs].sum(0)[k],
                    rcond=None)[0], constraint)
        tau2, sig2 = np.full(R, 0.01), 0.2
    else:
        m = init["m"].copy()
        tau2, sig2 = init["tau2"].copy(), float(init["sigma2"])
    it = 0
    for it in range(max_iter):
        U = np.zeros((n_pool, R, P))
        Vd = np.zeros((n_pool, R))
        rss = 0.0
        for c, k in enumerate(KS):
            if not len(k):
                continue
            Ak = A[c][np.ix_(k, k)]
            mk = m[by_pool[c], k]
            Qi = np.linalg.inv(Ak / sig2 + np.diag(1 / tau2[k]))
            u = Qi @ (B[c, k] - Ak @ mk) / sig2
            U[c, k] = u
            Vd[c, k] = np.diag(Qi)
            beta = mk + u
            rss += (yy[c].sum() - 2 * np.sum(beta * B[c, k])
                    + np.sum(beta * (Ak @ beta)) + P * np.sum(Ak * Qi))
        m_new = np.zeros_like(m)
        for s in range(n_agg):
            cs = np.flatnonzero(by_pool == s)
            k = np.flatnonzero(present[s])
            if not len(k):
                continue
            rhs = sum(B[c][k] - A[c][np.ix_(k, k)] @ U[c][k] for c in cs)
            m_new[s, k] = np.linalg.lstsq(A[cs].sum(0)[np.ix_(k, k)], rhs,
                                          rcond=None)[0]
        tau_new = np.maximum(((U ** 2).mean(2) + Vd).sum(0)
                             / np.maximum(cnt, 1), 1e-8)
        if constraint is not None:
            mp = np.stack([_project(m_new[s], constraint)
                           for s in range(n_agg)])
            cap = ((present[:, :, None] * mp * (1 - mp)).sum((0, 2))
                   / (P * np.maximum(present.sum(0), 1)))
            tau_new = np.clip(tau_new, 1e-8, np.maximum(cap, 1e-8))
        sig_new = max(rss / (n_obs * P) if yy.sum() > 0 else sig2, 1e-12)
        delta = max(np.max(np.abs(np.log(tau_new) - np.log(tau2))),
                    abs(np.log(sig_new / sig2)), np.max(np.abs(m_new - m)))
        m, tau2, sig2 = m_new, tau_new, sig_new
        if delta < tol:
            break
    mu = np.full((n_pool, R, P), np.nan)
    for c, k in enumerate(KS):
        if len(k):
            lam = sig2 / tau2[k]
            mu[c, k] = np.linalg.solve(A[c][np.ix_(k, k)] + np.diag(lam),
                                       B[c, k] + lam[:, None] * m[by_pool[c], k])
    return mu, dict(m=m, tau2=tau2, sigma2=sig2, iters=it + 1)


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


def _project_aggregates(theta: Array, constraint: str | None) -> Array:
    """Each aggregate's means of each group projected onto the constraint
    (rows with no estimate stay nan)."""
    if constraint is None:
        return theta
    H, R, P = theta.shape
    flat = theta.reshape(H * R, P)
    out = flat.copy()
    ok = np.isfinite(flat).all(1)
    out[ok] = _project(flat[ok], constraint)
    return out.reshape(H, R, P)


def _if_vcov(
        geo: IntArray, bins: IntArray, M: Array, Y: Array, pool: IntArray,
        by_pool: IntArray, n_agg: int, fold_p: IntArray,
        clus: dict[int, tuple[IntArray, IntArray]], phis: Phis, H: Array,
        mu: Array, solve: BoolArray, weight: Array, N: Array, G: Array,
        chunk: int, verbose: bool) -> Array:
    """Covariance of the aggregates from the influence function of the
    orthogonal least squares (see downstream_crossfit).

    The influence function of aggregate h at person j is
    sum_c w_c H_c^{-1} [pi_j (Y_j - mu_c' pi_j)' 1{j in c}
                        + sum_{f, b, s} T^f_c[b, s] zeta^f_{b, s}(O_j)],
    with w_c the unit's share of each group's members in the aggregate,
    H_c = A_c + C_c, T^f_c[b, s] the derivative of unit c's score in
    P(B_f = b | R = s), and zeta^f(O_j) the influence function of the
    least squares for P(B_f = b | R = r), whose (s, b) entry is
    (G^{-1} M_g)_s (1{B_jf = b} - (Phi' M_g)_b). Its last term is the same
    for every person of a geography. The covariance is the sum of the
    squared influence functions centred within each geography (the
    geographies held fixed), and that term therefore drops out.

    For each aggregate, one pass over its people builds the T terms
    (K_f R P R values per name field and clustering), and one pass over all
    people evaluates them.

    Args:
        H: (n_pool, R, R) matrix of each unit's corrected equation.
        mu: (n_pool, R, P) group means of each unit.
        solve: (n_pool, R) groups estimated in each unit.
        weight: (n_pool, R) group members of each unit used for the
            aggregates.
        N: (H, R) group members of each aggregate.
        G: (R, R) the matrix M'WM of the least squares for
            P(B = b | R = r) on all folds.
        Other arguments as in _pool_grams().

    Returns:
        (H, R * P, R * P) array.
    """
    n, P = Y.shape
    R = M.shape[1]
    n_pool = H.shape[0]
    n_fields = bins.shape[0]
    U = np.linalg.solve(G + RIDGE * np.eye(R), M.T).T
    mu0 = np.nan_to_num(mu)
    agg_p = by_pool[pool[geo]]
    n_g = np.bincount(geo, minlength=M.shape[0]).astype(float)
    Gc = np.zeros((n_pool, R, R))
    for c in range(n_pool):
        k = np.flatnonzero(solve[c])
        if len(k) == 0:
            continue
        Nk = N[by_pool[c], k]
        om = np.where(Nk > 0, weight[c, k] / np.where(Nk > 0, Nk, 1), 0.0)
        Gc[c][np.ix_(k, k)] = om[:, None] * np.linalg.pinv(H[c][np.ix_(k, k)])
    # one set of T terms per distinct clustering
    keys: dict[int, int] = {}
    for v in sorted(clus):
        keys.setdefault(id(clus[v][0]), v)
    first_of = {v: keys[id(clus[v][0])] for v in clus}
    vcov = np.full((n_agg, R * P, R * P), np.nan)
    everyone = np.arange(n)
    for h in range(n_agg):
        T = {v: [np.zeros((int(k), R, P, R)) for k in clus[v][1]]
             for v in keys.values()}
        for i, starts, ends in _pool_batches(np.flatnonzero(agg_p == h), geo,
                                             pool, chunk):
            post = _posteriors(i, geo, bins, M, fold_p, phis)
            y = Y[i]
            for s0, e0 in zip(starts, ends):
                c = pool[geo[i[s0]]]
                fp = fold_p[i[s0:e0]]
                # a pooling unit may span folds: one block per fold
                for v in np.unique(fp):
                    rows = s0 + np.flatnonzero(fp == v)
                    _if_terms(T[first_of[int(v)]], phis[int(v)],
                              bins[:, i[rows]], post[rows], y[rows], mu0[c],
                              Gc[c])
        V = np.zeros((R * P, R * P))
        for i, starts, ends in _pool_batches(everyone, geo, pool, chunk):
            IF = np.zeros((len(i), R * P))
            for v, Tv in T.items():
                for f in range(n_fields):
                    b = clus[v][0][f, i]
                    ub, inv = np.unique(b, return_inverse=True)
                    order = np.argsort(inv, kind="stable")
                    cuts = np.r_[0, np.cumsum(np.bincount(inv))]
                    Ui = U[geo[i[order]]]
                    for j, bj in enumerate(ub):
                        rows = order[cuts[j]:cuts[j + 1]]
                        IF[rows] += (Ui[cuts[j]:cuts[j + 1]]
                                     @ Tv[f][bj].reshape(R * P, R).T)
            own = np.flatnonzero(agg_p[i] == h)
            if len(own):
                post = _posteriors(i[own], geo, bins, M, fold_p, phis)
                c_o = pool[geo[i[own]]]
                for c in np.unique(c_o):
                    t = np.flatnonzero(c_o == c)
                    err = Y[i[own[t]]] - post[t] @ mu0[c]
                    gpi = post[t] @ Gc[c].T
                    IF[own[t]] += (gpi[:, :, None]
                                   * err[:, None, :]).reshape(len(t), R * P)
            ug, ginv = np.unique(geo[i], return_inverse=True)
            ind = sparse.csr_matrix(
                (np.ones(len(i)), (ginv, np.arange(len(i)))),
                shape=(len(ug), len(i)))
            S = np.asarray(ind @ IF)
            V += IF.T @ IF - S.T @ (S / n_g[ug][:, None])
        vcov[h] = V
        _log(verbose, f"  influence function: aggregate {h + 1} of {n_agg}")
    return vcov


def _if_terms(T: list[Array], phis: list[Array], bins: IntArray, post: Array,
              y: Array, mu: Array, Gc: Array) -> None:
    """Add to T the derivatives of one pooling unit's weighted score in
    P(B_f = b | R = s), from people of one fold (see _if_vcov).

    For person i with posterior pi, a = pi / phi_{., B_i}, fitted value
    yhat = mu' pi, residual e = y - yhat and g = Gc pi, the derivative of
    Gc pi e' in phi_{s, B_i} is a_s [Gc[r, s] e_p - g_r (y_p - 2 yhat_p +
    mu[s, p])], indexed [r, p, s].

    Args:
        T: one (K_f, R, P, R) array per name field, added to in place.
        phis: the (R, K_f) P(B_f = b | R = r) of the people's fold.
        bins: (n_fields, m) clusters of the people.
        post, y: (m, R) posteriors and (m, P) outcomes of the people.
        mu: (R, P) the unit's group means.
        Gc: (R, R) the unit's weighted inverse of its corrected equation.
    """
    m, R = post.shape
    P = y.shape[1]
    yhat = post @ mu
    err = y - yhat
    gpi = post @ Gc.T
    z = y - 2 * yhat
    for f in range(len(T)):
        b = bins[f]
        a = post / phis[f][:, b].T
        Bm = a[:, None, :] * (z[:, :, None] + mu.T[None])
        ub, inv = np.unique(b, return_inverse=True)
        ind = sparse.csr_matrix((np.ones(m), (inv, np.arange(m))),
                                shape=(len(ub), m))
        alpha = np.asarray(ind @ (a[:, :, None]
                                  * err[:, None, :]).reshape(m, R * P))
        T[f][ub] += np.einsum("rs,bsp->brps", Gc, alpha.reshape(len(ub), R, P))
        order = np.argsort(inv, kind="stable")
        cuts = np.r_[0, np.cumsum(np.bincount(inv))]
        gs, Bs = gpi[order], Bm[order].reshape(m, P * R)
        for j, bj in enumerate(ub):
            lo, hi = cuts[j], cuts[j + 1]
            T[f][bj] -= (gs[lo:hi].T @ Bs[lo:hi]).reshape(R, P, R)


def _fold_phis(fold_tot: dict[int, list[tuple[Array, Array]]],
               fold_ids: Sequence[int], weight: Array,
               leave_out: bool = True) -> Phis:
    """P(B_f = b | R = r) for the people of each fold, fit on the fold terms
    weighted by `weight` (one per fold of the terms), without the person's
    own fold when `leave_out`."""
    out: Phis = {}
    for v in fold_ids:
        wv = np.asarray(weight, float).copy()
        if leave_out:
            wv[v] = 0.0
        out[int(v)] = [_phi_from_sums(np.tensordot(wv, tX, 1),
                                      np.tensordot(wv, tY, 1))
                       for tX, tY in fold_tot[int(v)]]
    return out


def _bootstrap_vcov(
        estimate: Any, fold_tot: dict[int, list[tuple[Array, Array]]],
        fold_ids: Sequence[int], n_fold: int, phis0: Phis, n: int,
        n_boot: int, n_split: int, rng: np.random.Generator,
        verbose: bool) -> Array:
    """Covariance of the unprojected aggregates (see downstream_crossfit).

    The covariance is the sum of two parts. The error of the fitted
    P(B = b | R = r): the covariance over `n_boot` replicates that resample
    the folds of the least squares terms with replacement, the people held
    fixed. The error from the people given P(B = b | R = r): over `n_split`
    random splits of the people into halves, the mean of
    (theta_1 - theta_2)(theta_1 - theta_2)' / 4.

    Args:
        estimate: function of (phis, people) returning the (H, R, P)
            unprojected aggregates.

    Returns:
        (H, R * P, R * P) array.
    """
    reps = []
    for b in range(n_boot):
        cnt = rng.multinomial(n_fold, np.full(n_fold, 1 / n_fold))
        reps.append(estimate(_fold_phis(fold_tot, fold_ids, cnt),
                             np.arange(n)))
        _log(verbose, f"  bootstrap of P(B | R): replicate {b + 1} of "
                      f"{n_boot}")
    reps_a = np.stack(reps).reshape(n_boot, reps[0].shape[0], -1)
    H, RP = reps_a.shape[1], reps_a.shape[2]
    vcov = np.zeros((H, RP, RP))
    for h in range(H):
        vcov[h] = np.cov(reps_a[:, h].T, ddof=1).reshape(RP, RP)
    for j in range(n_split):
        half = rng.random(n) < 0.5
        d = (estimate(phis0, np.flatnonzero(half))
             - estimate(phis0, np.flatnonzero(~half))).reshape(H, RP)
        vcov += np.einsum("hi,hj->hij", d, d) / (4 * n_split)
        _log(verbose, f"  half split {j + 1} of {n_split}")
    return vcov


def combine_clusterings(results: Sequence[dict[str, Any]],
                        constraint: str | None = "simplex") -> dict[str, Any]:
    """Average of the estimates from several clusterings (for example K-means
    with different seeds).

    The estimate depends on the random start of K-means. The average over m
    clusterings removes most of that dependence; its covariance is the mean
    of the clusterings' covariances plus the covariance of the unprojected
    aggregates across clusterings divided by m.

    Args:
        results: outputs of downstream_crossfit() with n_seeds = 1, one per
            clustering, for the same people, outcome and aggregates.
        constraint: the constraint the averaged aggregates are projected
            onto.

    Returns:
        dict like downstream_crossfit()'s, with K stacked over clusterings.
    """
    m = len(results)
    raw = np.stack([r["theta_raw"] for r in results])
    theta_raw = raw.mean(0)
    vcov = None
    if all(r["vcov"] is not None for r in results):
        vcov = np.mean([r["vcov"] for r in results], 0)
        if m > 1:
            H = raw.shape[1]
            flat = raw.reshape(m, H, -1)
            for h in range(H):
                vcov[h] = vcov[h] + np.cov(flat[:, h].T, ddof=1) / m
    return dict(mu=np.mean([r["mu"] for r in results], 0),
                theta=_project_aggregates(theta_raw, constraint),
                theta_raw=theta_raw, vcov=vcov, N=results[0]["N"],
                K=np.concatenate([np.asarray(r["K"]) for r in results]),
                pooling=[p for r in results for p in r["pooling"]])


def downstream_crossfit(
        F: npt.ArrayLike | Sequence[npt.ArrayLike], geo: npt.ArrayLike,
        prior: npt.ArrayLike, Y: npt.ArrayLike,
        fold: npt.ArrayLike | None = None, pool: npt.ArrayLike | None = None,
        by: npt.ArrayLike | None = None,
        K: int | Sequence[int] | None = None, V: int = 5,
        bins: npt.ArrayLike | None = None,
        fold_stats: Sequence[tuple[Array, Array]] | None = None,
        constraint: str | None = "simplex", correct: bool = False,
        se: str | None = "auto", n_boot: int = 100, n_split: int = 10,
        n_seeds: int = 3, fold_clusters: bool = False,
        seed: int = 0, chunk: int = 50000, verbose: bool = True,
) -> dict[str, Any]:
    """Cross-fitted estimator of the mean outcome of each group.

    Under the lBISG assumptions and the BIRDiE assumption that the outcome is
    independent of the name given the group and geography,
    E[Y | B, G] = sum_r mu_r P(R = r | B, G). The estimand is
    mu_{c, r} = E[Y | R = r, c], the mean outcome of group r in pooling unit
    c (for example the county), and its aggregates over the pooling units of
    each aggregate (for example the state), weighted by the group's prior
    members in each unit, the sum of P(R = r | G) over the unit's people.

    Within each pooling unit, the group means solve the least squares of Y on
    the posterior probabilities. The posterior of each person uses the
    P(B = b | R = r) fit on the other folds of geographies; folds should be
    spatial blocks, such as states. The clusters are fit once on all
    people, or on the other folds as well with `fold_clusters`, in which
    case a person's posterior uses nothing from their own fold.

    With many groups whose names and geography are similar, each unit's
    least squares is close to singular, and its estimates of rare groups can
    lie far outside [0, 1]. By default (correct = False) the means of the
    pooling units are therefore pooled toward the means of their aggregate
    by empirical Bayes (see _eb_pools()): each group's means are pulled
    toward the aggregate in proportion to how little the unit's own data
    identify them, with the pooling estimated from the data.

    With correct = True, the means of each pooling unit are instead the
    least squares with the orthogonal correction of the paper's estimating
    equation, without pooling: the change in each unit's score when the
    cross-fitted P(B = b | R = r) is replaced by the fit on all folds (to
    first order, the derivative of the score in that direction). The
    correction removes the first-order sensitivity to the error of the
    fitted P(B = b | R = r), but it needs that error to be small in every
    direction, which holds for a few groups with distinct names and
    geography. With many similar groups, M'WM is badly conditioned, the
    corrected equations can be indefinite, and the estimates and standard
    errors break down; the condition number of M'WM is the check.

    The constraint (the simplex for an outcome with mutually exclusive
    categories, or [0, 1]) is imposed on the reported aggregates only: each
    aggregate is projected onto it. A group whose unprojected aggregate
    (theta_raw) lies far outside the constraint, with a large standard error,
    is not identified by the data.

    The estimate depends on the random start of K-means. Without `bins`,
    the clusters are fit with `n_seeds` seeds and the estimates averaged
    (see combine_clusterings()). With `bins`, run each clustering separately
    and combine them with combine_clusterings().

    Standard errors, with the geographies held fixed. se = "bootstrap" (the
    default with correct = False) adds two parts: the error of the fitted
    P(B = b | R = r), from `n_boot` replicates that resample the folds of
    the least squares terms with replacement (it needs many folds, for
    example the states), and the error from the people given
    P(B = b | R = r), from `n_split` random splits of the people into
    halves. se = "if" (the default with correct = True) is the influence
    function of the corrected estimator; it includes the error of the fitted
    P(B = b | R = r) through the people in Y (with `fold_stats`, people
    outside Y are left out). Both are for the unprojected aggregates;
    intervals for a projected aggregate are cut at the constraint.

    With several name fields, the clusters of the fields are taken as
    independent given the group and geography (the lBIFSG assumption), and
    the posterior is proportional to P(R = r | G) prod_f P(B_f = b | R = r).

    Args:
        F: (N, L) list scores per person (from lbisg_field()), or a list of
            such matrices, one per name field.
        geo: (N,) 0-based geography of each person, at the prior's level.
        prior: (N, R) P(R = r | G) per person; rows sum to one.
        Y: (N,) or (N, P) outcome; the columns are estimated jointly.
        fold: (n_geo,) 0-based fold of each geography, or None for V folds
            of geographies with approximately equal numbers of people.
        pool: (n_geo,) 0-based pooling unit of each geography, nested within
            `by`, or None for one pooling unit per aggregate (no pooling).
        by: (n_geo,) 0-based aggregate of each geography, or None for a
            single aggregate over all geographies.
        K: number of clusters, or a list with one per name field; required
            unless `bins` is given.
        V: number of folds when `fold` is None.
        bins: optional (n_fields, N) clusters of each person in each field,
            to use an existing clustering.
        fold_stats: optional list, one per field, of the (XtWX, XtWY) terms of
            _fold_stats() over a larger set of folds (for example every
            state), so that P(B = b | R = r) is fit on data outside Y.
            `fold` then indexes these folds, and `bins` is required.
        constraint: "simplex", "box" or None. With a constraint, the outcome
            should lie in [0, 1]; it also bounds the pooling variance.
        correct: the corrected least squares without pooling, instead of
            the empirical-Bayes pooling.
        se: "bootstrap", "if", "auto" ("bootstrap" with correct = False,
            "if" with correct = True) or None.
        n_boot: bootstrap replicates of P(B = b | R = r).
        n_split: half splits of the people.
        n_seeds: K-means seeds averaged over when `bins` is None.
        fold_clusters: fit the clusters of each fold on the other folds
            (Algorithm 3), or False for one clustering of all people.
            Ignored when `bins` is given. The least squares terms are then
            kept for each fold's clustering, V times the memory of one.
        seed: first K-means seed (seeds seed, ..., seed + n_seeds - 1) and
            seed of the bootstrap.
        chunk: people per block.
        verbose: print progress.

    Returns:
        dict with
          mu (n_pool, R, P): group means in each pooling unit, nan for groups
            with no prior mass there;
          theta (H, R, P): the estimate of each aggregate, with the
            constraint;
          theta_raw (H, R, P): the same before the aggregate's projection;
          vcov (H, R * P, R * P): covariance of theta_raw_h, indexed (r, p)
            row-major, or None with se = None;
          N (H, R): group prior members in each aggregate;
          K: clusters used in each name field, (n_seeds, n_fields), or
            (n_seeds, V, n_fields) with fold_clusters;
          pooling: for each clustering, the empirical-Bayes fit (m, tau2,
            sigma2, iters), or None with correct = True.
    """
    fields: list[Any] = list(F) if isinstance(F, (list, tuple)) else [F]
    n_fields = len(fields)
    geo, prior_a, n_geo, w, M = _geo_setup(geo, prior)
    Y = np.asarray(Y, float)
    if Y.ndim == 1:
        Y = Y[:, None]
    n, R = prior_a.shape
    P = Y.shape[1]
    correct = bool(correct)
    if constraint not in ("simplex", "box", None):
        raise ValueError("constraint must be 'simplex', 'box' or None")
    if constraint == "simplex" and P < 2:
        raise ValueError("constraint 'simplex' needs one column per category")
    if fold_stats is not None and (fold is None or bins is None):
        raise ValueError("fold_stats requires fold and bins")
    if se == "auto":
        se = "if" if correct else "bootstrap"
    if se not in ("bootstrap", "if", None):
        raise ValueError("se must be 'bootstrap', 'if', 'auto' or None")
    if se == "if" and not correct:
        raise ValueError("se 'if' requires correct = True")
    if se == "bootstrap" and correct:
        raise ValueError("se 'bootstrap' requires correct = False")
    if int(n_seeds) < 1:
        raise ValueError("n_seeds must be at least 1")
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
    fold_ids = [int(v) for v in np.unique(fold_p)]
    n_fold = (int(np.max(fold_g)) + 1 if fold_stats is None
              else fold_stats[0][0].shape[0])
    prior_mass = np.zeros((n_pool, R))
    np.add.at(prior_mass, pool, w[:, None] * M)
    solve = prior_mass > 0
    weight = prior_mass * solve
    everyone = np.arange(n)
    if bins is None:
        K_fields: list[Any] = (list(K) if isinstance(K, (list, tuple))
                               else [K] * n_fields)
        if len(K_fields) != n_fields or any(k is None for k in K_fields):
            raise ValueError("K must have one value per name field")
        Z_fields = [logodds(F_f) for F_f in fields]
        seeds = [int(seed) + s for s in range(int(n_seeds))]
    else:
        seeds = [int(seed)]

    def clusterings(seed_s: int) -> dict[int, tuple[IntArray, IntArray]]:
        """Algorithm 1, Step 7: clus[v] is the clustering of the people of
        fold v and of the P(B = b | R = r) their posteriors use."""
        if bins is not None:
            b_all = np.asarray(bins, np.int64).reshape(n_fields, n)
            return {v: (b_all, b_all.max(1) + 1) for v in fold_ids}
        clus: dict[int, tuple[IntArray, IntArray]] = {}
        for v in (fold_ids if fold_clusters else [-1]):
            rows = np.flatnonzero(fold_p != v)
            b_v = np.empty((n_fields, n), np.int64)
            k_v = np.zeros(n_fields, np.int64)
            for f in range(n_fields):
                b_v[f], k_v[f] = kmeans_occupied(Z_fields[f], rows,
                                                 int(K_fields[f]), seed_s)
                _log(verbose, f"  seed {seed_s}, field {f + 1}"
                              + (f", fold {v}" if fold_clusters else "")
                              + f": K = {K_fields[f]} ({k_v[f]} used)")
            clus[int(v)] = (b_v, k_v)
        if not fold_clusters:
            clus = {v: clus[-1] for v in fold_ids}
        return clus

    def run(seed_s: int) -> dict[str, Any]:
        """The estimate and its covariance for one clustering."""
        clus = clusterings(seed_s)
        per_fold = bins is None and fold_clusters
        bins_a = np.empty((n_fields, n), np.int64)
        for v in fold_ids:
            s = fold_p == v
            bins_a[:, s] = clus[v][0][:, s]
        K_used = (np.stack([clus[v][1] for v in fold_ids]) if per_fold
                  else clus[fold_ids[0]][1])
        # Step 9 terms by fold, for each fold's clustering
        fold_tot: dict[int, list[tuple[Array, Array]]] = {}
        for v in fold_ids:
            if not per_fold and fold_tot:
                fold_tot[v] = fold_tot[next(iter(fold_tot))]
                continue
            b_v, k_v = clus[v]
            fold_tot[v] = (list(fold_stats) if fold_stats is not None else
                           [_fold_stats(b_v[f], geo, M, w, fold_g, n_fold,
                                        int(k_v[f]))
                            for f in range(n_fields)])
        ones = np.ones(n_fold)
        phis0 = _fold_phis(fold_tot, fold_ids, ones)
        if correct:
            fulls0 = _fold_phis(fold_tot, fold_ids, ones, leave_out=False)
            A, B, _, C, c0 = _pool_grams(everyone, geo, bins_a, M, Y, pool,
                                         n_pool, fold_p, phis0, chunk, fulls0)
            mu = _solve_pools(A + C, B + c0, solve)
            theta_raw, N = _aggregate(mu, weight, by_pool, n_agg)
            vcov = None
            if se == "if":
                G = fold_tot[fold_ids[0]][0][0].sum(0)
                vcov = _if_vcov(geo, bins_a, M, Y, pool, by_pool, n_agg,
                                fold_p, clus, phis0, A + C, mu, solve, weight,
                                N, G, chunk, verbose)
            return dict(mu=mu, theta_raw=theta_raw, vcov=vcov, N=N,
                        K=K_used[None], pooling=[None])

        def estimate(phis: Phis, people: IntArray,
                     init: dict[str, Any] | None = None
                     ) -> tuple[Array, dict[str, Any]]:
            A, B, _, _, _ = _pool_grams(people, geo, bins_a, M, Y, pool,
                                        n_pool, fold_p, phis, chunk)
            yy = np.zeros((n_pool, P))
            np.add.at(yy, pool[geo[people]], Y[people] ** 2)
            mu, fit = _eb_pools(A, B, yy, solve, by_pool, n_agg, constraint,
                                init=init,
                                max_iter=2000 if init is None else 200)
            return mu, fit

        mu, fit0 = estimate(phis0, everyone)
        theta_raw, N = _aggregate(mu, weight, by_pool, n_agg)
        _log(verbose, f"  empirical Bayes: {fit0['iters']} EM iterations")
        vcov = None
        if se == "bootstrap":
            vcov = _bootstrap_vcov(
                lambda ph, people: _aggregate(
                    estimate(ph, people, fit0)[0], weight, by_pool,
                    n_agg)[0],
                fold_tot, fold_ids, n_fold, phis0, n, int(n_boot),
                int(n_split), np.random.default_rng(seed_s), verbose)
        return dict(mu=mu, theta_raw=theta_raw, vcov=vcov, N=N,
                    K=K_used[None], pooling=[fit0])

    return combine_clusterings([run(s) for s in seeds], constraint)


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
