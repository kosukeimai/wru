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
  - downstream_crossfit(): the cross-fitted estimator of the mean outcome of
    each group in each geography.

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

import warnings
from typing import Any, Sequence

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
# One fold of the downstream estimator: its people, their clusters in each
# name field, and P(B_f = b | R = r) of each name field
Nuisance = tuple[IntArray, IntArray, list[Array]]


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
def _fold_nuisance(
        fields: Sequence[Array], Z_fields: Sequence[Array], geo: IntArray,
        prior: Array, M: Array, w: Array, fold_g: IntArray, v: int,
        K: Sequence[int | None], grid: Sequence[int] | None, J: int,
        seed: int, verbose: bool) -> Nuisance:
    """Clusters and P(B = b | R = r) of fold v, from the observations not in
    fold v (the loop of the cross-fitting algorithm).

    Returns:
        (rows, bins, phis): the people of fold v, their (n_fields, n_v)
        clusters B^(-v), and one (R, K^(-v)) matrix phi^(-v) per name field.
    """
    fold_p = fold_g[geo]
    train = np.flatnonzero(fold_p != v)
    rows = np.flatnonzero(fold_p == v)
    take = (fold_g != v).astype(float)
    bins, phis = [], []
    for f, (F_f, Z) in enumerate(zip(fields, Z_fields)):
        K_v = K[f]
        if K_v is None:
            geo_train = np.unique(geo[train], return_inverse=True)[1]
            K_v, _ = select_K(F_f[train], np.asarray(geo_train).ravel(),
                              prior[train], grid=grid, J=J, seed=seed,
                              verbose=verbose)
        labels, K_used = kmeans_occupied(Z, train, int(K_v), seed)
        _log(verbose, f"  fold {v}, field {f + 1}: K = {K_v} ({K_used} used)")
        C = sparse.coo_matrix((np.ones(len(train)), (geo[train], labels[train])),
                              shape=(M.shape[0], K_used)).tocsr()
        phis.append(wls_pi(C, M, w, take))
        bins.append(labels[rows])
    return rows, np.stack(bins), phis


def _products(phis: Sequence[Array]) -> tuple[list[Array], Array, list[Array]]:
    """Sums over the clusters of the products of phi.

    Returns:
        (S, S_all, others): S[f] = sum_b phi^f_b phi^f_b'; S_all is their
        elementwise product, sum_b phi_b phi_b' over the cells b of all name
        fields; others[f] is the elementwise product without field f (all ones
        with one field).
    """
    S: list[Array] = [Phi @ Phi.T for Phi in phis]
    S_all: Array = np.prod(S, axis=0)
    others: list[Array] = [np.prod([S[h] for h in range(len(S)) if h != f]
                                   + [np.ones_like(S_all)], axis=0)
                           for f in range(len(S))]
    return S, S_all, others


def _zeta_matrix(nuisance: Sequence[Nuisance], geo: IntArray, M: Array,
                 Qinv: Array) -> Array:
    """The term of the estimating equation that holds zeta.

    Summing over the clusters and the people of each fold v,
    (1/n) sum_v sum_{i in V_v} sum_b A^(-v)_bg zeta_b(O_i^(-v), phi^(-v))'
    (M_g * mu_g) = (M_g * T * M_g') mu_g, the same T for every g, with
    T = (1/n) sum_v sum_{i in V_v} (phi^(-v)_{B_i} - S^(-v) M_{G_i})
    M_{G_i}' E[M_G M_G']^{-1}. Several name fields add one such term per field
    by the product rule, each multiplied elementwise by the S of the others.

    Returns:
        (R, R) array T.
    """
    T = np.zeros((M.shape[1], M.shape[1]))
    n = sum(len(rows) for rows, _, _ in nuisance)
    for rows, bins, phis in nuisance:
        Mi = M[geo[rows]]
        U = Mi @ Qinv
        S, _, others = _products(phis)
        for f, Phi in enumerate(phis):
            T += ((Phi[:, bins[f]].T - Mi @ S[f]).T @ U) * others[f]
    return T / n


def _solve_mu(nuisance: Sequence[Nuisance], geo: IntArray, M: Array, Y: Array,
              T: Array) -> tuple[Array, BoolArray]:
    """Solve the estimating equation for each geography.

    With D_g = diag(M_g) and v the fold of g, the equation is
    D_g (S_all^(-v) + T) D_g mu_g
    = D_g (1 / n_g) sum_{i: G_i = g} phi^(-v)_{B_i} Y_i,
    using the sample share n_g / n for Pr(G = g). D_g cancels from the left.
    There is no solution when a group has M_{g, r} = 0, or when S_all^(-v) or
    S_all^(-v) + T is rank deficient; mu_g is then nan.

    Returns:
        (mu, singular): the (G, R, P) estimates and the (G,) geographies with
        no solution, including those with no people.
    """
    n_geo, R = M.shape
    P = Y.shape[1]
    n_g = np.bincount(geo, minlength=n_geo).astype(float)
    mu = np.full((n_geo, R, P), np.nan)
    singular = np.ones(n_geo, bool)
    for rows, bins, phis in nuisance:
        _, S_all, _ = _products(phis)
        X = S_all + T
        if np.linalg.matrix_rank(S_all) < R or np.linalg.matrix_rank(X) < R:
            continue
        cell: Array = np.ones((len(rows), R))
        for f, Phi in enumerate(phis):
            cell = cell * Phi[:, bins[f]].T
        g_v, inv = np.unique(geo[rows], return_inverse=True)
        inv = np.asarray(inv).ravel()
        rhs = np.zeros((len(g_v), R, P))
        for r in range(R):
            for p in range(P):
                rhs[:, r, p] = np.bincount(inv, weights=cell[:, r] * Y[rows, p],
                                           minlength=len(g_v))
        rhs = rhs / n_g[g_v][:, None, None]
        sol = np.linalg.solve(X, rhs)
        use = (M[g_v] > 0).all(1)
        mu[g_v[use]] = sol[use] / M[g_v[use]][:, :, None]
        singular[g_v[use]] = False
    return mu, singular


def _zeta_coef(others: Sequence[Array], d: Array) -> Array:
    """Coefficients of the zeta term of the influence function for one
    geography, as a linear map of the person-level x_i of _if_vcov().

    Args:
        others: as in _products().
        d: (R, P) M_g * mu_g.

    Returns:
        (R * P, n_fields * R * R) array.
    """
    R, P = d.shape
    F = len(others)
    B = np.zeros((R * P, F * R * R))
    for f in range(F):
        off = f * R * R
        for r in range(R):
            B[r * P:(r + 1) * P, off + r * R:off + (r + 1) * R] = (
                others[f][r][:, None] * d).T
    return B


def _if_vcov(nuisance: Sequence[Nuisance], geo: IntArray, M: Array, Y: Array,
             Qinv: Array, mu: Array, singular: BoolArray,
             chunk: int = 10000) -> Array:
    """Covariance of mu_g from the influence function of the paper's theorem.

    For person i of fold v, with the plug-ins of fold v,
    varphi_i = (A_g' A_g)^{-1} sum_b A_bg [eta_bg(O_i) - zeta_b(O_i)'
    (M_g * mu_g)]; the estimated covariance of mu_g is
    (1/n^2) sum_v sum_{i in V_v} varphi_i varphi_i', without centring.

    Only the people of g have an eta term. Every person has a zeta term,
    z_i = B_g x_i, with x_i the vector of the products of
    phi^f_{B_i} - S^f M_{G_i} and E[M_G M_G']^{-1} M_{G_i} over the name
    fields and B_g from _zeta_coef(). The sum of z_i z_i' over a fold is
    therefore B_g C B_g', with C = sum_i x_i x_i' computed once per fold. C has
    (n_fields R^2)^2 entries per fold, which limits the number of groups.

    Returns:
        (G, R * P, R * P) array indexed (r, p) row-major, nan where `singular`.
    """
    n_geo, R = M.shape
    P = Y.shape[1]
    F = len(nuisance[0][2])
    n = sum(len(rows) for rows, _, _ in nuisance)
    n_g = np.bincount(geo, minlength=n_geo).astype(float)
    vcov = np.full((n_geo, R * P, R * P), np.nan)
    keep = np.flatnonzero(~singular)
    vcov[keep] = 0.0
    for rows, bins, phis in nuisance:
        S, S_all, others = _products(phis)
        full = np.linalg.matrix_rank(S_all) == R
        L = np.linalg.inv(S_all) if full else np.full((R, R), np.nan)

        def x_of(idx: IntArray) -> Array:
            Mi = M[geo[rows[idx]]]
            U = Mi @ Qinv
            return np.concatenate(
                [((phis[f][:, bins[f, idx]].T - Mi @ S[f])[:, :, None]
                  * U[:, None, :]).reshape(len(idx), R * R)
                 for f in range(F)], axis=1)

        C = np.zeros((F * R * R, F * R * R))
        for lo in range(0, len(rows), chunk):
            x = x_of(np.arange(lo, min(lo + chunk, len(rows))))
            C += x.T @ x
        order = np.argsort(geo[rows], kind="stable")
        g_v, start = np.unique(geo[rows[order]], return_index=True)
        stop = np.r_[start[1:], len(rows)]
        own = {int(g): order[a:b] for g, a, b in zip(g_v, start, stop)}
        for g in keep:
            m = M[g]
            d = m[:, None] * mu[g]
            B = _zeta_coef(others, d)
            Z = B @ C @ B.T
            idx = own.get(int(g))
            if idx is not None:
                cell: Array = np.ones((len(idx), R))
                for f, Phi in enumerate(phis):
                    cell = cell * Phi[:, bins[f, idx]].T
                a = (n / n_g[g]) * (cell[:, :, None] * Y[rows[idx]][:, None, :]
                                    - S_all @ d)
                a = a.reshape(len(idx), R * P)
                z = x_of(idx) @ B.T
                az = a.T @ z
                Z = Z + a.T @ a - az - az.T
            W = np.kron(L / m[:, None], np.eye(P))
            vcov[g] += W @ Z @ W.T
    return vcov / n ** 2


def downstream_crossfit(
        F: npt.ArrayLike | Sequence[npt.ArrayLike], geo: npt.ArrayLike,
        prior: npt.ArrayLike, Y: npt.ArrayLike,
        fold: npt.ArrayLike | None = None, V: int = 5,
        K: int | Sequence[int] | None = None,
        grid: Sequence[int] | None = None, J: int = 5, se: bool = True,
        seed: int = 0, verbose: bool = True,
) -> dict[str, Any]:
    """Cross-fitted estimator of mu_g, the mean outcome of each group in each
    geography, E[Y | R = r, G = g], under the BIRDiE assumption that the outcome
    is independent of the name given the group and geography.

    The geographic units are randomly partitioned into V folds. For each fold
    v, the observations outside the fold give K^(-v) (Algorithm 2, or `K`),
    the clusters B^(-v) and phi^(-v) = P(B = b | R = r) (Algorithm 1, Steps
    7-10). With O = (Y, B, G) and A_bg = M_g * phi_b, the moments are

      zeta_b(O, phi_b) = E[M_G M_G']^{-1} M_G (1{B = b} - M_G' phi_b),
      eta_bg(O, phi_b, mu_g) = 1{G = g} / Pr(G = g) [Y 1{B = b} - A_bg' mu_g],
      psi^(-v)_g = sum_b A^(-v)_bg [eta_bg(O_i^(-v), phi^(-v)_b, mu_g)
                     - zeta_b(O_i^(-v), phi^(-v)_b)' (M_g * mu_g)],

    and mu_g solves (1/n) sum_i sum_v 1{i in V_v} psi^(-v)_g = 0. The equation
    is linear in mu_g; see _zeta_matrix() and _solve_mu() for its closed form.
    Pr(G = g) is the sample share of g and E[M_G M_G'] the mean over all people.

    With several name fields, A_bg = M_g * prod_f phi^f_{b_f} and the derivative
    of A_bg' mu_g gives one zeta term per field by the product rule.

    Args:
        F: (N, L) list scores per person (from lbisg_field()), or a list of
            such matrices, one per name field.
        geo: (N,) 0-based geography of each person; g in mu_g.
        prior: (N, R) P(R = r | G) per person; rows sum to one.
        Y: (N,) or (N, P) outcome. Each column is its own estimating equation.
        fold: (n_geo,) 0-based fold of each geography, or None for a random
            partition into V folds.
        V: number of folds when `fold` is None.
        K: number of clusters, or a list with one per name field; None chooses
            K^(-v) by Algorithm 2 in each fold's training sample.
        grid: candidate values of K for Algorithm 2.
        J: number of geographic folds of Algorithm 2.
        se: compute the covariance of mu_g from the influence function.
        seed: seed of the random partition, K-means and Algorithm 2.
        verbose: print progress.

    Returns:
        dict with
          mu (G, R, P): the estimates, nan for geographies with no solution;
          vcov (G, R * P, R * P): covariance of each mu_g, indexed (r, p)
            row-major, or None with se = False;
          singular (G,): True for geographies with no solution, which also
            raise a warning;
          fold (G,): the fold of each geography;
          K (V, n_fields): clusters used in each fold and name field.
    """
    F_list = list(F) if isinstance(F, (list, tuple)) else [F]
    fields: list[Array] = [np.asarray(F_f, float) for F_f in F_list]
    geo, prior_a, n_geo, w, M = _geo_setup(geo, prior)
    Y = np.asarray(Y, float)
    if Y.ndim == 1:
        Y = Y[:, None]
    n = prior_a.shape[0]
    if K is None:
        K_fields: list[int | None] = [None] * len(fields)
    elif isinstance(K, (int, np.integer)):
        K_fields = [int(K)] * len(fields)
    else:
        K_fields = [int(k) for k in K]
    if len(K_fields) != len(fields):
        raise ValueError("K must have one value per name field")
    fold_g = (np.random.default_rng(seed).permutation(n_geo) % int(V)
              if fold is None else np.asarray(fold, np.int64))
    fold_ids = [int(v) for v in np.unique(fold_g[geo])]
    if len(fold_ids) < 2:
        raise ValueError("cross-fitting needs at least two folds")
    Qinv = np.linalg.inv(M.T @ (w[:, None] * M) / n)
    Z_fields = [logodds(F_f) for F_f in fields]
    nuisance = [_fold_nuisance(fields, Z_fields, geo, prior_a, M, w, fold_g, v,
                               K_fields, grid, int(J), int(seed), verbose)
                for v in fold_ids]
    T = _zeta_matrix(nuisance, geo, M, Qinv)
    mu, singular = _solve_mu(nuisance, geo, M, Y, T)
    if singular.any():
        warnings.warn(f"{int(singular.sum())} of {n_geo} geographies have no "
                      "solution of the estimating equation; their estimates "
                      "are nan", stacklevel=2)
    vcov = _if_vcov(nuisance, geo, M, Y, Qinv, mu, singular) if se else None
    return dict(mu=mu, vcov=vcov, singular=singular, fold=fold_g,
                K=np.array([[Phi.shape[1] for Phi in phis]
                            for _, _, phis in nuisance]))


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
