"""lBISG helper for wru: list scores, clustering and estimation.

Called from R through reticulate (see R/lbisg.R) and not usually run directly.
Name embeddings are obtained with ebisg_helper.embed_names(). Chasalow,
Dasanaike and Imai (2026) describe the method:
https://arxiv.org/abs/2610.06273.

For each name field, lbisg_field()
  1. trains a network for each list on the name embeddings, and scores each
     name with a network that was trained without that name
     (list_scores_oof());
  2. determines the number of clusters K (select_K());
  3. clusters the log-odds list scores, estimates P(cluster | group) by
     weighted least squares on the geographic prior, and applies Bayes' rule
     (fit_posterior()).

combine_fields() combines the posteriors of several name fields.
recover_prevalence() estimates the geographic prior from the list rates when
the prior is unknown or known only for coarse groups. list_quality() estimates
how well the lists separate the groups.

Example:
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

EPS_Z = 1e-6     # clip for the log-odds of the list scores
# Floor for P(B = b | R = r). The floor ensures that P(B | G) remains above 0.
FLOOR = 1e-9
RIDGE = 1e-9     # added to the diagonal of M'WM
DEFAULT_GRID = [10, 15, 20, 30, 50, 75, 100, 150, 200, 300, 450, 600, 999,
                1500, 2000, 3000, 5000, 7500, 10000, 15000, 20000]

Array = npt.NDArray[np.floating[Any]]
IntArray = npt.NDArray[np.int64]


def _log(verbose: bool, *args: object) -> None:
    if verbose:
        print(*args, flush=True)


def _device() -> torch.device:
    return torch.device("cuda" if torch.cuda.is_available() else "cpu")


# ---------------------------------------------------------------------------
# List scores
# ---------------------------------------------------------------------------
def name_folds(counts: npt.ArrayLike, J: int = 5,
               seed: int = 2026) -> IntArray:
    """Split the distinct names at random into J folds.

    Names are taken in random order, and each name is assigned to the fold
    with the fewest people so far. The folds therefore contain approximately
    equal numbers of people.

    Args:
        counts: (n_names,) number of people with each name.
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
    """A separate network for each list.

    The networks share no parameters, and each network has a separate
    optimizer. Training the networks together therefore produces the same
    result as training them one at a time.

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

        The loss for each name is weighted by the number of people with that
        name.
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
    """Out-of-fold list scores for the distinct names.

    Each name is scored by a model that was trained without the fold that
    contains the name. For each fold k, the number of epochs is selected by
    cross-validation on the other J - 1 folds. A model is trained with each of
    those folds held out, and the selected number of epochs is the one with the
    lowest mean held-out loss. Training is stopped when that loss has not
    improved for `patience` epochs, or at `max_epochs`. A model is then
    trained on all folds except k for the selected number of epochs, and this
    model is used to score the names in fold k.

    Args:
        emb: (n_names, D) name embeddings.
        onmat: (n_names, L) 0/1 list membership of each name.
        counts: (n_names,) number of people with each name. The loss is
            weighted by these counts in order for the fit to be equivalent to a
            fit on individual people.
        J: number of name folds.
        max_epochs, patience: limits on the selected number of epochs.
        batch_size: minibatch size in distinct names.
        seed: seed for the network weights and minibatch order.
        fold_seed: seed for the name folds.
        verbose: print progress.

    Returns:
        (scores, info): scores is an (n_names, L) array of P(L_l = 1 | name).
        info maps each fold to the selected number of epochs and the mean
        held-out loss for that fold.
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


def save_list_model(model: PerListModel, groups: Sequence[str], path: str,
                    embedding: str = "intfloat/multilingual-e5-large") -> None:
    """Save the networks of `model` in the format read by pretrained_scores().

    The per-list networks are stored as stacked weights W1 (L, D, 256),
    b1 (L, 256), W2 (L, 256, 128), b2 (L, 128), W3 (L, 128) and b3 (L), in
    half precision, with the list names in `groups`.
    """
    lin = [[m for m in net if isinstance(m, nn.Linear)] for net in model.nets]
    stack = lambda f: torch.stack([f(l) for l in lin]).detach().half().cpu()
    state = dict(
        W1=stack(lambda l: l[0].weight.T), b1=stack(lambda l: l[0].bias),
        W2=stack(lambda l: l[1].weight.T), b2=stack(lambda l: l[1].bias),
        W3=stack(lambda l: l[2].weight[0]), b3=stack(lambda l: l[2].bias[0]))
    torch.save(dict(state=state, groups=[str(g) for g in groups],
                    embedding=embedding,
                    normalisation="upper case, internal spaces collapsed"),
               path)


def fit_list_model(emb: npt.ArrayLike, onmat: npt.ArrayLike,
                   counts: npt.ArrayLike, groups: Sequence[str], path: str,
                   epochs: int | None = None, J: int = 5,
                   max_epochs: int = 100, patience: int = 100,
                   batch_size: int = 64, seed: int = 42,
                   fold_seed: int = 2026,
                   embedding: str = "intfloat/multilingual-e5-large",
                   verbose: bool = True) -> dict[str, Any]:
    """Fit the list models on all names and save them for pretrained_scores().

    With `epochs` None, the number of epochs minimizes the mean held-out loss
    over J folds of the names (training stops once it has not improved for
    `patience` epochs, or at `max_epochs`). The model is then fit to all names
    for that number of epochs. To fit on the lists alone, pass the union of the
    lists as the names, with a count of one for each.

    Returns:
        dict with the number of epochs and the cross-validation curve.
    """
    device = _device()
    X = torch.as_tensor(np.ascontiguousarray(emb, np.float32), device=device)
    Y = torch.as_tensor(np.ascontiguousarray(onmat, np.float32),
                        device=device)
    W = torch.as_tensor(np.maximum(np.asarray(counts, np.float32), 1e-4),
                        device=device)
    input_dim, n_lists = X.shape[1], Y.shape[1]
    curve: list[float] = []
    if epochs is None:
        fold = name_folds(counts, J, fold_seed)
        cv = [dict(model=PerListModel(input_dim, n_lists, seed + j, device),
                   rng=np.random.RandomState(seed + j),
                   train=np.flatnonzero(fold != j),
                   valid=np.flatnonzero(fold == j)) for j in range(J)]
        best_loss, epochs = np.inf, 1
        for epoch in range(1, max_epochs + 1):
            losses = []
            for m in cv:
                m["model"].train_epoch(X, Y, W, m["train"], m["rng"],
                                       batch_size)
                losses.append(m["model"].heldout_bce(X, Y, W, m["valid"]))
            curve.append(float(np.mean(losses)))
            if curve[-1] < best_loss:
                best_loss, epochs = curve[-1], epoch
            elif epoch - epochs >= patience:
                break
        del cv
    model = PerListModel(input_dim, n_lists, seed, device)
    rng = np.random.RandomState(seed)
    allnames = np.arange(len(X))
    for _ in range(int(epochs)):
        model.train_epoch(X, Y, W, allnames, rng, batch_size)
    save_list_model(model, groups, path, embedding)
    _log(verbose, f"  saved list models ({n_lists} lists, {epochs} epochs) "
                  f"to {path}")
    return dict(epochs=int(epochs), cv_curve=curve)


# ---------------------------------------------------------------------------
# Clusters, the number of clusters and the posterior
# ---------------------------------------------------------------------------
def logodds(F: npt.ArrayLike) -> Array:
    """Log-odds of the list scores, after clipping the scores to
    [EPS_Z, 1 - EPS_Z]."""
    F = np.clip(np.asarray(F, float), EPS_Z, 1 - EPS_Z)
    return np.log(F) - np.log(1 - F)


def kmeans_occupied(Z: Array, rows: npt.ArrayLike, K: int,
                    seed: int = 0) -> tuple[IntArray, int]:
    """K-means on the log-odds list scores.

    The centres are fit on the observations `rows`, and each observation is
    assigned to the nearest centre. Centres with no observation from `rows`
    are dropped and the observations are reassigned, until each centre has at
    least one observation from `rows`. Without this step, a cluster could
    have P(B = b) = 0 among the fitting observations, and Q(K) would be
    infinite for any held-out observation in that cluster.

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
    """P(B = b | R = r) by weighted least squares.

    Args:
        C: (G, K) sparse counts of people by geography and cluster.
        M: (G, R) prior P(R = r | G = g).
        w: (G,) people per geography (the weights W).
        take: (G,) 1 for the geographies used in the fit, 0 otherwise.

    Returns:
        (R, K) array, with each row normalized to sum to one.
    """
    R = M.shape[1]
    XtWX = M.T @ ((w * take)[:, None] * M)
    XtWY = np.asarray(C.T @ (M * take[:, None])).T
    Pi = np.linalg.solve(XtWX + RIDGE * np.eye(R), XtWY)
    Pi = np.maximum(Pi, FLOOR)
    return Pi / Pi.sum(1, keepdims=True)


def geo_folds(geo: IntArray, J: int) -> IntArray:
    """J folds of geographic units with approximately equal numbers of people.

    The units are sorted from largest to smallest and assigned to folds
    0, 1, ..., J - 1, 0, 1, ... in that order.
    """
    n_geo = int(np.max(geo)) + 1
    w = np.bincount(geo, minlength=n_geo).astype(float)
    fold = np.zeros(n_geo, int)
    fold[np.argsort(-w, kind="stable")] = np.arange(n_geo) % J
    return fold


def split_units(n_geo: int, frac: float = 0.2, seed: int = 0) -> IntArray:
    """Random split of the geographic units into training (0) and validation.

    Training units are coded 0 and validation units 1.

    Each unit is in the validation set with probability `frac`.
    """
    return (np.random.default_rng(seed).random(n_geo) < frac).astype(int)


def fold_term(Z: Array, geo: IntArray, M: Array, w: Array, n_geo: int,
              K: int, j: int, fold: IntArray,
              seed: int = 0) -> tuple[float, int]:
    """Sum of the log ratios in Q(K) over the units with fold == j.

    The clusters and P(B = b | R = r) are fit on the units with fold != j.
    """
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
             extend: bool = True, verbose: bool = True,
             method: str = "split", val_frac: float = 0.2,
             split_seed: int = 0) -> tuple[int, list[tuple[int, float]]]:
    """Determine the number of clusters K by maximizing the criterion Q(K).

    Q(K) is the mean of log P(B | G) - log P(B) over held-out observations,
    with both probabilities fit on the other geographic units. With
    method="split" (the default), each unit is held out with probability
    `val_frac`. With method="crossfit", the units are divided into J folds of
    approximately equal numbers of people. Each fold is then held out once.

    Values of K below R, or at least the number of distinct score vectors in
    the training data, are excluded. As long as Q(K) is largest at the top of
    the grid, the grid is extended upward by a factor of 1.5.

    Args:
        F: (N, L) list scores per person.
        geo: (N,) 0-based geography of each person.
        prior: (N, R) P(R = r | G) per person, with rows that sum to one.
        grid: candidate values of K (default DEFAULT_GRID).
        J: number of geographic folds (method="crossfit" only).
        seed: random seed for K-means.
        extend: extend the grid upward while the maximum is at the top of
            the grid.
        verbose: print Q(K) as it is computed.
        method: "split" or "crossfit".
        val_frac: probability that a unit is in the validation set
            (method="split" only).
        split_seed: seed of the split (method="split" only).

    Returns:
        (K_star, curve): the selected K and a sorted list of (K, Q(K)).
    """
    Z = logodds(F)
    geo, prior, n_geo, w, M = _geo_setup(geo, prior)
    R = M.shape[1]
    if method == "split":
        fold = split_units(n_geo, val_frac, split_seed)
        held = [1]
        if not ((fold[geo] == 1).any() and (fold[geo] == 0).any()):
            raise ValueError(
                "Choosing K needs geographic units in both the training and "
                "the validation sets. Use more units or another val_frac.")
    elif method == "crossfit":
        fold = geo_folds(geo, J)
        held = list(range(J))
        if len(np.unique(fold)) < J:
            raise ValueError(
                f"Choosing K needs at least J = {J} geographic units")
    else:
        raise ValueError("method must be 'split' or 'crossfit'")
    n_held = int(sum((fold[geo] == j).sum() for j in held))
    limit = min(len(np.unique(Z[fold[geo] != j], axis=0))
                for j in held) - 1
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
                     for j in held]
            curve.append((int(K), sum(t[0] for t in terms) / n_held))
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
    """lBISG posterior for all observations.

    The observations are clustered, P(B = b | R = r) is estimated by weighted
    least squares, and the posterior is computed by Bayes' rule.

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
                verbose: bool = True, k_method: str = "split",
                val_frac: float = 0.2,
                split_seed: int = 0, save_path: str | None = None,
                groups: Sequence[str] | None = None,
                embedding: str = "intfloat/multilingual-e5-large",
                ) -> dict[str, Any]:
    """lBISG for one name field.

    With `save_path`, the list models are also fit to all the distinct names,
    for the mean of the numbers of epochs selected in the folds, and saved
    there for later use with pretrained_scores().

    Args:
        emb_unique: (n_names, D) embeddings of the distinct names.
        idx: (N,) 0-based index into emb_unique of the name of each person.
        onmat_unique: (n_names, L) 0/1 list membership of each name.
        geo: (N,) 0-based geography of each person.
        prior: (N, R) P(R = r | G) per person, with rows that sum to one.
        K: number of clusters, or None to determine K with select_K().
        grid: candidate values of K.
        n_folds: number of name folds for the list scores.
        geo_folds_J: number of geographic folds when
            k_method="crossfit".
        max_epochs, patience, batch_size, seed: see list_scores_oof().
        verbose: print progress.
        k_method, val_frac, split_seed: see select_K() (method, val_frac,
            split_seed).

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
    if save_path is not None:
        ep = max(1, int(round(np.mean([info[k]["selected_epochs"]
                                       for k in info]))))
        fit_list_model(emb_unique[used], np.asarray(onmat_unique)[used],
                       counts[used], groups or [], save_path, epochs=ep,
                       batch_size=int(batch_size), seed=int(seed),
                       embedding=embedding, verbose=verbose)
    res = lbisg_field_scores(scores[remap[idx]], geo, prior, K=K, grid=grid,
                             geo_folds_J=geo_folds_J, verbose=verbose,
                             k_method=k_method, val_frac=val_frac,
                             split_seed=split_seed)
    res["epochs"] = [info[k]["selected_epochs"] for k in sorted(info)]
    return res


def lbisg_field_scores(F: npt.ArrayLike, geo: npt.ArrayLike,
                       prior: npt.ArrayLike, K: int | None = None,
                       grid: Sequence[int] | None = None,
                       geo_folds_J: int = 5, verbose: bool = True,
                       k_method: str = "split", val_frac: float = 0.2,
                       split_seed: int = 0) -> dict[str, Any]:
    """lBISG for one name field from given list scores.

    Clusters the scores, selects K when K is None, and computes the
    posterior. lbisg_field() calls it after fitting the list scores; with
    pre-trained list models it is called on the scores of pretrained_scores().

    Args:
        F: (N, L) list scores per person, in (0, 1).
        geo, prior, K, grid, geo_folds_J, verbose, k_method, val_frac,
            split_seed: see lbisg_field().

    Returns:
        dict with posterior (N, R), K, K_used, Q (list of (K, Q(K)) or None)
        and scores (N, L).
    """
    F = np.asarray(F, float)
    geo = np.asarray(geo, np.int64)
    curve: list[tuple[int, float]] | None = None
    if K is None:
        K, curve = select_K(F, geo, prior, grid=grid, J=int(geo_folds_J),
                            verbose=verbose, method=k_method,
                            val_frac=float(val_frac),
                            split_seed=int(split_seed))
        _log(verbose, f"  selected K = {K}")
    post, _, K_used, _ = fit_posterior(F, geo, prior, int(K))
    return dict(posterior=post, K=int(K), K_used=int(K_used), Q=curve,
                scores=F)


def pretrained_scores(emb_unique: npt.ArrayLike,
                      path: str) -> tuple[Array, list[str]]:
    """List scores from a pre-trained list model fit on the lists alone.

    The checkpoint holds one network per list (D -> 256 -> 128 -> 1, ReLU),
    stored as stacked weights W1 (L, D, 256), b1 (L, 256), W2 (L, 256, 128),
    b2 (L, 128), W3 (L, 128) and b3 (L), with the list identifiers in
    `groups`. The networks share no parameters. Dropout is off at prediction.

    Args:
        emb_unique: (n_names, D) embeddings of the distinct names, from the
            embedding model named in the checkpoint.
        path: path to the .pt checkpoint.

    Returns:
        (scores, groups): the (n_names, L) scores and the list identifiers.
    """
    ck = torch.load(path, map_location="cpu", weights_only=False)
    s = {k: v.float() for k, v in ck["state"].items()}
    X = torch.as_tensor(np.asarray(emb_unique, np.float32))
    if X.shape[1] != s["W1"].shape[1]:
        raise ValueError(f"The embeddings have {X.shape[1]} dimensions; the "
                         f"model expects {s['W1'].shape[1]}.")
    out = []
    with torch.no_grad():
        for i in range(0, len(X), 4096):
            x = X[i:i + 4096]
            h = torch.relu(torch.einsum("bd,ldh->blh", x, s["W1"]) + s["b1"])
            h = torch.relu(torch.einsum("blh,lhk->blk", h, s["W2"]) + s["b2"])
            out.append(torch.sigmoid(
                torch.einsum("blk,lk->bl", h, s["W3"]) + s["b3"]).numpy())
    scores = (np.concatenate(out) if out
              else np.zeros((0, s["b3"].shape[0]), np.float32))
    return scores, [str(g) for g in ck["groups"]]


def combine_fields(posteriors: Sequence[npt.ArrayLike],
                   prior: npt.ArrayLike) -> Array:
    """Combine the posteriors of several name fields.

    P(R | B_F, B_S, G) is proportional to
    P(R | B_F, G) * P(R | B_S, G) / P(R | G), assuming that the clusters of
    the two fields are independent given R and G.

    Args:
        posteriors: list of (N, R) posteriors, one per name field.
        prior: (N, R) P(R = r | G) per person.

    Returns:
        (N, R) combined posterior, with rows that sum to one.
    """
    prior = np.asarray(prior, float)
    out = np.ones_like(prior)
    for p in posteriors:
        out = out * np.asarray(p, float)
    out = out / np.maximum(prior, 1e-300) ** (len(posteriors) - 1)
    out[prior <= 0] = 0
    return out / np.maximum(out.sum(1, keepdims=True), 1e-300)


# ---------------------------------------------------------------------------
# Estimating the geographic prior from the lists
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


def _normalize_to(theta: Array, target: Array) -> Array:
    """Scale the shares in each geography to sum to `target`.

    Geographies in which each estimate is zero are assigned the overall mix of
    the block.
    """
    total = theta.sum(1)
    positive = total > 1e-12
    if positive.any():
        mix = theta[positive].sum(0) / max(theta[positive].sum(), 1e-12)
    else:
        mix = np.full(theta.shape[1], 1 / theta.shape[1])
    theta = np.where(positive[:, None],
                     theta / np.maximum(total, 1e-12)[:, None],
                     mix[None, :])
    return theta * target[:, None]


def _check_coverage(pi: Array, a: Array, lam: float) -> None:
    """Warn when an estimated coverage is not in (lam, 1].

    A coverage outside this range means that the assumptions of the recovery
    do not hold.
    """
    bad = ~np.isfinite(pi) | (a <= 0) | (pi <= lam) | (pi > 1 + 1e-9)
    if bad.any():
        warnings.warn(
            f"estimated coverage outside (lambda, 1] for lists "
            f"{np.flatnonzero(bad).tolist()}: the assumptions of the "
            f"prevalence recovery do not hold at lambda = {lam}",
            stacklevel=3)


def _recover_block(rho: Array, target: Array, weights: Array,
                   lam: float = 0.0) -> tuple[Array, Array]:
    """Estimate the shares of the groups in one block.

    A block is a coarse group, or the whole population. With lam = 0, the
    inverse coverages 1 / pi are fit by non-negative least squares. With
    lam > 0, members of each group are assumed to have a name on each of the
    other lists in the block at rate lam. The shares are then B Pi^{-1}, with
    negative shares set to zero. Finally, the shares are scaled to sum to
    `target` in each geography.

    Args:
        rho: (G, Rc) list rates of the groups in the block.
        target: (G,) total share of the block in each geography.
        weights: (G,) weight of each geography.
        lam: assumed cross-group list rate within the block.

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
        with np.errstate(divide="ignore", invalid="ignore"):
            pi = lam + (1 - lam * a.sum()) / a
        _check_coverage(pi, a, lam)
        Pi = np.diag(pi - lam) + lam * np.ones((len(pi), len(pi)))
        theta = np.maximum(rho @ np.linalg.inv(Pi), 0)
        coverage = pi
    return _normalize_to(theta, target), coverage


def _recover_joint(rho: Array, Mc: Array, coarse_of: IntArray,
                   weights: Array, lam: float,
                   gamma: float) -> tuple[Array, Array]:
    """Estimate subgroup shares when lists also overlap across coarse groups.

    Members of a group are assumed to have a name on the list of another
    group at rate lam within their coarse group and gamma outside it. The
    coverages and the shares B Pi^{-1} are solved for all groups together,
    with negative shares set to zero. The shares in each coarse group are then
    scaled to the share of that coarse group.

    Args:
        rho: (G, L) list rates, after the contribution of the coarse groups
            without a list is subtracted.
        Mc: (G, C) shares of the coarse groups with at least one list.
        coarse_of: (L,) 0-based column of Mc for the group of each list.
        weights: (G,) weight of each geography.
        lam: cross-group list rate within a coarse group.
        gamma: cross-group list rate across coarse groups.

    Returns:
        (Theta, coverage): the (G, L) recovered shares and the estimated
        coverage of each list.
    """
    n_c = Mc.shape[1]
    sw = np.sqrt(np.maximum(weights, 0))
    A = np.linalg.lstsq(rho * sw[:, None], Mc * sw[:, None], rcond=None)[0]
    a = A.sum(1)
    U = np.eye(n_c)[coarse_of]
    Cm = (lam - gamma) * np.eye(n_c) + gamma * np.ones((n_c, n_c))
    num = 1 - Cm @ (U.T @ a)
    with np.errstate(divide="ignore", invalid="ignore"):
        pi = lam + num[coarse_of] / a
    _check_coverage(pi, a, lam)
    Pi = np.diag(pi - lam) + U @ Cm @ U.T
    theta = np.maximum(rho @ np.linalg.inv(Pi), 0)
    for c in range(n_c):
        cols = np.flatnonzero(coarse_of == c)
        theta[:, cols] = _normalize_to(theta[:, cols], Mc[:, c])
    return theta, pi


def recover_prevalence(
        onmat: npt.ArrayLike, geo: npt.ArrayLike,
        coarse_prior: npt.ArrayLike | None = None,
        coarse_of: npt.ArrayLike | None = None, lam: float = 0.0,
        gamma: float = 0.0,
) -> tuple[Array, Array]:
    """Estimate the share of each group in each geography from the list rates.

    Without a coarse prior, the lists must cover the whole population, and the
    shares in each geography sum to one. With a coarse prior, the shares of the
    subgroups of a coarse group sum to the share of that coarse group, and a
    coarse group with a single subgroup retains the share of that coarse group.
    `lam` and
    `gamma` are the assumed rates at which members of a group have a name on
    the list of another group, within and across coarse groups. With
    gamma > 0, all groups are solved together, and members of a coarse group
    without a list are assumed to have a name on each list at rate gamma.

    Args:
        onmat: (N, L) 0/1 list membership per person.
        geo: (N,) 0-based geography of each person.
        coarse_prior: optional (G, C) coarse shares P(C = c | G) per
            geography.
        coarse_of: (L,) 0-based coarse group of the group of each list.
        lam: assumed cross-group list rate (0 for exclusive lists).
        gamma: assumed list rate across coarse groups (coarse prior only).

    Returns:
        (Theta, coverage): the (G, L) recovered shares per geography and the
        estimated coverage of each list.
    """
    if not 0 <= lam < 1:
        raise ValueError("lambda must be in [0, 1)")
    if not 0 <= gamma <= lam:
        raise ValueError("gamma must be between 0 and lambda")
    onmat = np.asarray(onmat, float)
    geo = np.asarray(geo, np.int64)
    n_geo = int(geo.max()) + 1
    rho, n = _list_rates(onmat, geo, n_geo)
    if coarse_prior is None:
        if gamma > 0:
            raise ValueError("gamma requires a coarse prior")
        return _recover_block(rho, np.ones(n_geo), n, lam)

    Mc = np.asarray(coarse_prior, float)
    if coarse_of is None:
        raise ValueError("coarse_prior requires coarse_of")
    coarse_of = np.asarray(coarse_of, np.int64)
    if gamma > 0:
        listed = np.unique(coarse_of)
        unlisted = np.setdiff1d(np.arange(Mc.shape[1]), listed)
        rho_net = rho - gamma * Mc[:, unlisted].sum(1, keepdims=True)
        return _recover_joint(rho_net, Mc[:, listed],
                              np.searchsorted(listed, coarse_of), n, lam,
                              gamma)

    theta = np.zeros((n_geo, onmat.shape[1]))
    coverage = np.full(onmat.shape[1], np.nan)
    for c in np.unique(coarse_of):
        cols = np.flatnonzero(coarse_of == c)
        if len(cols) == 1:
            # A coarse group with a single subgroup retains the share of that
            # coarse group,
            # and the coverage of that list remains NaN.
            theta[:, cols[0]] = Mc[:, c]
            continue
        theta[:, cols], coverage[cols] = _recover_block(
            rho[:, cols], Mc[:, c], n * Mc[:, c], lam)
    return theta, coverage


# ---------------------------------------------------------------------------
# List quality
# ---------------------------------------------------------------------------
def list_quality(onmat: npt.ArrayLike, geo: npt.ArrayLike,
                 prior: npt.ArrayLike,
                 list_group: npt.ArrayLike | None = None,
                 min_geo_n: int = 1) -> dict[str, Any]:
    """How well the lists separate the groups, estimated without labels.

    Pi[r, l] = P(L_l = 1 | R = r) is fit by least squares on
    P(L_l = 1 | G) = sum_r Pi[r, l] P(R = r | G), with each geography weighted
    by the number of people in that geography. The estimates are then clipped
    to [0, 1].

    Args:
        onmat: (N, L) 0/1 list membership per person.
        geo: (N,) 0-based geography of each person.
        prior: (N, R) P(R = r | G) per person, with rows that sum to one.
        list_group: (L,) 0-based group that corresponds to each list (by
            default, list l corresponds to group l).
        min_geo_n: geographies with fewer than min_geo_n people are excluded
            from the least squares fit of Pi.

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

    # D: for each group, the rate on the list of that group minus the mean rate
    # on the other lists, averaged over groups. With one list and two groups, D
    # is pi_1 - pi_0.
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
