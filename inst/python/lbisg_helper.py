"""
lBISG (list-powered BISG) engine for wru, following Chasalow, Dasanaike and Imai.

Algorithm 1 (lBISG)
  Step 2      the distinct names are randomly partitioned into J folds with approximately equal numbers of
              people (each name, in random order, goes to the fold with the fewest people so far);
  Steps 3-6   for each fold j, a separate predictive model for membership in each list is fit on the names
              outside fold j and scores the names in fold j, so a name's score never sees its own label.
              The models are neural networks on the name embedding; the number of training epochs is chosen
              by cross-validation within the training data (the other J-1 folds held out in turn) before the
              model is fit to all the training data;
  Step 7      K-means on the log-odds list scores, clipped at 1e-6;
  Step 8      cluster proportions within each geography;
  Step 9      weighted least squares with W = diag(n_g); negative estimates set to zero and renormalized;
  Step 10     posterior proportional to P(B = b | R = r) P(R = r | G = g).
Algorithm 2   K is chosen by the held-out log-likelihood ratio Q(K) over J population-balanced folds of the
              geographic units; the candidate grid is extended upward while the maximum sits at its top.
Section 3.2   geographic prevalence recovered from list rates when no prevalence is available (exhaustive,
              exclusive lists; lambda >= 0 for the sensitivity analysis of Appendix C).
Section 3.3   subgroup prevalence recovered within each coarse group from the coarse prevalence (non-negative
              least squares weighted by the expected number of coarse group members, then normalized to the
              coarse share).
Section 4.3   list quality Pi_{r,r'} = P(L_{r'} = 1 | R = r) recovered from the geographic system, its diagonal
              dominance D(Pi), and the implied coverage and precision of each list.
Section 2.5   first-name and surname posteriors are combined as P_F P_S / P(R | G).

Numerical details that are not in the paper (as in the reference implementation): Step 9 adds 1e-9 to the
diagonal of M'WM, and "set negative estimates to zero" is implemented as a floor of 1e-9 so that Phat(B | G) is
never exactly zero; K-means centres with no member among the fitting observations are dropped, so every cluster
used has members.
"""
import numpy as np
from scipy import sparse
from scipy.optimize import nnls

EPS_Z, FLOOR, RIDGE = 1e-6, 1e-9, 1e-9
DEFAULT_GRID = [10, 15, 20, 30, 50, 75, 100, 150, 200, 300, 450, 600, 999, 1500, 2000, 3000, 5000, 7500,
                10000, 15000, 20000]


def _log(verbose, *a):
    if verbose:
        print(*a, flush=True)


# ----------------------------------------------------------------------------------------------------------
# Algorithm 1, Steps 2-6: out-of-fold list scores
# ----------------------------------------------------------------------------------------------------------
def name_folds(counts, J=5, seed=2026):
    """Step 2: random partition of the distinct names into J folds with approximately equal numbers of people."""
    counts = np.asarray(counts, np.float64); n = len(counts)
    order = np.random.default_rng(seed).permutation(n)
    load = np.zeros(J); fold = np.empty(n, np.int64)
    for i in order:
        f = int(np.argmin(load)); fold[i] = f; load[f] += counts[i]
    return fold


def _net(d, hidden=(256, 128), dropout=0.3):
    import torch.nn as nn
    layers, prev = [], d
    for h in hidden:
        layers += [nn.Linear(prev, h), nn.ReLU(), nn.Dropout(dropout)]; prev = h
    layers.append(nn.Linear(prev, 1))
    return nn.Sequential(*layers)


class _PerList:
    """One separate network per list (Step 4 fits a model separately for membership in each list)."""

    def __init__(self, d, k, seed, dev):
        import torch
        torch.manual_seed(seed)
        self.nets = [_net(d).to(dev) for _ in range(k)]
        self.opts = [torch.optim.Adam(n.parameters(), 1e-3, weight_decay=1e-5) for n in self.nets]

    def train_epoch(self, X, Y, W, idx, rng, batch, dev):
        import torch
        bce = torch.nn.BCEWithLogitsLoss(reduction="none")
        for n in self.nets:
            n.train()
        perm = idx[rng.permutation(len(idx))]
        for s0 in range(0, len(perm), batch):
            i = torch.as_tensor(perm[s0:s0 + batch], device=dev)
            xb, wb = X[i], W[i]
            for k, (n, o) in enumerate(zip(self.nets, self.opts)):
                o.zero_grad(set_to_none=True)
                (bce(n(xb)[:, 0], Y[i, k]) * wb).mean().backward(); o.step()

    def predict(self, X, idx, dev, logits=False):
        import torch
        out = np.zeros((len(idx), len(self.nets)), np.float32)
        with torch.no_grad():
            for n in self.nets:
                n.eval()
            for s0 in range(0, len(idx), 16384):
                j = torch.as_tensor(idx[s0:s0 + 16384], device=dev)
                z = torch.cat([n(X[j]) for n in self.nets], 1)
                out[s0:s0 + len(j)] = (z if logits else torch.sigmoid(z)).cpu().numpy()
        return out

    def heldout_bce(self, X, Y, W, idx, dev):
        import torch
        bce = torch.nn.BCEWithLogitsLoss(reduction="none")
        num = den = 0.0
        with torch.no_grad():
            for n in self.nets:
                n.eval()
            for s0 in range(0, len(idx), 16384):
                j = torch.as_tensor(idx[s0:s0 + 16384], device=dev)
                z = torch.cat([n(X[j]) for n in self.nets], 1)
                w = W[j]
                num += float((bce(z, Y[j]).mean(1) * w).sum()); den += float(w.sum())
        return num / max(den, 1e-12)


def list_scores_oof(emb, onmat, counts, J=5, max_epochs=100, patience=5, batch_size=512, seed=42,
                    fold_seed=2026, verbose=True):
    """Algorithm 1 Steps 2-6 on the distinct names.

    emb    (n_names, D) name embeddings, onmat (n_names, L) 0/1 list membership, counts (n_names,) people per name.
    Each row is weighted by the number of people carrying the name, so the fit is the same as fitting on people.
    For each outer fold k, the inner folds are the other J-1 name folds, each held out in turn; the inner models
    are trained in lockstep and the number of epochs is the minimum of their mean held-out weighted binary
    cross-entropy (training stops once the mean has not improved for `patience` epochs, or at `max_epochs`).
    One model is then fit on all the training folds for that many epochs and scores the names in fold k.
    Returns (scores (n_names, L), info)."""
    import torch
    dev = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    X = torch.as_tensor(np.ascontiguousarray(emb, np.float32), device=dev)
    Y = torch.as_tensor(np.ascontiguousarray(onmat, np.float32), device=dev)
    W = torch.as_tensor(np.maximum(np.asarray(counts, np.float32), 1e-4), device=dev)
    fold = name_folds(counts, J, fold_seed)
    D, L = X.shape[1], Y.shape[1]
    out = np.full((len(fold), L), np.nan, np.float32); info = {}
    for k in range(J):
        inner = [j for j in range(J) if j != k]
        models = [(_PerList(D, L, seed + 10 * k + j, dev), np.random.RandomState(seed + 10 * k + j),
                   np.flatnonzero((fold != k) & (fold != j)), np.flatnonzero(fold == j)) for j in inner]
        curve, best, best_ep = [], np.inf, 0
        for ep in range(1, max_epochs + 1):
            vals = []
            for m, rng, tr, va in models:
                m.train_epoch(X, Y, W, tr, rng, batch_size, dev); vals.append(m.heldout_bce(X, Y, W, va, dev))
            curve.append(float(np.mean(vals)))
            if curve[-1] < best:
                best, best_ep = curve[-1], ep
            elif ep - best_ep >= patience:
                break
        del models
        tr = np.flatnonzero(fold != k); te = np.flatnonzero(fold == k)
        m = _PerList(D, L, seed + k, dev); rng = np.random.RandomState(seed + k)
        for _ in range(best_ep):
            m.train_epoch(X, Y, W, tr, rng, batch_size, dev)
        out[te] = m.predict(X, te, dev)
        info[k] = dict(selected_epochs=best_ep, inner_mean_bce=curve)
        _log(verbose, f"  list scores: fold {k + 1} of {J}, {best_ep} epochs")
        del m
        if dev.type == "cuda":
            torch.cuda.empty_cache()
    assert np.isfinite(out).all()
    return out, info


# ----------------------------------------------------------------------------------------------------------
# Algorithm 1, Steps 7-10, and Algorithm 2
# ----------------------------------------------------------------------------------------------------------
def logodds(F):
    F = np.clip(np.asarray(F, float), EPS_Z, 1 - EPS_Z)
    return np.log(F) - np.log(1 - F)


def kmeans_occupied(Z, rows, K, seed=0):
    """Step 7: K-means with K centres fit on the observations `rows`; centres no observation in `rows` is nearest
    to are dropped and every observation is assigned to the nearest remaining centre. Returns (labels, K used)."""
    from sklearn.cluster import MiniBatchKMeans
    from sklearn.metrics import pairwise_distances_argmin
    u, inv = np.unique(Z, axis=0, return_inverse=True); inv = np.asarray(inv).ravel()
    km = MiniBatchKMeans(K, random_state=seed, n_init=3, compute_labels=False).fit(Z[rows])
    lab_u = pairwise_distances_argmin(u, km.cluster_centers_)
    kept = np.unique(lab_u[inv[rows]])
    lab_u = pairwise_distances_argmin(u, km.cluster_centers_[kept])
    return lab_u[inv], len(kept)


def _geo_setup(geo, prior):
    geo = np.asarray(geo, np.int64); prior = np.asarray(prior, float); ng = int(geo.max()) + 1
    w = np.bincount(geo, minlength=ng).astype(float)
    M = np.column_stack([np.bincount(geo, weights=prior[:, r], minlength=ng) / np.maximum(w, 1)
                         for r in range(prior.shape[1])])
    return geo, prior, ng, w, M


def wls_pi(C, M, w, take):
    """Step 9 on the geographies with take = 1: R x K matrix of P(B = b | R = r)."""
    R = M.shape[1]
    XtWX = M.T @ ((w * take)[:, None] * M); XtWY = np.asarray(C.T @ (M * take[:, None])).T
    Pi = np.maximum(np.linalg.solve(XtWX + RIDGE * np.eye(R), XtWY), FLOOR)
    return Pi / Pi.sum(1, keepdims=True)


def geo_folds(geo, J):
    """Algorithm 2 Step 1: J folds of geographic units with approximately equal numbers of people."""
    ng = int(geo.max()) + 1; w = np.bincount(geo, minlength=ng).astype(float)
    fold = np.zeros(ng, int); fold[np.argsort(-w, kind="stable")] = np.arange(ng) % J
    return fold


def fold_term(Z, geo, M, w, ng, K, j, fold, seed=0):
    ofold = fold[geo]; tr = np.flatnonzero(ofold != j); te = np.flatnonzero(ofold == j)
    bins, Ku = kmeans_occupied(Z, tr, K, seed)
    C = sparse.coo_matrix((np.ones(len(tr)), (geo[tr], bins[tr])), shape=(ng, Ku)).tocsr()
    Pi = wls_pi(C, M, w, (fold != j).astype(float))
    p_bg = np.einsum("ir,ri->i", M[geo[te]], Pi[:, bins[te]])
    p_b = np.bincount(bins[tr], minlength=Ku).astype(float) / len(tr)
    return float((np.log(p_bg) - np.log(p_b[bins[te]])).sum()), Ku


def select_K(F, geo, prior, grid=None, J=5, seed=0, extend=True, verbose=True):
    """Algorithm 2. F (N, L) list scores per person, prior (N, R) P(R | G) per person (rows sum to one).
    The candidate grid is restricted to R <= K < number of distinct score vectors among the training
    observations; while the maximum sits at the top of the grid it is extended upward by a factor of 1.5.
    Returns (K*, [(K, Q(K)), ...])."""
    Z = logodds(F); geo, prior, ng, w, M = _geo_setup(geo, prior); R = M.shape[1]
    fold = geo_folds(geo, J)
    if len(np.unique(fold)) < J:
        raise ValueError(f"Algorithm 2 needs at least J = {J} geographic units")
    limit = min(len(np.unique(Z[fold[geo] != j], axis=0)) for j in range(J)) - 1
    grid = sorted({int(k) for k in (grid or DEFAULT_GRID) if R <= int(k) <= limit})
    if not grid:
        grid = [max(R, min(limit, 10))]
    curve, done = [], set()

    def run(ks):
        for K in ks:
            if K in done:
                continue
            terms = [fold_term(Z, geo, M, w, ng, K, j, fold, seed) for j in range(J)]
            curve.append((int(K), sum(t[0] for t in terms) / len(Z))); done.add(K)
            _log(verbose, f"  Q(K = {K}) = {curve[-1][1]:.6f}")

    run(grid)
    best = max(curve, key=lambda a: a[1])[0]
    while extend and best == max(k for k, _ in curve) and best < limit:
        new = min(int(best * 1.5), limit)
        if new <= best:
            break
        run([new]); best = max(curve, key=lambda a: a[1])[0]
    return best, sorted(curve)


def fit_posterior(F, geo, prior, K, seed=0):
    """Algorithm 1 Steps 7-10 on all observations: (posterior (N, R), clusters, K used, P(B | R))."""
    Z = logodds(F); geo, prior, ng, w, M = _geo_setup(geo, prior)
    bins, Ku = kmeans_occupied(Z, np.arange(len(Z)), K, seed)
    C = sparse.coo_matrix((np.ones(len(Z)), (geo, bins)), shape=(ng, Ku)).tocsr()
    Pi = wls_pi(C, M, w, np.ones(ng))
    post = prior * Pi[:, bins].T
    return post / np.maximum(post.sum(1, keepdims=True), 1e-300), bins, Ku, Pi


def lbisg_field(emb_unique, idx, onmat_unique, geo, prior, K=None, grid=None, n_folds=5, geo_folds_J=5,
                max_epochs=100, patience=5, batch_size=512, seed=42, verbose=True):
    """Algorithm 1 (with K from Algorithm 2 when K is None) for one name field.
    emb_unique (n_names, D), idx (N,) 0-based name index per person, onmat_unique (n_names, L) list membership,
    geo (N,) 0-based geography, prior (N, R) P(R | G) with rows summing to one.
    Returns dict(posterior, K, Q, scores (N, L), epochs)."""
    idx = np.asarray(idx, np.int64); geo = np.asarray(geo, np.int64)
    counts = np.bincount(idx, minlength=len(emb_unique)).astype(float)
    used = np.flatnonzero(counts > 0)
    remap = np.full(len(emb_unique), -1, np.int64); remap[used] = np.arange(len(used))
    _log(verbose, f"  fitting list scores on {len(used)} distinct names")
    S, info = list_scores_oof(np.asarray(emb_unique)[used], np.asarray(onmat_unique)[used], counts[used],
                              J=int(n_folds), max_epochs=int(max_epochs), patience=int(patience),
                              batch_size=int(batch_size), seed=int(seed), verbose=verbose)
    F = S[remap[idx]]
    curve = None
    if K is None:
        K, curve = select_K(F, geo, prior, grid=grid, J=int(geo_folds_J), verbose=verbose)
        _log(verbose, f"  selected K = {K}")
    post, bins, Ku, Pi = fit_posterior(F, geo, prior, int(K))
    return dict(posterior=post, K=int(K), K_used=int(Ku), Q=curve, scores=F,
                epochs=[info[k]["selected_epochs"] for k in sorted(info)])


def combine_fields(post_list, prior):
    """Section 2.5: P(R | B_F, B_S, G) proportional to P(R | B_F, G) P(R | B_S, G) / P(R | G)."""
    prior = np.asarray(prior, float)
    out = np.ones_like(prior)
    for p in post_list:
        out = out * np.asarray(p, float)
    out = out / np.maximum(prior, 1e-300) ** (len(post_list) - 1)
    out[prior <= 0] = 0
    return out / np.maximum(out.sum(1, keepdims=True), 1e-300)


# ----------------------------------------------------------------------------------------------------------
# Section 3: recovering geographic prevalence
# ----------------------------------------------------------------------------------------------------------
def _list_rates(onmat, geo, ng):
    w = np.bincount(geo, minlength=ng).astype(float)
    rho = np.column_stack([np.bincount(geo, weights=onmat[:, r], minlength=ng) for r in range(onmat.shape[1])])
    return rho / np.maximum(w, 1)[:, None], w


def _recover_block(rho, target, wts, lam=0.0):
    """Recover Theta (G x Rc) with Theta 1 = target from list rates rho (G x Rc) within one block.
    lambda = 0: non-negative least squares on u = 1 / pi (Appendix C.3), weighted by wts.
    lambda > 0: the homogeneous non-separation solution (Appendix C.1/C.2): a = (B'WB)^{-1} B'W target,
    pi_r = lambda + (1 - lambda 1'a) / a_r, Theta = B Pi^{-1}. Negative entries are set to zero.
    Rows are then normalized to `target`; rows whose estimates are all zero get the overall mix."""
    sw = np.sqrt(np.maximum(wts, 0))
    if lam == 0:
        u, _ = nnls(rho * sw[:, None], target * sw)
        Th = rho * u[None, :]; cover = np.where(u > 0, 1 / np.maximum(u, 1e-300), np.nan)
    else:
        Bw = rho * sw[:, None]
        a = np.linalg.lstsq(Bw, target * sw, rcond=None)[0]
        pi = lam + (1 - lam * a.sum()) / a
        Pm = np.diag(pi - lam) + lam * np.ones((len(pi), len(pi)))
        Th = np.maximum(rho @ np.linalg.inv(Pm), 0); cover = pi
    tot = Th.sum(1); pos = tot > 1e-12
    mix = Th[pos].sum(0) / max(Th[pos].sum(), 1e-12) if pos.any() else np.full(Th.shape[1], 1 / Th.shape[1])
    Th = np.where(pos[:, None], Th / np.maximum(tot, 1e-12)[:, None], mix[None, :]) * target[:, None]
    return Th, cover


def recover_prevalence(onmat, geo, coarse_prior=None, coarse_of=None, lam=0.0):
    """Section 3.2 (coarse_prior None): P(R | G) from exhaustive, exclusive lists, with target 1 per geography
    and weights n_g. Section 3.3: coarse_prior (G x C) is P(C | G) per geography, coarse_of (L,) the 0-based coarse
    group of each list's group; within each coarse group with more than one subgroup the subgroup shares are
    recovered with weights n_g P(C = c | G = g); a coarse group with one subgroup keeps its coarse share.
    onmat (N, L) list membership per person, geo (N,) 0-based geography.
    Returns (Theta (G x L) per geography, coverage estimates (L,))."""
    onmat = np.asarray(onmat, float); geo = np.asarray(geo, np.int64); ng = int(geo.max()) + 1; L = onmat.shape[1]
    rho, n = _list_rates(onmat, geo, ng)
    if coarse_prior is None:
        return _recover_block(rho, np.ones(ng), n, lam)
    Mc = np.asarray(coarse_prior, float); coarse_of = np.asarray(coarse_of, np.int64)
    Th = np.zeros((ng, L)); cover = np.full(L, np.nan)
    for c in np.unique(coarse_of):
        cols = np.flatnonzero(coarse_of == c)
        if len(cols) == 1:                                            # nothing to recover: the coarse share
            Th[:, cols[0]] = Mc[:, c]; cover[cols[0]] = rho[:, cols[0]].dot(n) / max((Mc[:, c] * n).sum(), 1e-300)
            continue
        Th[:, cols], cover[cols] = _recover_block(rho[:, cols], Mc[:, c], n * Mc[:, c], lam)
    return Th, cover


# ----------------------------------------------------------------------------------------------------------
# Section 4.3: list quality when geographic prevalence is available
# ----------------------------------------------------------------------------------------------------------
def list_quality(onmat, geo, prior, list_group=None, min_geo_n=1):
    """Pi (R x L) with Pi[r, l] = P(L_l = 1 | R = r), solved from P(L_l = 1 | G) = sum_r Pi[r, l] P(R = r | G)
    by least squares weighted by the people per geography, clipped to [0, 1]. list_group (L,) gives the 0-based
    group each list targets (default: list l targets group l). Returns dict(Pi, D, coverage, precision, q)."""
    onmat = np.asarray(onmat, float); geo, prior, ng, w, M = _geo_setup(geo, prior)
    rho, _ = _list_rates(onmat, geo, ng)
    keep = w >= max(min_geo_n, 1)
    sw = np.sqrt(w[keep])
    Pi = np.clip(np.linalg.lstsq(M[keep] * sw[:, None], rho[keep] * sw[:, None], rcond=None)[0], 0, 1)
    L = onmat.shape[1]
    lg = np.arange(L) if list_group is None else np.asarray(list_group, np.int64)
    q = (M * w[:, None]).sum(0) / w.sum()
    blk = Pi[lg, :]                                                   # L x L: groups with lists x lists
    D = float(np.mean([blk[i, i] - np.delete(blk[i], i).mean() for i in range(L)])) if L > 1 else float(blk[0, 0])
    if L == 1 and M.shape[1] == 2:                                    # binary case: pi_1 - pi_0
        D = float(Pi[lg[0], 0] - Pi[1 - lg[0], 0])
    coverage = np.array([Pi[lg[l], l] for l in range(L)])
    precision = np.array([q[lg[l]] * Pi[lg[l], l] / max((q * Pi[:, l]).sum(), 1e-300) for l in range(L)])
    return dict(Pi=Pi, D=D, coverage=coverage, precision=precision, q=q)
