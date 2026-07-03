"""
lBISG (list-powered BISG) helper: canonical proxy-BISG engine + list-membership V-model.

This is the reference implementation from the paper (Chasalow, Dasanaike & Imai), ported into wru.
Two stages, both label-free (the only labels used are list membership, a function of the name):

  1. V-model  : a small MLP maps the E5 surname embedding to soft list-membership scores
                f_r = P(L_r = 1 | V), trained out-of-fold with 1/L-weighted targets.
  2. route-B binned BISG (`binned_bisg`, assembly="jointbin"): cluster the logit scores into one
     composite bin B = b(V), recover P(B=b | R) for all K+1 classes from the geographic system
     P(B=b | G) = sum_r P(B=b | R=r) P(R=r | G) by weighted least squares over geographic units,
     and normalize once against a leakage-free prior P(R | G):  p_r  propto  P(R=r|G) P(B=b|R=r).

The prior P(R|G) must be EXTERNAL (census) or leave-one-out, never the in-sample race crosstab.
Embeddings are produced separately by ebisg_helper.embed_names (shared E5 infrastructure).
"""
import numpy as np
from scipy import sparse

DEFAULT_Q = (0, .5, .75, .9, .96, .99, .995, 1.0)


# ----------------------------------------------------------------------------
# Canonical proxy-BISG engine (verbatim from scripts/proxybisg.py)
# ----------------------------------------------------------------------------
def quantile_bins(f, q=DEFAULT_Q):
    """Coarsen a name-only score into quantile bins. Returns (bin_index, n_bins)."""
    edges = np.unique(np.quantile(f, q))
    return np.clip(np.searchsorted(edges, f, side="right") - 1, 0, len(edges) - 2), len(edges) - 1


def _coarsen_likelihoods(bin_i, n_bins, geo_idx, n_units, share_unit, fit_mask):
    """Step 2 (one-vs-rest): regress per-unit bin rate on the unit group-share over FIT units ->
    b0[b]=P(B=b|R!=r), b1[b]=P(B=b|R=r). Closed-form weighted least squares, vectorized over bins."""
    inf = fit_mask
    C = sparse.coo_matrix((np.ones(inf.sum()), (geo_idx[inf], bin_i[inf])), shape=(n_units, n_bins)).tocsr()
    w = np.bincount(geo_idx[inf], minlength=n_units).astype(float)
    sw = w.sum()
    xbar = (w * share_unit).sum() / sw
    varx = (w * (share_unit - xbar) ** 2).sum() / sw
    ybar = np.asarray(C.sum(0)).ravel() / sw
    slope = (C.T.dot(share_unit - xbar) / sw) / max(varx, 1e-12)
    b0 = np.clip(ybar - slope * xbar, 1e-12, None)
    b1 = np.clip(ybar + slope * (1 - xbar), 1e-12, None)
    return b0, b1


def binned_bisg(F, geo_idx, p_rg, n_units=None, crossfit_fold=None, q=DEFAULT_Q, return_raw=False,
                assembly="jointbin", M=50):
    """
    F            : (N, K) name-only list scores f_r = P(L_r=1 | V)
    geo_idx      : (N,)   integer geographic unit per person
    p_rg         : (N, K) leakage-free P(R=r | G) per person (external census or leave-one-out)
    crossfit_fold: (N,) in {0,1}, assigned by geographic UNIT, for honest P(B|R); None = no cross-fit
    assembly     : "jointbin" (CANONICAL, route B, paper Eq. 5) or "legacy" (deprecated one-vs-rest).
    returns      : (N, K+1) P(R | V, G), last column = "other" residual
    """
    if not return_raw and assembly == "jointbin":
        return binned_bisg_jointbin(F, geo_idx, p_rg, n_units=n_units, crossfit_fold=crossfit_fold, M=M)
    F = np.asarray(F, float); p_rg = np.asarray(p_rg, float); N, K = F.shape
    if n_units is None: n_units = int(geo_idx.max()) + 1
    if crossfit_fold is None: crossfit_fold = np.zeros(N, int)
    unit_n = np.bincount(geo_idx, minlength=n_units).astype(float)
    P = np.zeros((N, K))
    folds = np.unique(crossfit_fold)
    for r in range(K):
        bin_i, nb = quantile_bins(F[:, r], q)
        share_unit = np.bincount(geo_idx, weights=p_rg[:, r], minlength=n_units) / np.maximum(unit_n, 1)
        for fl in folds:
            fit = crossfit_fold != fl if len(folds) > 1 else np.ones(N, bool)
            ap = crossfit_fold == fl
            b0, b1 = _coarsen_likelihoods(bin_i, nb, geo_idx, n_units, share_unit, fit)
            l1 = b1[bin_i[ap]]; l0 = b0[bin_i[ap]]; s = p_rg[ap, r]
            P[ap, r] = l1 * s / np.maximum(l1 * s + l0 * (1 - s), 1e-12)
    if return_raw:
        return P
    oth = np.clip(1 - P.sum(1), 0, 1)
    Mx = np.column_stack([P, oth])
    return Mx / np.maximum(Mx.sum(1, keepdims=True), 1e-8)


def binned_bisg_jointbin(F, geo_idx, p_rg, n_units=None, crossfit_fold=None, M=50, seed=0, bins=None):
    """Route (B), CANONICAL: coherent normalization with ONE composite bin on the JOINT name scores.
    Cluster the logit score vector (f_1,...,f_K), which is name-only and leakage-free, into M cells B=b(V),
    recover P(B=m | R=r) for the full K+1 classes from Eq. (5) with that single bin, normalize once:
    p_r propto P(R=r|G) P(B=m | R=r). Returns N x (K+1), last column = other."""
    from sklearn.cluster import MiniBatchKMeans
    F = np.asarray(F, float); p_rg = np.asarray(p_rg, float); N = F.shape[0]; Kc = p_rg.shape[1] + 1
    if n_units is None: n_units = int(geo_idx.max()) + 1
    if crossfit_fold is None: crossfit_fold = np.zeros(N, int)
    if bins is not None:
        bin_i = np.asarray(bins); nb = int(bin_i.max()) + 1
    else:
        if n_units < Kc: M = max(2, n_units)                       # too few geographies (rank condition)
        else: M = max(2, min(M, N // 1000))                        # >= ~1000 names/cell
        Z = np.log(np.clip(F, 1e-6, 1 - 1e-6)) - np.log(np.clip(1 - F, 1e-6, 1 - 1e-6))   # logit scores
        bin_i = MiniBatchKMeans(M, random_state=seed, n_init=3).fit_predict(Z); nb = M
    prior = np.column_stack([p_rg, np.clip(1 - p_rg.sum(1), 0, 1)])
    unit_n = np.bincount(geo_idx, minlength=n_units).astype(float)
    Xunit = np.column_stack([np.bincount(geo_idx, weights=prior[:, r], minlength=n_units)
                             / np.maximum(unit_n, 1) for r in range(Kc)])
    lik = np.ones((N, Kc)); folds = np.unique(crossfit_fold)
    for fl in folds:
        fit = (crossfit_fold != fl) if len(folds) > 1 else np.ones(N, bool)
        ap = crossfit_fold == fl
        C = sparse.coo_matrix((np.ones(fit.sum()), (geo_idx[fit], bin_i[fit])), shape=(n_units, nb)).toarray()
        w = C.sum(1); Y = C / np.maximum(w[:, None], 1)
        XtWX = Xunit.T @ (w[:, None] * Xunit); XtWY = Xunit.T @ (w[:, None] * Y)
        B = np.linalg.solve(XtWX + 1e-9 * np.eye(Kc), XtWY)        # Kc x M : P(B=m | R=r)
        B = np.clip(B, 1e-9, None); B = B / B.sum(1, keepdims=True)
        lik[ap] = B[:, bin_i[ap]].T
    post = prior * lik
    return post / np.maximum(post.sum(1, keepdims=True), 1e-12)


def loo_geo_shares(geo_idx, y_onehot, n_units):
    """Leave-one-out P(R=r | G) per person: (unit count of r minus self) / (unit size minus 1).
    y_onehot used ONLY to de-leak the geographic prior; never as a predictor."""
    N, K = y_onehot.shape
    unit_n = np.bincount(geo_idx, minlength=n_units).astype(float)
    out = np.zeros((N, K))
    for r in range(K):
        csum = np.bincount(geo_idx, weights=y_onehot[:, r], minlength=n_units)
        out[:, r] = (csum[geo_idx] - y_onehot[:, r]) / np.maximum(unit_n[geo_idx] - 1, 1)
    return out


def crossfit_by_unit(geo_idx, n_units, seed=42):
    """Assign each geographic unit to fold 0/1 (by size, deterministic) -> per-person fold."""
    un = np.bincount(geo_idx, minlength=n_units)
    fold_u = np.zeros(n_units, int); fold_u[np.argsort(-un)] = np.arange(n_units) % 2
    return fold_u[geo_idx]


def build_list(surname, count, p_group, base=None, floor=0.3, K=1000):
    """CANONICAL list builder (expected-contribution + precision floor). Rank surnames by
    count*(p_group - base) among those with p_group >= floor; take top-K. Returns ordered list."""
    surname = np.asarray(surname); count = np.asarray(count, float); p = np.asarray(p_group, float)
    if base is None: base = (count * p).sum() / max(count.sum(), 1)
    keep = p >= floor
    if keep.sum() == 0: return []
    sc = np.where(keep, count * (p - base), -np.inf)
    k = min(K, int(keep.sum()))
    idx = np.argsort(-sc)[:k]
    return list(surname[idx])


# ----------------------------------------------------------------------------
# V-model: soft list-membership MLP on name embeddings (from scripts/save_fhat.py)
# ----------------------------------------------------------------------------
def _make_net(d, k, hidden=(256, 128), dropout=0.3):
    import torch.nn as nn
    layers, prev = [], d
    for hh in hidden:
        layers += [nn.Linear(prev, hh), nn.ReLU(), nn.Dropout(dropout)]; prev = hh
    layers.append(nn.Linear(prev, k))
    return nn.Sequential(*layers)


def train_vmodel_oof(X, H, n_folds=5, epochs=8, seed=42, hidden=(256, 128), dropout=0.3,
                     lr=1e-3, weight_decay=1e-5, batch=8192):
    """Out-of-fold f_hat = P(L_r=1 | V). X: (N,D) embeddings; H: (N,K) 1/L-weighted list targets.
    Random folds (targets are a function of the name, so no labels are used). Mirrors save_fhat.py."""
    import torch, torch.nn as nn
    from sklearn.model_selection import KFold
    X = np.asarray(X, np.float32); H = np.asarray(H, np.float32)
    N, D = X.shape; K = H.shape[1]
    dev = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    fhat = np.zeros((N, K), np.float32)
    for k, (tr, va) in enumerate(KFold(n_folds, shuffle=True, random_state=seed).split(np.arange(N))):
        torch.manual_seed(seed + k)
        m = _make_net(D, K, hidden, dropout).to(dev)
        opt = torch.optim.Adam(m.parameters(), lr, weight_decay=weight_decay)
        bce = nn.BCEWithLogitsLoss()
        for ep in range(epochs):
            m.train(); perm = tr[np.random.permutation(len(tr))]
            for s0 in range(0, len(tr), batch):
                i = perm[s0:s0 + batch]
                xb = torch.from_numpy(X[i]).to(dev); hb = torch.from_numpy(H[i]).to(dev)
                opt.zero_grad(); bce(m(xb), hb).backward(); opt.step()
        m.eval()
        with torch.no_grad():
            for s0 in range(0, len(va), 16384):
                j = va[s0:s0 + 16384]
                fhat[j] = torch.sigmoid(m(torch.from_numpy(X[j]).to(dev))).cpu().numpy()
        del m
        if dev.type == "cuda":
            torch.cuda.empty_cache()
    return fhat


# ----------------------------------------------------------------------------
# Top-level entry point called from R (predict_race_lbisg)
# ----------------------------------------------------------------------------
def predict_lbisg(emb_unique, idx, onmat, p_rg, geo_idx, epochs=8, n_folds=5, seed=42,
                  route="jointbin", M=50):
    """
    emb_unique : (n_unique, D) E5 embeddings of the unique surnames
    idx        : (N,) 0-based index of each voter's surname into emb_unique
    onmat      : (N, K) list-membership indicators (0/1), one column per listed group
    p_rg       : (N, K) leakage-free external P(R=r | G) for the K listed groups (other = residual)
    geo_idx    : (N,) 0-based contiguous geographic-unit index
    returns    : (N, K+1) posterior P(R | V, G), last column = "other" residual
    """
    X = np.asarray(emb_unique, np.float32)[np.asarray(idx, np.int64)]      # expand to per-voter
    onmat = np.asarray(onmat, np.float32); p_rg = np.asarray(p_rg, float)
    geo_idx = np.asarray(geo_idx, np.int64)
    n_units = int(geo_idx.max()) + 1
    cf = crossfit_by_unit(geo_idx, n_units)
    L = onmat.sum(1, keepdims=True)
    H = np.divide(onmat, L, out=np.zeros_like(onmat), where=L > 0)          # 1/L weighting
    fhat = train_vmodel_oof(X, H, n_folds=n_folds, epochs=epochs, seed=seed)
    return binned_bisg(fhat, geo_idx, p_rg, n_units=n_units, crossfit_fold=cf, assembly=route, M=M)
