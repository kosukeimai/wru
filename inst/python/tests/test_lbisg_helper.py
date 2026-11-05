"""Tests of lbisg_helper: list scores, clustering, the geographic solve,
the downstream estimator and the recovery of geographic prevalence."""
from __future__ import annotations

from typing import Any

import numpy as np
import pytest
import torch
from scipy import sparse

import lbisg_helper as lb
from conftest import GROUP_MEANS


def people_from_shares(shares: np.ndarray, rates: np.ndarray,
                       per_geo: int = 1000) -> tuple[np.ndarray, ...]:
    """People whose group and list shares equal `shares` and `rates` exactly.

    Each geography has `per_geo` people split into groups by `shares`
    (G, R), and within group r the first rates[r, l] of the people are on
    list l. The shares and rates must give whole numbers of people.
    """
    geo, grp, onmat = [], [], []
    for g, row in enumerate(shares):
        for r, s in enumerate(row):
            m = int(round(s * per_geo))
            on = np.zeros((m, rates.shape[1]))
            for col in range(rates.shape[1]):
                on[:int(round(rates[r, col] * m)), col] = 1
            geo += [g] * m
            grp += [r] * m
            onmat.append(on)
    return np.array(geo), np.array(grp), np.vstack(onmat)


# ---------------------------------------------------------------------------
# Algorithm 1, Steps 2-6
# ---------------------------------------------------------------------------
def test_name_folds_balances_people_across_folds() -> None:
    counts = np.random.default_rng(0).integers(1, 50, 500)
    fold = lb.name_folds(counts, J=5, seed=1)
    load = np.bincount(fold, weights=counts, minlength=5)
    assert set(fold) == set(range(5))
    assert load.max() - load.min() <= counts.max()
    np.testing.assert_array_equal(fold, lb.name_folds(counts, J=5, seed=1))


def test_make_net_has_the_requested_layers() -> None:
    net = lb._make_net(8, hidden_dims=(6, 4))
    linear = [m for m in net if isinstance(m, torch.nn.Linear)]
    assert [(m.in_features, m.out_features) for m in linear] == [
        (8, 6), (6, 4), (4, 1)]
    assert net(torch.zeros(3, 8)).shape == (3, 1)


def separable_names(n: int = 400, seed: int = 0) -> tuple[np.ndarray, ...]:
    """Embeddings whose first two coordinates decide membership in two lists."""
    rng = np.random.default_rng(seed)
    emb = rng.normal(size=(n, 4)).astype(np.float32)
    onmat = np.column_stack([emb[:, 0] > 0, emb[:, 1] > 0.5]).astype(float)
    counts = rng.integers(1, 10, n).astype(float)
    return emb, onmat, counts


def test_list_scores_oof_learns_the_lists() -> None:
    emb, onmat, counts = separable_names()
    scores, info = lb.list_scores_oof(emb, onmat, counts, J=3, max_epochs=8,
                                      patience=3, verbose=False)
    assert scores.shape == onmat.shape
    assert np.all((scores > 0) & (scores < 1))
    assert sorted(info) == [0, 1, 2]
    assert all(1 <= info[k]["selected_epochs"] <= 8 for k in info)
    for col in range(2):
        on = onmat[:, col] == 1
        assert scores[on, col].mean() > scores[~on, col].mean() + 0.2


def test_list_scores_oof_never_sees_the_scored_names_labels() -> None:
    """Changing the labels in fold 0 leaves the scores in fold 0 unchanged."""
    emb, onmat, counts = separable_names(n=200)
    fold = lb.name_folds(counts, J=3, seed=2026)
    flipped = onmat.copy()
    flipped[fold == 0] = 1 - flipped[fold == 0]
    kw: dict[str, Any] = dict(J=3, max_epochs=3, patience=2, verbose=False)
    a, _ = lb.list_scores_oof(emb, onmat, counts, **kw)
    b, _ = lb.list_scores_oof(emb, flipped, counts, **kw)
    np.testing.assert_allclose(a[fold == 0], b[fold == 0], rtol=1e-6)
    assert not np.allclose(a[fold != 0], b[fold != 0])


def test_per_list_model_heldout_loss_falls_with_training() -> None:
    emb, onmat, counts = separable_names()
    X, Y, W = (torch.as_tensor(a, dtype=torch.float32)
               for a in (emb, onmat, counts))
    idx = np.arange(len(emb))
    model = lb.PerListModel(4, 2, seed=0, device=torch.device("cpu"))
    before = model.heldout_bce(X, Y, W, idx)
    rng = np.random.RandomState(0)
    for _ in range(5):
        model.train_epoch(X, Y, W, idx, rng, batch_size=64)
    assert model.heldout_bce(X, Y, W, idx) < before
    assert model.predict(X, idx[:7]).shape == (7, 2)


# ---------------------------------------------------------------------------
# Algorithm 1, Steps 7-10, and Algorithm 2
# ---------------------------------------------------------------------------
def test_logodds_clips_at_zero_and_one() -> None:
    z = lb.logodds([0.0, 0.5, 1.0])
    assert np.all(np.isfinite(z))
    assert z[1] == 0
    np.testing.assert_allclose(z[0], -z[2])


def test_kmeans_occupied_leaves_every_cluster_with_a_fitting_observation(
) -> None:
    rng = np.random.default_rng(3)
    Z = np.round(rng.normal(size=(3000, 3)), 1)
    rows = np.sort(rng.choice(3000, 2000, replace=False))
    labels, K_used = lb.kmeans_occupied(Z, rows, 300, 0)
    assert set(labels[rows]) == set(range(K_used))
    assert labels.max() < K_used <= 300
    # identical score vectors share a cluster
    _, inv = np.unique(Z, axis=0, return_inverse=True)
    inv = np.asarray(inv).ravel()
    first = {}
    for i, u in enumerate(inv):
        assert first.setdefault(u, labels[i]) == labels[i]


def test_geo_setup_averages_the_prior_within_geography() -> None:
    geo = np.array([0, 0, 2, 2, 2])
    prior = np.array([[1, 0], [0, 1], [1, 0], [1, 0], [0, 1]], float)
    geo_a, prior_a, n_geo, w, M = lb._geo_setup(geo, prior)
    assert n_geo == 3
    np.testing.assert_array_equal(w, [2, 0, 3])
    np.testing.assert_allclose(M, [[0.5, 0.5], [0, 0], [2 / 3, 1 / 3]])


def test_wls_pi_recovers_noiseless_cluster_rates() -> None:
    rng = np.random.default_rng(0)
    G, R, K = 60, 3, 7
    M = rng.dirichlet(np.ones(R), G)
    w = rng.integers(50, 500, G).astype(float)
    Pi = rng.dirichlet(np.ones(K), R)
    C = sparse.csr_matrix(w[:, None] * (M @ Pi))
    np.testing.assert_allclose(lb.wls_pi(C, M, w, np.ones(G)), Pi, atol=1e-6)
    # geographies left out of the fit do not affect it
    take = (np.arange(G) % 3 != 0).astype(float)
    C_bad = C.toarray()
    C_bad[take == 0] = rng.random((int((take == 0).sum()), K)) * 100
    np.testing.assert_allclose(
        lb.wls_pi(sparse.csr_matrix(C_bad), M, w, take), Pi, atol=1e-6)


def test_geo_folds_balances_people() -> None:
    geo = np.repeat(np.arange(30), np.arange(1, 31))
    fold = lb.geo_folds(geo, 4)
    load = np.bincount(fold, weights=np.bincount(geo), minlength=4)
    assert set(fold) == set(range(4))
    assert load.max() - load.min() <= 30


def test_fold_term_is_finite(sim_small: dict[str, Any]) -> None:
    Z = lb.logodds(sim_small["fields"][0])
    geo, _, n_geo, w, M = lb._geo_setup(sim_small["geo"], sim_small["prior"])
    fold = lb.geo_folds(geo, 3)
    q, K_used = lb.fold_term(Z, geo, M, w, n_geo, 10, 0, fold)
    assert np.isfinite(q)
    assert K_used <= 10


def test_select_k_returns_a_value_on_its_curve(
        sim_small: dict[str, Any]) -> None:
    K, curve = lb.select_K(sim_small["fields"][0], sim_small["geo"],
                           sim_small["prior"], grid=[3, 5, 10], J=3,
                           extend=False, verbose=False)
    ks = [k for k, _ in curve]
    assert ks == sorted(ks) == [3, 5, 10]
    assert K == max(curve, key=lambda kq: kq[1])[0]
    assert all(np.isfinite(q) for _, q in curve)


def test_select_k_extends_the_grid_while_its_top_is_best(
        sim_small: dict[str, Any]) -> None:
    _, curve = lb.select_K(sim_small["fields"][0], sim_small["geo"],
                           sim_small["prior"], grid=[3], J=3, verbose=False)
    ks = [k for k, _ in curve]
    assert ks[0] == 3 and len(ks) > 1


def test_select_k_needs_j_geographies() -> None:
    geo = np.repeat(np.arange(2), 50)
    with pytest.raises(ValueError, match="geographic units"):
        lb.select_K(np.random.default_rng(0).random((100, 3)), geo,
                    np.full((100, 2), 0.5), J=5, verbose=False)


def test_fit_posterior_sharpens_the_prior(sim: dict[str, Any]) -> None:
    post, bins, K_used, Pi = lb.fit_posterior(sim["fields"][0], sim["geo"],
                                              sim["prior"], 30)
    np.testing.assert_allclose(post.sum(1), 1)
    np.testing.assert_allclose(Pi.sum(1), 1)
    assert Pi.shape == (3, K_used) and bins.max() < K_used
    own = np.arange(len(post)), sim["grp"]
    assert post[own].mean() > sim["prior"][own].mean() + 0.1


def test_lbisg_field_runs_end_to_end(sim_small: dict[str, Any]) -> None:
    rng = np.random.default_rng(0)
    n_names = 150
    name_grp = rng.integers(0, 3, n_names)
    emb = (np.eye(3)[name_grp] + rng.normal(0, 0.3, (n_names, 3)))
    onmat = (np.eye(3)[name_grp] * (rng.random((n_names, 3)) < 0.7))
    grp = sim_small["grp"]
    idx = np.array([rng.choice(np.flatnonzero(name_grp == g)) for g in grp])
    res = lb.lbisg_field(emb, idx, onmat, sim_small["geo"],
                         sim_small["prior"], grid=[3, 6], n_folds=3,
                         geo_folds_J=3, max_epochs=3, patience=2,
                         verbose=False)
    assert res["posterior"].shape == sim_small["prior"].shape
    ks = [k for k, _ in res["Q"]]
    assert ks[:2] == [3, 6] and res["K"] in ks
    assert res["K_used"] <= res["K"]
    assert res["scores"].shape == (len(idx), 3) and len(res["epochs"]) == 3
    fixed = lb.lbisg_field(emb, idx, onmat, sim_small["geo"],
                           sim_small["prior"], K=4, n_folds=3,
                           max_epochs=2, verbose=False)
    assert fixed["K"] == 4 and fixed["Q"] is None


def test_combine_fields_divides_out_the_repeated_prior() -> None:
    rng = np.random.default_rng(0)
    prior = rng.dirichlet(np.ones(4), 10)
    prior[0, 1] = 0
    p1, p2 = rng.dirichlet(np.ones(4), (2, 10))
    out = lb.combine_fields([p1, p2], prior)
    expected = p1 * p2 / np.where(prior > 0, prior, 1)
    expected[prior == 0] = 0
    np.testing.assert_allclose(out, expected / expected.sum(1, keepdims=True))
    # one field: the posterior itself, zero where the prior is zero
    one = np.where(prior > 0, p1, 0)
    np.testing.assert_allclose(lb.combine_fields([p1], prior),
                               one / one.sum(1, keepdims=True))


# ---------------------------------------------------------------------------
# Downstream estimator: building blocks
# ---------------------------------------------------------------------------
def test_fold_stats_sum_to_the_full_least_squares(
        sim_small: dict[str, Any]) -> None:
    geo, _, n_geo, w, M = lb._geo_setup(sim_small["geo"], sim_small["prior"])
    bins, K_used = lb.kmeans_occupied(lb.logodds(sim_small["fields"][0]),
                                      np.arange(len(geo)), 12)
    fold = lb.geo_folds(geo, 4)
    XtWX, XtWY = lb._fold_stats(bins, geo, M, w, fold, 4, K_used)
    C = sparse.coo_matrix((np.ones(len(geo)), (geo, bins)),
                          shape=(n_geo, K_used)).tocsr()
    np.testing.assert_allclose(lb._phi_from_sums(XtWX.sum(0), XtWY.sum(0)),
                               lb.wls_pi(C, M, w, np.ones(n_geo)))
    np.testing.assert_allclose(
        lb._phi_from_sums(XtWX[1:].sum(0), XtWY[1:].sum(0)),
        lb.wls_pi(C, M, w, (fold != 0).astype(float)))


def test_posteriors_use_the_fit_of_each_persons_fold() -> None:
    rng = np.random.default_rng(0)
    M = rng.dirichlet(np.ones(3), 6)
    geo = np.repeat(np.arange(6), 5)
    fold = np.where(geo < 3, 0, 1)
    bins = rng.integers(0, 4, (2, len(geo)))
    phis = {v: [rng.dirichlet(np.ones(4), 3) for _ in range(2)]
            for v in (0, 1)}
    i = np.arange(len(geo))
    post = lb._posteriors(i, geo, bins, M, fold, phis)
    for p in i:
        v = fold[p]
        e = M[geo[p]] * phis[v][0][:, bins[0, p]] * phis[v][1][:, bins[1, p]]
        np.testing.assert_allclose(post[p], e / e.sum())


def test_pool_batches_hold_whole_pools_once() -> None:
    rng = np.random.default_rng(0)
    geo = rng.integers(0, 40, 2000)
    pool = np.arange(40) // 3
    people = rng.permutation(2000)[:1500]
    seen = []
    for i, starts, ends in lb._pool_batches(people, geo, pool, chunk=300):
        units = [pool[geo[i[s:e]]] for s, e in zip(starts, ends)]
        assert all(len(set(u)) == 1 for u in units)
        assert len({u[0] for u in units}) == len(units)
        assert len(i) <= 300 or len(units) == 1
        assert ends[-1] == len(i)
        seen.append(i)
    allp = np.concatenate(seen)
    assert sorted(allp) == sorted(people)
    batch_pools = [set(pool[geo[i]]) for i in seen]
    assert sum(len(b) for b in batch_pools) == len(set.union(*batch_pools))


def test_pool_grams_match_a_direct_sum() -> None:
    rng = np.random.default_rng(0)
    M = rng.dirichlet(np.ones(3), 10)
    geo = rng.integers(0, 10, 400)
    fold = (geo % 2).astype(np.int64)
    bins = rng.integers(0, 5, (1, 400))
    Y = np.eye(2)[rng.integers(0, 2, 400)]
    pool = np.arange(10) // 4
    phis = {v: [rng.dirichlet(np.ones(5), 3)] for v in (0, 1)}
    people = np.arange(400)
    fulls = {v: [rng.dirichlet(np.ones(5), 3)] for v in (0, 1)}
    A, B, mass, C, c0 = lb._pool_grams(people, geo, bins, M, Y, pool, 3,
                                       fold, phis, chunk=50, fulls=fulls)
    post = lb._posteriors(people, geo, bins, M, fold, phis)
    dpi = lb._posteriors(people, geo, bins, M, fold, fulls) - post
    for c in range(3):
        s = pool[geo] == c
        np.testing.assert_allclose(A[c], post[s].T @ post[s])
        np.testing.assert_allclose(B[c], post[s].T @ Y[s])
        np.testing.assert_allclose(mass[c], post[s].sum(0))
        np.testing.assert_allclose(C[c], dpi[s].T @ post[s]
                                   + post[s].T @ dpi[s])
        np.testing.assert_allclose(c0[c], dpi[s].T @ Y[s])
    # without deltas the correction terms are zero
    out = lb._pool_grams(people, geo, bins, M, Y, pool, 3, fold, phis)
    assert not out[3].any() and not out[4].any()


def test_simplex_rows_projects_onto_the_simplex() -> None:
    rng = np.random.default_rng(0)
    X = rng.normal(0, 2, (50, 4))
    P = lb._simplex_rows(X)
    assert np.all(P >= 0)
    np.testing.assert_allclose(P.sum(1), 1)
    # a point on the simplex is its own projection
    S = rng.dirichlet(np.ones(4), 10)
    np.testing.assert_allclose(lb._simplex_rows(S), S)
    # the projection is the nearest point: (x - p)'(q - p) <= 0 for all q
    Q = rng.dirichlet(np.ones(4), 200)
    for x, p in zip(X, P):
        assert np.all((Q - p) @ (x - p) <= 1e-10)


def test_project_applies_each_constraint() -> None:
    X = np.array([[-0.5, 0.2, 1.4]])
    np.testing.assert_allclose(lb._project(X, "box"), [[0, 0.2, 1]])
    np.testing.assert_allclose(lb._project(X, None), X)
    np.testing.assert_allclose(lb._project(X, "simplex").sum(), 1)


def test_solve_pools_skips_groups_absent_from_a_pool() -> None:
    rng = np.random.default_rng(0)
    Pm = rng.dirichlet(np.ones(3), (2, 30))
    A = np.einsum("cnr,cns->crs", Pm, Pm)
    B = np.einsum("cnr,np->crp", Pm, np.eye(2)[rng.integers(0, 2, 30)])
    solve = np.array([[True, True, True], [True, False, True]])
    mu = lb._solve_pools(A, B, solve)
    assert np.all(np.isnan(mu[1, 1])) and not np.any(np.isnan(mu[0]))
    np.testing.assert_allclose(mu[0], np.linalg.solve(A[0], B[0]))
    k = [0, 2]
    np.testing.assert_allclose(
        mu[1, k], np.linalg.solve(A[1][np.ix_(k, k)], B[1, k]))


def _eb_data(seed: int = 0, n_pool: int = 30, n_per: int = 400,
             spread: float = 0.05) -> dict[str, Any]:
    """Pooling units in two aggregates with unit means spread around the
    aggregate means, and the sums _eb_pools() takes."""
    rng = np.random.default_rng(seed)
    by_pool = np.arange(n_pool) % 2
    m = np.stack([GROUP_MEANS, GROUP_MEANS[::-1]])
    mu = np.stack([lb._simplex_rows(m[s] + rng.normal(0, spread, (3, 3)))
                   for s in by_pool])
    A = np.zeros((n_pool, 3, 3))
    B = np.zeros((n_pool, 3, 3))
    yy = np.zeros((n_pool, 3))
    for c in range(n_pool):
        pi = rng.dirichlet(np.full(3, 0.6), n_per)
        p = pi @ mu[c]
        y = np.eye(3)[(rng.random(n_per)[:, None] > np.cumsum(p, 1)).sum(1)
                      .clip(0, 2)]
        A[c], B[c], yy[c] = pi.T @ pi, pi.T @ y, y.sum(0)
    return dict(A=A, B=B, yy=yy, by_pool=by_pool, mu=mu, m=m,
                solve=np.ones((n_pool, 3), bool))


def test_eb_pools_is_a_ridge_toward_the_aggregate() -> None:
    d = _eb_data(n_per=4000)
    mu, fit = lb._eb_pools(d["A"], d["B"], d["yy"], d["solve"], d["by_pool"],
                           2, "simplex")
    # the posterior mean solves (A_c + Lambda) mu_c = B_c + Lambda m_s
    lam = fit["sigma2"] / fit["tau2"]
    for c in range(len(mu)):
        lhs = (d["A"][c] + np.diag(lam)) @ mu[c]
        rhs = d["B"][c] + lam[:, None] * fit["m"][d["by_pool"][c]]
        np.testing.assert_allclose(lhs, rhs, atol=1e-8)
    # the aggregate means are recovered
    target = np.stack([d["mu"][d["by_pool"] == s].mean(0) for s in (0, 1)])
    np.testing.assert_allclose(fit["m"], target, atol=0.03)
    # pooling beats the least squares of each unit
    ls = lb._solve_pools(d["A"], d["B"], d["solve"])
    assert (np.mean((mu - d["mu"]) ** 2)
            < np.mean((ls - d["mu"]) ** 2))


def test_eb_pools_bound_caps_the_pooling_variance() -> None:
    # units with almost no data: without the bound, tau can be implausible
    d = _eb_data(n_per=15, spread=0.0)
    _, fit = lb._eb_pools(d["A"], d["B"], d["yy"], d["solve"], d["by_pool"],
                          2, "simplex")
    mp = np.stack([lb._simplex_rows(x) for x in fit["m"]])
    cap = (mp * (1 - mp)).mean((0, 2))
    assert np.all(fit["tau2"] <= cap + 1e-12)
    _, free = lb._eb_pools(d["A"], d["B"], d["yy"], d["solve"],
                           d["by_pool"], 2, None)
    assert np.all(np.isfinite(free["tau2"]))


def test_eb_pools_skips_groups_absent_from_a_pool() -> None:
    d = _eb_data()
    solve = d["solve"].copy()
    solve[3, 1] = False
    mu, _ = lb._eb_pools(d["A"], d["B"], d["yy"], solve, d["by_pool"], 2,
                         "simplex")
    assert np.all(np.isnan(mu[3, 1]))
    assert np.isfinite(np.delete(mu.reshape(-1, 3), 3 * 3 + 1, 0)).all()


def test_aggregate_is_the_weighted_mean_over_pools() -> None:
    mu = np.array([[[0.2], [np.nan]], [[0.6], [0.5]], [[0.9], [0.1]]])
    weight = np.array([[1.0, 0.0], [3.0, 2.0], [4.0, 1.0]])
    theta, N = lb._aggregate(mu, weight, np.array([0, 0, 1]), 2)
    np.testing.assert_allclose(N, [[4, 2], [4, 1]])
    np.testing.assert_allclose(theta[:, :, 0], [[0.5, 0.5], [0.9, 0.1]])


def test_fold_phis_leave_out_each_fold() -> None:
    rng = np.random.default_rng(0)
    XtWX = rng.random((4, 3, 3)) + np.eye(3) * 5
    XtWY = rng.random((4, 3, 6))
    tot = {v: [(XtWX, XtWY)] for v in (0, 2)}
    wt = np.array([1.0, 2.0, 0.0, 1.0])
    phis = lb._fold_phis(tot, [0, 2], wt)
    np.testing.assert_allclose(
        phis[0][0], lb._phi_from_sums(2 * XtWX[1] + XtWX[3],
                                      2 * XtWY[1] + XtWY[3]))
    full = lb._fold_phis(tot, [0, 2], np.ones(4), leave_out=False)
    np.testing.assert_allclose(full[2][0], lb._phi_from_sums(XtWX.sum(0),
                                                             XtWY.sum(0)))


def test_combine_clusterings_averages_and_adds_the_spread() -> None:
    theta = [np.array([[[0.2, 0.8]]]), np.array([[[0.4, 0.6]]])]
    res = [dict(mu=t, theta_raw=t, vcov=np.eye(2)[None] * 0.01,
                N=np.ones((1, 1)), K=np.array([[5]]), pooling=[None])
           for t in theta]
    out = lb.combine_clusterings(res, "simplex")
    np.testing.assert_allclose(out["theta"][0, 0], [0.3, 0.7])
    # mean covariance plus the covariance across clusterings over m
    between = np.cov(np.array([[0.2, 0.8], [0.4, 0.6]]).T, ddof=1) / 2
    np.testing.assert_allclose(out["vcov"][0], 0.01 * np.eye(2) + between)
    assert out["K"].shape == (2, 1)


# ---------------------------------------------------------------------------
# Downstream estimator
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("correct", [False, True])
@pytest.mark.parametrize("n_fields", [1, 2])
def test_downstream_crossfit_recovers_group_means(
        sim: dict[str, Any], correct: bool, n_fields: int) -> None:
    out = lb.downstream_crossfit(sim["fields"][:n_fields], sim["geo"],
                                 sim["prior"], sim["Y"], fold=sim["state"],
                                 pool=sim["county"], K=[30, 20][:n_fields],
                                 correct=correct, se=None, n_seeds=1,
                                 verbose=False)
    assert out["theta"].shape == (1, 3, 3) and out["vcov"] is None
    assert out["mu"].shape == (20, 3, 3)
    assert out["K"].shape == (1, n_fields)
    np.testing.assert_allclose(out["theta"][0].sum(1), 1)
    np.testing.assert_allclose(out["theta"][0], GROUP_MEANS, atol=0.05)
    # the aggregate is the projection of the unprojected aggregate
    np.testing.assert_allclose(out["theta"][0],
                               lb._simplex_rows(out["theta_raw"][0]))
    assert (out["pooling"][0] is None) == correct


def test_downstream_crossfit_pools_toward_the_aggregate(
        sim: dict[str, Any]) -> None:
    kw: dict[str, Any] = dict(fold=sim["state"], pool=sim["county"], K=30,
                              se=None, n_seeds=1, verbose=False)
    eb = lb.downstream_crossfit(sim["fields"][0], sim["geo"], sim["prior"],
                                sim["Y"], **kw)
    ls = lb.downstream_crossfit(sim["fields"][0], sim["geo"], sim["prior"],
                                sim["Y"], correct=True, **kw)
    # the true means are the same in every county: pooling is closer
    assert (np.nanmean((eb["mu"] - GROUP_MEANS) ** 2)
            < np.nanmean((ls["mu"] - GROUP_MEANS) ** 2))
    fit = eb["pooling"][0]
    assert fit["m"].shape == (1, 3, 3) and fit["tau2"].shape == (3,)


def test_downstream_crossfit_bootstrap_se(sim: dict[str, Any]) -> None:
    out = lb.downstream_crossfit(sim["fields"][0], sim["geo"], sim["prior"],
                                 sim["Y"], fold=sim["state"],
                                 pool=sim["county"], K=30, n_boot=20,
                                 n_split=4, n_seeds=1, verbose=False)
    v = out["vcov"][0]
    assert v.shape == (9, 9)
    np.testing.assert_allclose(v, v.T, atol=1e-12)
    assert np.all(np.diag(v) > 0)
    se = np.sqrt(np.diag(v)).reshape(3, 3)
    assert np.all(np.abs(out["theta_raw"][0] - GROUP_MEANS) < 4 * se + 0.01)


def test_downstream_crossfit_averages_over_seeds(
        sim_small: dict[str, Any]) -> None:
    s = sim_small
    kw: dict[str, Any] = dict(fold=s["state"], K=10, n_boot=5, n_split=2,
                              verbose=False)
    two = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                 s["Y"], n_seeds=2, **kw)
    one = [lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                  s["Y"], n_seeds=1, seed=k, **kw)
           for k in (0, 1)]
    assert two["K"].shape == (2, 1) and len(two["pooling"]) == 2
    np.testing.assert_allclose(two["theta_raw"],
                               (one[0]["theta_raw"] + one[1]["theta_raw"]) / 2)
    comb = lb.combine_clusterings(one, "simplex")
    np.testing.assert_allclose(two["vcov"], comb["vcov"])


def test_downstream_crossfit_fold_clusters_leave_out_the_fold(
        sim_small: dict[str, Any]) -> None:
    s = sim_small
    kw: dict[str, Any] = dict(fold=s["state"], K=10, se=None, n_seeds=1,
                              verbose=False)
    per = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                 s["Y"], fold_clusters=True, **kw)
    one = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                 s["Y"], **kw)
    assert per["K"].ndim == 3 and one["K"].ndim == 2
    # with one clustering, the result matches the clustering passed as bins
    bins, _ = lb.kmeans_occupied(lb.logodds(s["fields"][0]),
                                 np.arange(len(s["geo"])), 10)
    given = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                   s["Y"], bins=bins[None], **kw)
    np.testing.assert_allclose(given["theta"], one["theta"])
    # the people of a fold get the clusters fit without that fold
    fold_p = s["state"][s["geo"]]
    v = int(fold_p[0])
    b_v, _ = lb.kmeans_occupied(lb.logodds(s["fields"][0]),
                                np.flatnonzero(fold_p != v), 10)
    alone = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                   s["Y"], bins=b_v[None], **kw)
    assert np.all(np.isfinite(per["theta"]))
    assert not np.allclose(alone["theta"], one["theta"])


def _if_direct(sim: dict[str, Any], bins: Any, out: dict[str, Any]) -> Any:
    """The influence-function covariance of the pooled aggregate computed
    directly, person by person, for one name field and one clustering."""
    geo, _, n_geo, w, M = lb._geo_setup(sim["geo"], sim["prior"])
    Y, fold, pool = sim["Y"], sim["state"], sim["county"]
    n, P = Y.shape
    R, K = M.shape[1], int(bins.max()) + 1
    V = int(fold.max()) + 1
    XtWX, XtWY = lb._fold_stats(bins, geo, M, w, fold, V, K)
    phis = {v: [lb._phi_from_sums(XtWX[np.arange(V) != v].sum(0),
                                  XtWY[np.arange(V) != v].sum(0))]
            for v in range(V)}
    ph = lb._phi_from_sums(XtWX.sum(0), XtWY.sum(0))
    G = XtWX.sum(0)
    fold_p, pc = fold[geo], pool[geo]
    post = lb._posteriors(np.arange(n), geo, bins[None], M, fold_p, phis)
    pm = np.zeros((pool.max() + 1, R))
    np.add.at(pm, pool, w[:, None] * M)
    U = np.linalg.solve(G, M.T).T
    IF = np.zeros((n, R, P))
    for c in range(pool.max() + 1):
        s = np.flatnonzero(pc == c)
        Phi = phis[int(fold_p[s[0]])][0]
        pi, y = post[s], Y[s]
        S = (np.einsum("ir,rs->irs", pi, np.eye(R))
             - pi[:, :, None] * pi[:, None, :])
        dS = S / Phi[:, bins[s]].T[:, None, :]
        full = M[geo[s]] * ph[:, bins[s]].T
        dpi = full / full.sum(1, keepdims=True) - pi
        H = (pi.T @ pi + dpi.T @ pi + pi.T @ dpi)
        mu = out["mu"][c]
        e = y - pi @ mu
        t = (np.einsum("irs,ip->irps", dS, e)
             - np.einsum("ir,iqs,qp->irps", pi, dS, mu))
        Tc = np.zeros((K, R, P, R))
        np.add.at(Tc, bins[s], t)
        contrib = np.einsum("irps,is->irp", Tc[bins], U[geo])
        contrib[s] += pi[:, :, None] * e[:, None, :]
        om = pm[c] / pm.sum(0)
        IF += om[None, :, None] * np.einsum("rs,isp->irp",
                                            np.linalg.inv(H), contrib)
    IF = IF.reshape(n, R * P)
    Sg = np.zeros((n_geo, R * P))
    np.add.at(Sg, geo, IF)
    n_g = np.bincount(geo, minlength=n_geo)
    return IF.T @ IF - Sg.T @ (Sg / np.maximum(n_g, 1)[:, None])


def test_downstream_crossfit_if_se_matches_a_direct_computation(
        sim: dict[str, Any]) -> None:
    bins, _ = lb.kmeans_occupied(lb.logodds(sim["fields"][0]),
                                 np.arange(len(sim["geo"])), 15)
    out = lb.downstream_crossfit(sim["fields"][0], sim["geo"], sim["prior"],
                                 sim["Y"], fold=sim["state"],
                                 pool=sim["county"], bins=bins[None],
                                 correct=True, chunk=700, verbose=False)
    v = out["vcov"][0]
    assert v.shape == (9, 9)
    np.testing.assert_allclose(v, v.T, atol=1e-12)
    assert np.all(np.diag(v) > 0)
    np.testing.assert_allclose(v, _if_direct(sim, bins, out), rtol=1e-6,
                               atol=1e-12)


def test_downstream_crossfit_by_aggregate(sim_small: dict[str, Any]) -> None:
    s = sim_small
    out = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                 s["Y"][:, 0], fold=s["state"],
                                 pool=s["county"], by=s["state"], K=10,
                                 constraint="box", se=None, n_seeds=1,
                                 verbose=False)
    assert out["theta"].shape == (5, 3, 1)
    assert np.nanmin(out["theta"]) >= 0 and np.nanmax(out["theta"]) <= 1
    assert out["pooling"][0]["m"].shape == (5, 3, 1)


def test_downstream_crossfit_accepts_external_fold_stats(
        sim_small: dict[str, Any]) -> None:
    s = sim_small
    geo, _, _, w, M = lb._geo_setup(s["geo"], s["prior"])
    bins, K_used = lb.kmeans_occupied(lb.logodds(s["fields"][0]),
                                      np.arange(len(geo)), 10)
    kw: dict[str, Any] = dict(fold=s["state"], bins=bins[None],
                              n_boot=5, n_split=2, verbose=False)
    base = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                  s["Y"], **kw)
    stats = [lb._fold_stats(bins, geo, M, w, s["state"], 5, K_used)]
    ext = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                 s["Y"], fold_stats=stats, **kw)
    np.testing.assert_allclose(ext["theta"], base["theta"])
    np.testing.assert_allclose(ext["vcov"], base["vcov"])


@pytest.mark.parametrize("kw, message", [
    (dict(constraint="unit"), "constraint must be"),
    (dict(Y_col=True), "one column per category"),
    (dict(fold_stats=[(None, None)]), "fold_stats requires"),
    (dict(fold="single"), "at least two folds"),
    (dict(pool="state", by="county"), "within one aggregate"),
    (dict(K=[10, 10]), "one value per name field"),
    (dict(se="parametric"), "se must be"),
    (dict(se="if"), "se 'if' requires"),
    (dict(se="bootstrap", correct=True), "se 'bootstrap' requires"),
    (dict(n_seeds=0), "n_seeds must be"),
])
def test_downstream_crossfit_checks_its_inputs(
        sim_small: dict[str, Any], kw: dict[str, Any], message: str) -> None:
    s = sim_small
    args: dict[str, Any] = dict(fold=s["state"], K=10, verbose=False)
    Y = s["Y"]
    if kw.pop("Y_col", False):
        Y = Y[:, 0]
    if kw.get("fold") == "single":
        kw["fold"] = np.zeros_like(s["state"])
    if kw.get("pool") == "state":
        kw["pool"], kw["by"] = s["state"], s["county"]
    args.update(kw)
    with pytest.raises(ValueError, match=message):
        lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"], Y,
                               **args)


# ---------------------------------------------------------------------------
# Section 3: recovering geographic prevalence
# ---------------------------------------------------------------------------
SHARES = np.array([[0.5, 0.3, 0.2], [0.1, 0.6, 0.3], [0.2, 0.2, 0.6],
                   [0.4, 0.4, 0.2], [0.7, 0.1, 0.2]])
COVERAGE = np.array([0.5, 0.8, 0.3])


def test_recover_block_with_exclusive_lists() -> None:
    rho = SHARES * COVERAGE
    theta, coverage = lb._recover_block(rho, np.ones(5), np.ones(5))
    np.testing.assert_allclose(theta, SHARES, atol=1e-8)
    np.testing.assert_allclose(coverage, COVERAGE, atol=1e-8)


def test_recover_block_with_a_cross_group_list_rate() -> None:
    lam = 0.05
    Pi = np.diag(COVERAGE - lam) + lam
    theta, coverage = lb._recover_block(SHARES @ Pi, np.ones(5), np.ones(5),
                                        lam=lam)
    np.testing.assert_allclose(theta, SHARES, atol=1e-8)
    np.testing.assert_allclose(coverage, COVERAGE, atol=1e-8)


def test_recover_prevalence_from_people() -> None:
    geo, _, onmat = people_from_shares(SHARES, np.diag(COVERAGE))
    theta, coverage = lb.recover_prevalence(onmat, geo)
    np.testing.assert_allclose(theta, SHARES, atol=1e-8)
    np.testing.assert_allclose(coverage, COVERAGE, atol=1e-8)


def test_recover_prevalence_within_coarse_groups() -> None:
    geo, _, onmat = people_from_shares(SHARES, np.diag(COVERAGE))
    coarse = np.column_stack([SHARES[:, :2].sum(1), SHARES[:, 2]])
    theta, coverage = lb.recover_prevalence(onmat, geo, coarse_prior=coarse,
                                            coarse_of=[0, 0, 1])
    np.testing.assert_allclose(theta, SHARES, atol=1e-8)
    np.testing.assert_allclose(coverage[:2], COVERAGE[:2], atol=1e-8)
    assert np.isnan(coverage[2])
    with pytest.raises(ValueError, match="coarse_of"):
        lb.recover_prevalence(onmat, geo, coarse_prior=coarse)


# ---------------------------------------------------------------------------
# Section 4.3: list quality
# ---------------------------------------------------------------------------
def test_list_quality_recovers_list_rates_without_labels() -> None:
    rates = np.array([[0.6, 0.1], [0.2, 0.7], [0.1, 0.0]])
    geo, grp, onmat = people_from_shares(SHARES, rates)
    prior = SHARES[geo]
    res = lb.list_quality(onmat, geo, prior)
    np.testing.assert_allclose(res["Pi"], rates, atol=1e-8)
    np.testing.assert_allclose(res["coverage"], [0.6, 0.7], atol=1e-8)
    q = np.bincount(grp, minlength=3) / len(grp)
    np.testing.assert_allclose(res["q"], q)
    for col in range(2):
        on = onmat[:, col] == 1
        np.testing.assert_allclose(res["precision"][col],
                                   (grp[on] == col).mean(), atol=1e-8)
    np.testing.assert_allclose(res["D"], np.mean([0.6 - 0.1, 0.7 - 0.2]))


def test_list_quality_binary_dominance() -> None:
    shares = SHARES[:, :2] / SHARES[:, :2].sum(1, keepdims=True)
    shares = np.round(shares, 1)
    shares[:, 1] = 1 - shares[:, 0]
    rates = np.array([[0.6], [0.1]])
    geo, _, onmat = people_from_shares(shares, rates)
    res = lb.list_quality(onmat, geo, shares[geo])
    np.testing.assert_allclose(res["D"], 0.5, atol=1e-8)
