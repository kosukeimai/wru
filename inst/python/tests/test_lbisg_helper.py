"""Tests of lbisg_helper: list scores, clustering, the geographic solve,
the downstream estimator and the recovery of geographic prevalence."""
from __future__ import annotations

import itertools
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
# Downstream estimator: the estimating equation as written in the paper
# ---------------------------------------------------------------------------
def literal_psi(nuisance: list[Any], geo: np.ndarray, M: np.ndarray,
                Y: np.ndarray, g: int, mu_g: np.ndarray) -> np.ndarray:
    """psi^(-v)_g for every person, by the sums over b as written.

    Several name fields use the extension A_bg = M_g * prod_f phi^f_{b_f}, with
    one zeta term per field. Y is (N,) and mu_g is (R,).
    """
    n = len(geo)
    Qinv = np.linalg.inv(M[geo].T @ M[geo] / n)
    p_g = (geo == g).mean()
    out = np.zeros((n, M.shape[1]))
    for rows, bins, phis in nuisance:
        for j, i in enumerate(rows):
            Mi = M[geo[i]]
            for b in itertools.product(*[range(P.shape[1]) for P in phis]):
                phi_b = np.prod([P[:, k] for P, k in zip(phis, b)], axis=0)
                A = M[g] * phi_b
                hit = all(bins[f, j] == k for f, k in enumerate(b))
                eta = (geo[i] == g) / p_g * (Y[i] * hit - A @ mu_g)
                corr = 0.0
                for f, k in enumerate(b):
                    zeta = Qinv @ Mi * ((bins[f, j] == k) - Mi @ phis[f][:, k])
                    rest = np.prod([np.ones(len(A))]
                                   + [P[:, c] for h, (P, c)
                                      in enumerate(zip(phis, b)) if h != f],
                                   axis=0)
                    corr += zeta @ (M[g] * mu_g * rest)
                out[i] += A * (eta - corr)
    return out


def literal_varphi(nuisance: list[Any], geo: np.ndarray, M: np.ndarray,
                   Y: np.ndarray, g: int, mu_g: np.ndarray) -> np.ndarray:
    """varphi_g of the theorem for every person, with each fold's plug-ins."""
    psi = literal_psi(nuisance, geo, M, Y, g, mu_g)
    out = np.zeros_like(psi)
    for rows, _, phis in nuisance:
        cells = itertools.product(*[range(P.shape[1]) for P in phis])
        A = np.array([M[g] * np.prod([P[:, k] for P, k in zip(phis, b)], axis=0)
                      for b in cells])
        out[rows] = np.linalg.solve(A.T @ A, psi[rows].T).T
    return out


def random_system(n_fields: int, seed: int = 0) -> dict[str, Any]:
    """Arbitrary clusters and phi that differ by fold, in number and in value."""
    rng = np.random.default_rng(seed)
    G, R, V = 9, 3, 3
    M = rng.dirichlet(np.full(R, 2.0), G)
    geo = rng.integers(0, G, 150)
    fold_g = np.arange(G) % V
    Y = rng.random((len(geo), 2))
    nuisance = []
    for v in range(V):
        rows = np.flatnonzero(fold_g[geo] == v)
        Ks = [[4, 5, 6][v], [3, 4, 3][v]][:n_fields]
        phis = [rng.dirichlet(np.ones(k), R) for k in Ks]
        bins = np.stack([rng.integers(0, k, len(rows)) for k in Ks])
        nuisance.append((rows, bins, phis))
    return dict(M=M, geo=geo, Y=Y, nuisance=nuisance,
                Qinv=np.linalg.inv(M[geo].T @ M[geo] / len(geo)))


@pytest.mark.parametrize("n_fields", [1, 2])
def test_solution_solves_the_equation_as_written(n_fields: int) -> None:
    s = random_system(n_fields)
    T = lb._zeta_matrix(s["nuisance"], s["geo"], s["M"], s["Qinv"])
    mu, singular = lb._solve_mu(s["nuisance"], s["geo"], s["M"], s["Y"], T)
    assert not singular.any()
    for g in range(len(s["M"])):
        for p in range(2):
            psi = literal_psi(s["nuisance"], s["geo"], s["M"], s["Y"][:, p], g,
                              mu[g, :, p])
            np.testing.assert_allclose(psi.mean(0), 0, atol=1e-10)


def test_equation_is_not_solved_by_another_value() -> None:
    """The check above can fail: a wrong mu leaves the equation unsolved."""
    s = random_system(1)
    T = lb._zeta_matrix(s["nuisance"], s["geo"], s["M"], s["Qinv"])
    mu, _ = lb._solve_mu(s["nuisance"], s["geo"], s["M"], s["Y"], T)
    psi = literal_psi(s["nuisance"], s["geo"], s["M"], s["Y"][:, 0], 0,
                      mu[0, :, 0] + 0.1)
    assert np.abs(psi.mean(0)).max() > 1e-3


def test_constant_second_field_reduces_to_the_single_field_equation() -> None:
    """A second field with one cluster has phi = 1: A_bg, zeta and the estimate
    are exactly those of the single name partition B of the paper."""
    s = random_system(1)
    two = [(rows, np.vstack([bins, np.zeros_like(bins)]),
            [phis[0], np.ones((3, 1))]) for rows, bins, phis in s["nuisance"]]
    T1 = lb._zeta_matrix(s["nuisance"], s["geo"], s["M"], s["Qinv"])
    T2 = lb._zeta_matrix(two, s["geo"], s["M"], s["Qinv"])
    np.testing.assert_allclose(T1, T2, atol=1e-14)
    mu1, _ = lb._solve_mu(s["nuisance"], s["geo"], s["M"], s["Y"], T1)
    mu2, _ = lb._solve_mu(two, s["geo"], s["M"], s["Y"], T2)
    np.testing.assert_allclose(mu1, mu2)


def test_zeta_matrix_with_one_field_is_the_sum_over_clusters() -> None:
    s = random_system(1)
    M, geo, Qinv = s["M"], s["geo"], s["Qinv"]
    T = np.zeros((3, 3))
    for rows, bins, phis in s["nuisance"]:
        Phi = phis[0]
        for j, i in enumerate(rows):
            for b in range(Phi.shape[1]):
                zeta = Qinv @ M[geo[i]] * ((bins[0, j] == b)
                                           - M[geo[i]] @ Phi[:, b])
                T += np.outer(Phi[:, b], zeta)
    np.testing.assert_allclose(lb._zeta_matrix(s["nuisance"], geo, M, Qinv),
                               T / len(geo))


@pytest.mark.parametrize("n_fields", [1, 2])
def test_if_vcov_matches_the_person_level_influence_function(
        n_fields: int) -> None:
    s = random_system(n_fields)
    M, geo, Y, Qinv = s["M"], s["geo"], s["Y"], s["Qinv"]
    T = lb._zeta_matrix(s["nuisance"], geo, M, Qinv)
    mu, singular = lb._solve_mu(s["nuisance"], geo, M, Y, T)
    vcov = lb._if_vcov(s["nuisance"], geo, M, Y, Qinv, mu, singular, chunk=17)
    n = len(geo)
    for g in range(len(M)):
        vp = np.stack([literal_varphi(s["nuisance"], geo, M, Y[:, p], g,
                                      mu[g, :, p]) for p in range(2)], axis=2)
        vp = vp.reshape(n, -1)
        np.testing.assert_allclose(vcov[g], vp.T @ vp / n ** 2, rtol=1e-8,
                                   atol=1e-12)


# ---------------------------------------------------------------------------
# Downstream estimator: properties on simulated data
# ---------------------------------------------------------------------------
TRUE_PHI = np.array([[0.55, 0.25, 0.10, 0.05, 0.05],
                     [0.05, 0.15, 0.55, 0.20, 0.05],
                     [0.10, 0.05, 0.05, 0.25, 0.55]])
TRUE_MU = np.array([0.8, 0.5, 0.2])


def known_setup(seed: int = 0, G: int = 15) -> tuple[np.ndarray, np.ndarray]:
    """Prevalence by geography and the fold of each geography."""
    rng = np.random.default_rng(seed)
    return rng.dirichlet(np.full(3, 3.0), G), np.arange(G) % 3


def draw_people(M: np.ndarray, n: int, rng: np.random.Generator
                ) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Geography at random, then group, cluster and outcome (BIRDiE holds)."""
    geo = rng.integers(0, len(M), n)
    grp = (rng.random(n)[:, None] > np.cumsum(M[geo], 1)).sum(1)
    b = (rng.random(n)[:, None] > np.cumsum(TRUE_PHI[grp], 1)).sum(1)
    y = (rng.random(n) < TRUE_MU[grp]).astype(float)
    return geo, b, y[:, None]


def known_nuisance(geo: np.ndarray, b: np.ndarray, fold_g: np.ndarray,
                   phi_of: Any) -> list[Any]:
    """The nuisance of each fold, with given clusters and phi_of(v)."""
    out = []
    for v in np.unique(fold_g):
        rows = np.flatnonzero(fold_g[geo] == v)
        out.append((rows, b[rows][None], [phi_of(int(v))]))
    return out


def fit_known(geo: np.ndarray, b: np.ndarray, y: np.ndarray, M: np.ndarray,
              fold_g: np.ndarray, phi_of: Any, zeta: bool = True,
              se: bool = False) -> tuple[np.ndarray, Any]:
    nuis = known_nuisance(geo, b, fold_g, phi_of)
    Qinv = np.linalg.inv(M[geo].T @ M[geo] / len(geo))
    T = lb._zeta_matrix(nuis, geo, M, Qinv) if zeta else np.zeros((3, 3))
    mu, singular = lb._solve_mu(nuis, geo, M, y, T)
    vcov = lb._if_vcov(nuis, geo, M, y, Qinv, mu, singular) if se else None
    return mu[:, :, 0], vcov


def test_estimating_equation_is_unbiased_at_known_phi() -> None:
    M, fold_g = known_setup()
    rng = np.random.default_rng(1)
    reps = np.stack([
        fit_known(*draw_people(M, 6000, rng), M, fold_g,
                  lambda v: TRUE_PHI)[0]
        for _ in range(400)])
    mc_se = reps.std(0, ddof=1) / np.sqrt(len(reps))
    assert np.abs(reps.mean(0) - TRUE_MU).max() < 0.02
    assert np.all(np.abs(reps.mean(0) - TRUE_MU) < 4 * mc_se)


def population(M: np.ndarray, per_geo: int = 20000
               ) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """People in the exact proportions of the model in every geography, so that
    sampling noise does not hide the order of the sensitivity to phi."""
    geo, b, y = [], [], []
    for g, m in enumerate(M):
        for r in range(3):
            for k in range(TRUE_PHI.shape[1]):
                count = int(round(per_geo * m[r] * TRUE_PHI[r, k]))
                ones = int(round(count * TRUE_MU[r]))
                geo += [g] * count
                b += [k] * count
                y += [1.0] * ones + [0.0] * (count - ones)
    return np.array(geo), np.array(b), np.array(y)[:, None]


def phi_direction(seed: int = 3) -> np.ndarray:
    """A perturbation of phi whose rows sum to zero, as phi's rows must."""
    h = np.random.default_rng(seed).normal(size=TRUE_PHI.shape)
    return h - h.mean(1, keepdims=True)


def order_of_sensitivity(fit: Any, base: np.ndarray) -> tuple[float, float]:
    """(change at eps = 0.01, ratio of the change at 0.01 to that at 0.005);
    the ratio is about 2 at first order."""
    d1 = np.abs(fit(0.01) - base).max()
    d2 = np.abs(fit(0.005) - base).max()
    return float(d1), float(d1 / d2)


def test_zeta_makes_the_estimate_orthogonal_to_a_perturbation_of_phi() -> None:
    M, fold_g = known_setup()
    geo, b, y = population(M)
    h = phi_direction()

    def fit(eps: float, zeta: bool) -> np.ndarray:
        return fit_known(geo, b, y, M, fold_g, lambda v: TRUE_PHI + eps * h,
                         zeta=zeta)[0]

    base = fit(0.0, True)
    np.testing.assert_allclose(base, np.broadcast_to(TRUE_MU, base.shape),
                               atol=2e-3)
    with_zeta, _ = order_of_sensitivity(lambda e: fit(e, True), base)
    without, ratio_none = order_of_sensitivity(lambda e: fit(e, False),
                                               fit(0.0, False))
    # without zeta, first order. With zeta, below second order (eps^2 = 1e-4):
    # in the population the moment at the true mu is zero for any phi common to
    # the folds, so all that is left is the rounding of the population's counts
    assert 1.8 < ratio_none < 2.2
    assert without > 0.01
    assert with_zeta < 1e-4
    assert with_zeta < without / 1000


def test_zeta_does_not_cancel_a_perturbation_that_differs_by_fold() -> None:
    """The equation as written: eta uses phi of g's fold only, while the zeta
    sum averages phi over the folds, so an error that differs by fold moves the
    estimate at first order (the point of the red note in the draft)."""
    M, fold_g = known_setup()
    geo, b, y = population(M)
    hs = [phi_direction(s) for s in (3, 4, 5)]

    def fit(eps: float) -> np.ndarray:
        return fit_known(geo, b, y, M, fold_g,
                         lambda v: TRUE_PHI + eps * hs[v])[0]

    change, ratio = order_of_sensitivity(fit, fit(0.0))
    assert 1.8 < ratio < 2.2
    assert change > 0.01


def test_if_se_matches_the_monte_carlo_sd_at_known_phi() -> None:
    M, fold_g = known_setup()
    rng = np.random.default_rng(2)
    est, var = [], []
    for _ in range(400):
        mu, vc = fit_known(*draw_people(M, 6000, rng), M, fold_g,
                           lambda v: TRUE_PHI, se=True)
        est.append(mu)
        var.append(vc[:, [0, 1, 2], [0, 1, 2]])
    ratio = np.sqrt(np.mean(var, 0)) / np.std(est, 0, ddof=1)
    assert 0.92 < np.median(ratio) < 1.08
    assert 0.8 < ratio.min() and ratio.max() < 1.2


def scores_and_outcome(M: np.ndarray, n: int, rng: np.random.Generator
                       ) -> tuple[np.ndarray, ...]:
    """People whose list scores depend only on their group."""
    geo = rng.integers(0, len(M), n)
    grp = (rng.random(n)[:, None] > np.cumsum(M[geo], 1)).sum(1)
    centers = np.array([[2.0, -2.0, -2.0], [-2.0, 2.0, -2.0], [-2.0, -2.0, 2.0]])
    F = 1 / (1 + np.exp(-(centers[grp] + rng.normal(0, 1.0, (n, 3)))))
    y = (rng.random(n) < TRUE_MU[grp]).astype(float)
    return F, geo, M[geo], y


def test_if_se_matches_the_monte_carlo_sd_with_estimated_phi() -> None:
    M, fold_g = known_setup(G=30)
    fold_g = np.arange(30) % 5
    rng = np.random.default_rng(4)
    est, var = [], []
    for _ in range(60):
        F, geo, prior, y = scores_and_outcome(M, 6000, rng)
        out = lb.downstream_crossfit(F, geo, prior, y, fold=fold_g, K=8,
                                     verbose=False)
        est.append(out["mu"][:, :, 0])
        var.append(out["vcov"][:, [0, 1, 2], [0, 1, 2]])
    ratio = np.sqrt(np.mean(var, 0)) / np.std(est, 0, ddof=1)
    assert 0.8 < np.median(ratio) < 1.25
    assert np.abs(np.mean(est, 0) - TRUE_MU).mean() < 0.02


# ---------------------------------------------------------------------------
# Downstream estimator: downstream_crossfit()
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("n_fields", [1, 2])
def test_downstream_crossfit_recovers_group_means_by_geography(
        sim: dict[str, Any], n_fields: int) -> None:
    out = lb.downstream_crossfit(sim["fields"][:n_fields], sim["geo"],
                                 sim["prior"], sim["Y"], fold=sim["state"],
                                 K=[30, 20][:n_fields], verbose=False)
    assert out["mu"].shape == (100, 3, 3)
    assert out["vcov"].shape == (100, 9, 9)
    assert out["K"].shape == (5, n_fields)
    assert not out["singular"].any()
    np.testing.assert_allclose(np.median(out["mu"], 0), GROUP_MEANS, atol=0.1)
    v = out["vcov"][0]
    np.testing.assert_allclose(v, v.T, atol=1e-12)
    assert np.all(np.diag(v) > 0)


def test_downstream_crossfit_single_field_list_is_the_bare_matrix(
        sim_small: dict[str, Any]) -> None:
    s = sim_small
    a = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"], s["Y"],
                               fold=s["state"], K=10, verbose=False)
    b = lb.downstream_crossfit([s["fields"][0]], s["geo"], s["prior"], s["Y"],
                               fold=s["state"], K=[10], verbose=False)
    np.testing.assert_allclose(a["mu"], b["mu"])
    np.testing.assert_allclose(a["vcov"], b["vcov"])


def test_downstream_crossfit_fits_each_fold_on_the_other_folds(
        sim_small: dict[str, Any]) -> None:
    """Changing the scores and outcomes of fold 0 leaves phi and the clusters of
    fold 0 alone, so the people of fold 0 enter only as data of fold 0."""
    s = sim_small
    fold_p = s["state"][s["geo"]]
    geo, prior, _, w, M = lb._geo_setup(s["geo"], s["prior"])
    F = s["fields"][0]
    Z = lb.logodds(F)
    args = (geo, prior, M, w, s["state"], 0, [10], None, 5, 0, False)
    rows, bins, phis = lb._fold_nuisance([F], [Z], *args)
    F2 = F.copy()
    F2[fold_p == 0] = 0.5 * F2[fold_p == 0]
    rows2, _, phis2 = lb._fold_nuisance([F2], [lb.logodds(F2)], *args)
    np.testing.assert_array_equal(rows, np.flatnonzero(fold_p == 0))
    np.testing.assert_array_equal(rows, rows2)
    assert bins.shape == (1, len(rows))
    np.testing.assert_allclose(phis[0], phis2[0])
    # the other folds do depend on fold 0
    args1 = (geo, prior, M, w, s["state"], 1, [10], None, 5, 0, False)
    _, _, phis1 = lb._fold_nuisance([F], [Z], *args1)
    _, _, phis1b = lb._fold_nuisance([F2], [lb.logodds(F2)], *args1)
    assert (phis1[0].shape != phis1b[0].shape
            or not np.allclose(phis1[0], phis1b[0]))


def test_downstream_crossfit_selects_k_in_each_fold(
        sim_small: dict[str, Any]) -> None:
    s = sim_small
    out = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"], s["Y"],
                                 V=3, grid=[3, 5, 10], J=3, se=False,
                                 verbose=False)
    assert out["vcov"] is None
    assert out["K"].shape == (3, 1)
    assert sorted(set(out["fold"])) == [0, 1, 2]
    # the partition is random, and repeats with the seed
    again = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"], s["Y"],
                                   V=3, K=5, se=False, verbose=False)
    np.testing.assert_array_equal(out["fold"], again["fold"])
    other = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"], s["Y"],
                                   V=3, K=5, se=False, seed=1, verbose=False)
    assert not np.array_equal(out["fold"], other["fold"])


def test_downstream_crossfit_group_absent_from_a_geography_is_nan(
        sim_small: dict[str, Any]) -> None:
    s = sim_small
    prior = s["prior"].copy()
    at = s["geo"] == 3
    prior[at] = [0.6, 0.0, 0.4]
    with pytest.warns(UserWarning, match="1 of 40 geographies"):
        out = lb.downstream_crossfit(s["fields"][0], s["geo"], prior, s["Y"],
                                     fold=s["state"], K=10, verbose=False)
    assert out["singular"].tolist() == [g == 3 for g in range(40)]
    assert np.isnan(out["mu"][3]).all() and np.isnan(out["vcov"][3]).all()
    keep = np.arange(40) != 3
    assert np.isfinite(out["mu"][keep]).all()
    assert np.isfinite(out["vcov"][keep]).all()


def test_downstream_crossfit_fewer_clusters_than_groups_is_nan(
        sim_small: dict[str, Any]) -> None:
    s = sim_small
    with pytest.warns(UserWarning, match="40 of 40 geographies"):
        out = lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"],
                                     s["Y"], fold=s["state"], K=2,
                                     verbose=False)
    assert out["singular"].all() and np.isnan(out["mu"]).all()


@pytest.mark.parametrize("kw, message", [
    (dict(K=[10, 10]), "one value per name field"),
    (dict(fold="single"), "at least two folds"),
])
def test_downstream_crossfit_checks_its_inputs(
        sim_small: dict[str, Any], kw: dict[str, Any], message: str) -> None:
    s = sim_small
    args: dict[str, Any] = dict(fold=s["state"], K=10, verbose=False)
    if kw.get("fold") == "single":
        kw["fold"] = np.zeros_like(s["state"])
    args.update(kw)
    with pytest.raises(ValueError, match=message):
        lb.downstream_crossfit(s["fields"][0], s["geo"], s["prior"], s["Y"],
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
