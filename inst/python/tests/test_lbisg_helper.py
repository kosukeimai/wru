"""Tests of lbisg_helper: list scores, clustering, the geographic solve
and the recovery of geographic prevalence."""
from __future__ import annotations

from typing import Any

import numpy as np
import pytest
import torch
from scipy import sparse

import lbisg_helper as lb


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
