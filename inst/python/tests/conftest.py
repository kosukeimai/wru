"""Shared simulated data for the tests of the Python helpers."""
from __future__ import annotations

from typing import Any

import numpy as np
import pytest

# Mean of each outcome category (Democrat, Republican, other) in each group
GROUP_MEANS = np.array([[0.80, 0.15, 0.05],
                        [0.50, 0.40, 0.10],
                        [0.30, 0.50, 0.20]])


def simulate(n_geo: int = 100, seed: int = 1) -> dict[str, Any]:
    """Three groups with distinct name signals and a three-category outcome.

    Every five geographies form a county and every four counties a state.
    Each person has two independent name fields, each scored on three lists.
    """
    rng = np.random.default_rng(seed)
    M = rng.dirichlet(np.full(3, 0.7), n_geo)
    geo = np.repeat(np.arange(n_geo), rng.poisson(150, n_geo) + 20)
    n = len(geo)
    grp = (rng.random(n)[:, None] > np.cumsum(M[geo], 1)).sum(1)
    centers = np.array([[2.0, -2.0, -2.0], [-2.0, 2.0, -2.0],
                        [-1.0, -1.0, 1.5]])
    fields = [1 / (1 + np.exp(-(centers[grp] + rng.normal(0, 1.6, (n, 3)))))
              for _ in range(2)]
    u = rng.random(n)
    cum = np.cumsum(GROUP_MEANS[grp], 1)
    Y = np.eye(3)[(u[:, None] > cum).sum(1)]
    county = np.arange(n_geo) // (n_geo // 20)
    return dict(geo=geo, prior=M[geo], M=M, grp=grp, fields=fields, Y=Y,
                county=county, state=county // 4)


@pytest.fixture(scope="session")
def sim() -> dict[str, Any]:
    return simulate()


@pytest.fixture(scope="session")
def sim_small() -> dict[str, Any]:
    return simulate(n_geo=40, seed=2)
