"""Shared simulated data for the tests of the Python helpers."""
from __future__ import annotations

from typing import Any

import numpy as np
import pytest


def simulate(n_geo: int = 100, seed: int = 1) -> dict[str, Any]:
    """Three groups with distinct name signals.

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
    return dict(geo=geo, prior=M[geo], M=M, grp=grp, fields=fields)


@pytest.fixture(scope="session")
def sim() -> dict[str, Any]:
    return simulate()


@pytest.fixture(scope="session")
def sim_small() -> dict[str, Any]:
    return simulate(n_geo=40, seed=2)
