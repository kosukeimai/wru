"""Tests of ebisg_helper: the name MLP, checkpoint loading and embedding.

The embedding model is replaced by a stub, so the tests need no download.
"""
from __future__ import annotations

import sys
import types
from pathlib import Path
from typing import Any, Iterator

import numpy as np
import pytest
import torch

import ebisg_helper as eb


def test_name_mlp_has_the_requested_layers() -> None:
    model = eb.NameMLP(5, [8, 4])
    linear = [m for m in model.net if isinstance(m, torch.nn.Linear)]
    assert [(m.in_features, m.out_features) for m in linear] == [
        (5, 8), (8, 4), (4, 6)]
    assert model(torch.zeros(3, 5)).shape == (3, 6)


def save_checkpoint(path: Path) -> eb.NameMLP:
    torch.manual_seed(0)
    model = eb.NameMLP(5, [8, 4], dropout=0.1)
    torch.save({"state_dict": model.state_dict(), "input_dim": 5,
                "params": {"n_layers": 2, "h0": 8, "h1": 4, "dropout": 0.1}},
               path)
    return model.eval()


def test_load_mlp_restores_the_saved_model(tmp_path: Path) -> None:
    path = tmp_path / "mlp.pt"
    saved = save_checkpoint(path)
    model, params = eb.load_mlp(str(path))
    assert not model.training
    assert params["n_layers"] == 2
    x = torch.randn(4, 5)
    with torch.no_grad():
        torch.testing.assert_close(model(x), saved(x))


def test_predict_mlp_returns_probabilities_in_any_batch_size(
        tmp_path: Path) -> None:
    path = tmp_path / "mlp.pt"
    save_checkpoint(path)
    model, _ = eb.load_mlp(str(path))
    emb = np.random.default_rng(0).normal(size=(10, 5)).astype(np.float32)
    probs = eb.predict_mlp(emb, model)
    assert probs.shape == (10, len(eb.EBISG_RACE_COLS))
    np.testing.assert_allclose(probs.sum(1), 1, rtol=1e-5)
    np.testing.assert_allclose(eb.predict_mlp(emb, model, batch_size=3),
                               probs, rtol=1e-5)


class StubEncoder:
    """Stand-in for SentenceTransformer that records how it is called."""

    instances = 0

    def __init__(self, model_name: str, **kwargs: Any) -> None:
        StubEncoder.instances += 1
        self.model_name = model_name
        self.kwargs = kwargs
        self.calls: list[dict[str, Any]] = []

    def encode(self, names: list[str], **kwargs: Any) -> np.ndarray:
        self.calls.append(dict(names=names, **kwargs))
        return np.arange(len(names) * 3, dtype=np.float64).reshape(-1, 3)


@pytest.fixture
def stub_encoder(
        monkeypatch: pytest.MonkeyPatch) -> Iterator[type[StubEncoder]]:
    module = types.ModuleType("sentence_transformers")
    module.SentenceTransformer = StubEncoder  # type: ignore[attr-defined]
    monkeypatch.setitem(sys.modules, "sentence_transformers", module)
    StubEncoder.instances = 0
    eb.get_embedding_model.cache_clear()
    yield StubEncoder
    eb.get_embedding_model.cache_clear()


def test_get_embedding_model_loads_each_model_once(
        stub_encoder: type[StubEncoder]) -> None:
    a = eb.get_embedding_model("m1", cache_dir="/tmp/hf")
    b = eb.get_embedding_model("m1", cache_dir="/tmp/hf")
    assert a is b and stub_encoder.instances == 1
    assert a.kwargs == {"device": "cpu", "cache_folder": "/tmp/hf"}
    eb.get_embedding_model("m2")
    assert stub_encoder.instances == 2


def test_embed_names_returns_normalized_float32_embeddings(
        stub_encoder: type[StubEncoder]) -> None:
    out = eb.embed_names(("WANTCHEKON", "RATKOVIC"), model_name="m1",
                         batch_size=7)
    assert out.dtype == np.float32 and out.shape == (2, 3)
    call = eb.get_embedding_model("m1", None).calls[0]
    assert call["names"] == ["WANTCHEKON", "RATKOVIC"]
    assert call["normalize_embeddings"] is True
    assert call["batch_size"] == 7
