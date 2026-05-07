"""eBISG helper: text embedding + MLP inference for wru.

This module provides the Python-side machinery for the embedding-supplemented
BISG (eBISG) model in the wru R package. It is imported from R via
reticulate (`reticulate::import_from_path("ebisg_helper", path = ...)`)
and is not typically run directly.

Pipeline (per call from R):
  1. ebisg_embed_names(): take a list of name strings, encode them with a
     sentence-transformer model (default: intfloat/multilingual-e5-large) into
     dense embedding vectors. Models are pulled from HuggingFace Hub on first
     use and cached on disk by `sentence_transformers` (typically under
     ~/.cache/huggingface/hub/).
  2. load_mlp(): deserialize a pre-trained NameMLP checkpoint (.pt file
     distributed with the package via piggyback) into a torch model in eval
     mode.
  3. predict_mlp(): run a forward pass on the embeddings, returning a
     (N, 6) array of softmax probabilities for the race categories
     (white, black, hispanic, asian, aian, other).

R-side caller (see R/ebisg_utils.R) maps the 6 categories to wru's 5-class
system (sums aian + other into oth) and overrides the generic
population-level prior used by standard BISG for unmatched surnames.

To run standalone for debugging:
  >>> from ebisg_helper import embed_names, load_mlp, predict_mlp
  >>> embs = embed_names(["WANTCHEKON", "RATKOVIC"])
  >>> model, params = load_mlp("ebisg-surname-mlp.pt")
  >>> probs = predict_mlp(embs, model)  # (2, 6)

Next steps for maintainers:
  - When adding a new embedding model to the registry in R/ebisg_utils.R,
    no change is needed here as long as the new model is a sentence-transformer
    on HuggingFace Hub. The MLP checkpoint must use the same input_dim.
  - The MLP architecture is fixed (Linear -> ReLU -> Dropout per hidden layer,
    final Linear -> Softmax). To support different architectures, modify
    NameMLP and the load_mlp() reconstruction logic.
"""
from functools import lru_cache

import numpy as np
import torch
import torch.nn as nn


class NameMLP(nn.Module):
    """Multi-layer perceptron for predicting race probabilities from name
    embeddings.

    Architecture: input -> [Linear -> ReLU -> Dropout] x n_layers -> Linear.
    Softmax is applied externally in predict_mlp() so the raw logits are
    available if needed.

    Args:
        input_dim: dimensionality of the input embedding (1024 for E5-Large).
        hidden_dims: list of hidden layer sizes.
        n_classes: output dimension (6 for the eBISG race categories).
        dropout: dropout probability (used during training; inactive in eval).
    """

    def __init__(self, input_dim, hidden_dims, n_classes=6, dropout=0.3):
        super().__init__()
        layers = []
        prev = input_dim
        for h in hidden_dims:
            layers += [nn.Linear(prev, h), nn.ReLU(), nn.Dropout(dropout)]
            prev = h
        layers.append(nn.Linear(prev, n_classes))
        self.net = nn.Sequential(*layers)

    def forward(self, x):
        return self.net(x)


@lru_cache(maxsize=None)
def get_embedding_model(model_name="intfloat/multilingual-e5-large", cache_dir=None):
    """Load a sentence-transformer model, caching across calls within the
    Python session.

    `lru_cache` keys on (model_name, cache_dir) and stores the loaded model so
    repeated calls return the same instance without re-deserializing the
    weights into RAM. Tensor weights themselves are cached on disk by
    `sentence_transformers` (typically ~/.cache/huggingface/hub/), so first
    use downloads from HuggingFace Hub and subsequent sessions load from disk.
    """
    from sentence_transformers import SentenceTransformer
    kwargs = {"device": "cpu"}
    if cache_dir is not None:
        kwargs["cache_folder"] = cache_dir
    return SentenceTransformer(model_name, **kwargs)


def embed_names(names, model_name="intfloat/multilingual-e5-large",
                cache_dir=None, batch_size=256):
    """Embed a list of name strings with a sentence-transformer model.

    Args:
        names: list of strings (uppercase name strings)
        model_name: HuggingFace model ID
        cache_dir: optional path to cache the model weights
        batch_size: encoding batch size

    Returns:
        numpy array of shape (len(names), embedding_dim)
    """
    model = get_embedding_model(model_name, cache_dir)
    embeddings = model.encode(
        names,
        batch_size=batch_size,
        show_progress_bar=len(names) > 1000,
        normalize_embeddings=True,
    )
    return embeddings.astype(np.float32)


def load_mlp(checkpoint_path):
    """Load a NameMLP from a .pt checkpoint.

    The checkpoint is a dict with two keys: `state_dict` (tensors) and
    `params` (a dict of primitive ints/floats describing the architecture).
    Both are safe under torch's `weights_only=True` mode, which restricts
    loading to tensors and primitive types and avoids the pickle-arbitrary-
    code-execution issue flagged by recent torch versions.

    Returns:
        (model, params_dict)
    """
    ckpt = torch.load(checkpoint_path, map_location="cpu", weights_only=True)
    params = ckpt["params"]
    hidden_dims = [params[f"h{i}"] for i in range(params["n_layers"])]
    model = NameMLP(
        input_dim=ckpt.get("input_dim", 1024),
        hidden_dims=hidden_dims,
        n_classes=6,
        dropout=params["dropout"],
    )
    model.load_state_dict(ckpt["state_dict"])
    model.eval()
    return model, params


# Column ordering for predict_mlp output. The MLP was trained on Census 2020
# race tabulations, where AAPI is split into Asian (4) and AIAN (5). wru uses
# 5 categories; the R-side caller sums "aian" and "oth" into "oth".
EBISG_RACE_COLS = ("whi", "bla", "his", "asi", "aian", "oth")


def predict_mlp(embeddings, model, batch_size=10000):
    """Run MLP forward pass, returning softmax probabilities.

    Args:
        embeddings: numpy array (N, dim).
        model: a loaded NameMLP in eval mode.
        batch_size: inference batch size. Default 10000 is a generous CPU
            chunk that keeps a single batch's intermediate activations
            (batch_size x max_hidden_dim x 4 bytes ~ 40 MB for the largest
            shipped MLP) comfortably in memory while minimizing Python-level
            loop overhead. Not formally tuned; lower it if running on a
            memory-constrained machine, raise it for marginal speedup on
            large batches.

    Returns:
        numpy array (N, 6) of probabilities, with columns ordered as
        EBISG_RACE_COLS (whi, bla, his, asi, aian, oth).
    """
    probs = []
    with torch.no_grad():
        for start in range(0, len(embeddings), batch_size):
            xb = torch.tensor(
                embeddings[start : start + batch_size], dtype=torch.float32
            )
            p = torch.softmax(model(xb), dim=1).numpy()
            probs.append(p)
    return np.vstack(probs)
