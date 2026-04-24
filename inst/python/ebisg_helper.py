"""eBISG helper: text embedding + MLP inference for wru."""
import numpy as np
import torch
import torch.nn as nn

_embedding_models = {}


class NameMLP(nn.Module):
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


def get_embedding_model(model_name="intfloat/multilingual-e5-large", cache_dir=None):
    """Load a sentence-transformer model, caching across calls."""
    global _embedding_models
    if model_name not in _embedding_models:
        from sentence_transformers import SentenceTransformer
        kwargs = {"device": "cpu"}
        if cache_dir is not None:
            kwargs["cache_folder"] = cache_dir
        _embedding_models[model_name] = SentenceTransformer(model_name, **kwargs)
    return _embedding_models[model_name]


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

    Returns:
        (model, params_dict)
    """
    ckpt = torch.load(checkpoint_path, map_location="cpu", weights_only=False)
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


def predict_mlp(embeddings, model, batch_size=10000):
    """Run MLP forward pass, returning softmax probabilities.

    Args:
        embeddings: numpy array (N, dim)
        model: a loaded NameMLP in eval mode
        batch_size: inference batch size

    Returns:
        numpy array (N, 6) of probabilities
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
