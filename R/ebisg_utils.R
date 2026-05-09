#' Utilities for eBISG (embedding-supplemented BISG)
#'
#' These functions support the eBISG model, which uses pre-trained text
#' embeddings to predict race probabilities for surnames not found in
#' Census surname lists.
#'
#' @name ebisg
NULL

# Package-level environment for caching loaded Python objects within a single
# R session. This caches the *loaded* SentenceTransformer / NameMLP objects in
# RAM to avoid re-deserializing them on every predict_race() call.
#
# The underlying weight files persist on disk:
#   - MLP .pt files: downloaded once via piggyback to tempdir() (or to getwd()
#     if options(wru_data_wd = TRUE)); re-downloads only if absent.
#   - E5-Large tensors: cached by sentence_transformers under
#     ~/.cache/huggingface/hub/.
#
# So no network round-trips happen between sessions. To force a reload of an
# updated .pt within a session, restart R or `rm(list=ls(.ebisg_env))`.
.ebisg_env <- new.env(parent = emptyenv())

#' Registry of built-in embedding models for eBISG
#'
#' Each entry specifies the HuggingFace transformer model, its embedding
#' dimension, and the corresponding MLP checkpoint filenames distributed
#' via piggyback. Users wanting to plug in a different sentence-transformer
#' (e.g., one of the newer top entries on
#' \href{https://huggingface.co/spaces/mteb/leaderboard}{MTEB}) can bypass
#' this registry by passing a list directly to \code{ebisg.model} with their
#' own \code{transformer}, \code{dim}, and \code{surname_mlp} (path to a local
#' .pt checkpoint). See \code{resolve_ebisg_model()} for the schema.
#' @keywords internal
ebisg_model_registry <- list(
  "intfloat/multilingual-e5-large" = list(
    transformer = "intfloat/multilingual-e5-large",
    dim = 1024L,
    surname_mlp = "ebisg-surname-mlp.pt",
    firstname_mlp = "ebisg-firstname-mlp.pt",
    tag = "v3.1.1"
  )
)


#' Resolve an ebisg.model argument into a config list
#'
#' @param ebisg.model A character string giving the HuggingFace model ID
#'   of a built-in model (e.g., \code{"intfloat/multilingual-e5-large"}),
#'   or a named list with elements \code{transformer}, \code{dim},
#'   \code{surname_mlp}, and optionally \code{firstname_mlp}.
#' @return A list with elements \code{transformer}, \code{dim},
#'   \code{surname_mlp}, \code{firstname_mlp}, \code{fullname_mlp}, \code{tag}.
#' @keywords internal
resolve_ebisg_model <- function(ebisg.model) {
  if (is.character(ebisg.model)) {
    # Don't lowercase: HuggingFace model IDs are case-sensitive in the org part.
    if (!(ebisg.model %in% names(ebisg_model_registry))) {
      stop(
        "Unknown ebisg.model '", ebisg.model, "'. ",
        "Available models: ",
        paste(names(ebisg_model_registry), collapse = ", "), ". ",
        "Or pass a custom list with 'transformer', 'dim', and 'surname_mlp'."
      )
    }
    return(ebisg_model_registry[[ebisg.model]])
  } else if (is.list(ebisg.model)) {
    required <- c("transformer", "dim", "surname_mlp")
    missing <- setdiff(required, names(ebisg.model))
    if (length(missing) > 0) {
      stop(
        "Custom ebisg.model list must include: ",
        paste(missing, collapse = ", ")
      )
    }
    # Defaults for optional fields
    if (is.null(ebisg.model$firstname_mlp)) ebisg.model$firstname_mlp <- NULL
    if (is.null(ebisg.model$fullname_mlp)) ebisg.model$fullname_mlp <- NULL
    if (is.null(ebisg.model$tag)) ebisg.model$tag <- NULL
    return(ebisg.model)
  } else {
    stop("ebisg.model must be a character string or a named list.")
  }
}


#' Set up Python environment for eBISG predictions
#'
#' Installs the required Python packages (sentence-transformers and torch)
#' and downloads pre-trained MLP model weights. Call this once before using
#' \code{predict_race(..., model = "eBISG")}.
#'
#' @param envname Name of the Python virtual environment to use. Defaults to
#'   \code{"r-ebisg"}.
#' @param ebisg.model Which embedding model to set up. Defaults to
#'   \code{"intfloat/multilingual-e5-large"}.
#'
#' @export
setup_ebisg <- function(envname = "r-ebisg",
                        ebisg.model = "intfloat/multilingual-e5-large") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "The 'reticulate' package is required for eBISG. ",
      "Install with: install.packages('reticulate')"
    )
  }
  cfg <- resolve_ebisg_model(ebisg.model)
  message("Installing Python packages into virtualenv '", envname, "'...")
  reticulate::py_install(
    c("sentence-transformers", "torch"),
    pip = TRUE,
    envname = envname
  )
  # Wire the freshly-set-up env into this R session so subsequent
  # py_module_available() / py$ lookups hit the right interpreter.
  reticulate::use_virtualenv(envname, required = TRUE)
  message("Downloading eBISG model weights...")
  ebisg_data_preflight(cfg)
  message(
    "eBISG setup complete. ",
    "You can now use predict_race(..., model = 'eBISG')"
  )
}


#' Check that Python dependencies for eBISG are available
#' @keywords internal
ensure_ebisg_python <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "The 'reticulate' package is required for model='eBISG'. ",
      "Install with: install.packages('reticulate')"
    )
  }
  if (!reticulate::py_module_available("sentence_transformers")) {
    stop(
      "Python package 'sentence-transformers' is required for model='eBISG'.\n",
      "Run setup_ebisg() or install manually with:\n",
      "  reticulate::py_install('sentence-transformers')\n",
      "  reticulate::py_install('torch')"
    )
  }
  if (!reticulate::py_module_available("torch")) {
    stop(
      "Python package 'torch' is required for model='eBISG'.\n",
      "Run setup_ebisg() or install manually with:\n",
      "  reticulate::py_install('torch')"
    )
  }
}


#' Download eBISG model weights via piggyback
#'
#' @param cfg A model config list from \code{resolve_ebisg_model()}.
#' @return The destination directory path.
#' @keywords internal
ebisg_data_preflight <- function(cfg) {
  dest <- ifelse(
    getOption("wru_data_wd", default = FALSE), getwd(), tempdir()
  )

  # For custom models with local paths, skip download
  if (is.null(cfg$tag)) {
    # Custom model: check that files exist at the given paths
    if (!file.exists(cfg$surname_mlp)) {
      stop("Custom surname MLP not found: ", cfg$surname_mlp)
    }
    return(dirname(cfg$surname_mlp))
  }

  files <- c(cfg$surname_mlp)
  if (!is.null(cfg$firstname_mlp)) files <- c(files, cfg$firstname_mlp)
  if (!is.null(cfg$fullname_mlp)) files <- c(files, cfg$fullname_mlp)

  # Only download if files are not already present
  missing <- !file.exists(file.path(dest, files))
  if (any(missing)) {
    tryCatch(
      piggyback::pb_download(
        file = files[missing],
        repo = "kosukeimai/wru",
        dest = dest,
        .token = "",
        tag = cfg$tag
      ),
      error = function(e) {
        stop(
          "Failed to download eBISG model weights: ", e$message, "\n",
          "Run setup_ebisg() to configure the eBISG environment."
        )
      }
    )
  }
  dest
}


#' Load the Python eBISG helper module
#'
#' Imports `inst/python/ebisg_helper.py` as a private module via
#' `reticulate::import_from_path()` and caches the module object on
#' `.ebisg_env`. Using `import_from_path` keeps the helper's symbols
#' encapsulated rather than dumping them into Python's global namespace
#' as `source_python` would.
#'
#' @keywords internal
get_ebisg_module <- function() {
  if (is.null(.ebisg_env$module)) {
    helper_path <- system.file("python", package = "wru")
    if (helper_path == "") {
      stop("Cannot find inst/python/ebisg_helper.py in the wru package.")
    }
    .ebisg_env$module <- reticulate::import_from_path(
      "ebisg_helper", path = helper_path
    )
  }
  .ebisg_env$module
}


#' Embed names using a sentence-transformer model via Python
#'
#' @param names Character vector of names to embed.
#' @param transformer HuggingFace model ID (e.g.,
#'   \code{"intfloat/multilingual-e5-large"}).
#' @param cache_dir Optional cache directory for model weights.
#' @return A matrix of dimensions (length(names), embedding_dim).
#' @keywords internal
ebisg_embed_names <- function(names, transformer = "intfloat/multilingual-e5-large",
                              cache_dir = NULL) {
  mod <- get_ebisg_module()
  embeddings <- mod$embed_names(
    as.list(names),
    model_name = transformer,
    cache_dir = cache_dir
  )
  as.matrix(embeddings)
}


#' Run MLP inference on embeddings
#'
#' @param embeddings Numeric matrix (N x dim) of name embeddings.
#' @param model_path Path to the .pt checkpoint file.
#' @return A matrix of dimensions (N, 6) with columns
#'   (whi, bla, his, asi, aian, oth).
#' @keywords internal
ebisg_predict_mlp <- function(embeddings, model_path) {
  mod <- get_ebisg_module()

  # Cache loaded models by path
  if (is.null(.ebisg_env$models)) {
    .ebisg_env$models <- list()
  }
  if (is.null(.ebisg_env$models[[model_path]])) {
    result <- mod$load_mlp(model_path)
    .ebisg_env$models[[model_path]] <- result[[1]]
  }

  model <- .ebisg_env$models[[model_path]]
  probs <- as.matrix(mod$predict_mlp(embeddings, model))
  # Source of truth for column ordering lives in Python (EBISG_RACE_COLS).
  colnames(probs) <- as.character(mod$EBISG_RACE_COLS)
  probs
}


#' Map 6-class eBISG output to wru's 5-class system
#'
#' The MLP outputs probabilities for 6 categories:
#' (whi, bla, his, asi, aian, oth) -- see \code{EBISG_RACE_COLS} in the
#' Python helper. wru uses 5 categories: (whi, bla, his, asi, oth).
#' This function sums \code{aian} and \code{oth} into \code{oth}.
#'
#' @param probs_6 A matrix with 6 columns named whi, bla, his, asi, aian, oth.
#' @return A matrix with 5 columns named c_whi, c_bla, c_his, c_asi, c_oth.
#' @keywords internal
map_6class_to_5class <- function(probs_6) {
  out <- cbind(
    probs_6[, "whi"],
    probs_6[, "bla"],
    probs_6[, "his"],
    probs_6[, "asi"],
    probs_6[, "aian"] + probs_6[, "oth"]
  )
  colnames(out) <- c("c_whi", "c_bla", "c_his", "c_asi", "c_oth")
  out
}
