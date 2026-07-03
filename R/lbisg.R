#' Utilities for lBISG (list-powered BISG)
#'
#' These functions support the lBISG model, which recovers calibrated race
#' probabilities from short, approximate group-specific name lists, with no
#' name-by-race frequency table. It treats list membership as a proxy
#' prediction task over name embeddings and corrects it using a geographic
#' prior from the census. See Chasalow, Dasanaike and Imai.
#'
#' @name lbisg
NULL

# Session cache for the loaded Python lbisg_helper module.
.lbisg_env <- new.env(parent = emptyenv())


#' Set up Python environment for lBISG predictions
#'
#' Installs the Python packages lBISG needs (sentence-transformers and torch for
#' embeddings and the list-membership model, plus scikit-learn and scipy for the
#' geographic solve). Call once before \code{predict_race(..., model = "lBISG")}.
#' Unlike \code{\link{setup_ebisg}}, no pre-trained weights are downloaded: the
#' list-membership model is trained at prediction time from the lists you supply.
#'
#' @param envname Name of the Python virtual environment. Defaults to
#'   \code{"r-ebisg"} so the environment is shared with \code{\link{setup_ebisg}}.
#' @export
setup_lbisg <- function(envname = "r-ebisg") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required for lBISG. ",
         "Install with: install.packages('reticulate')")
  }
  message("Installing Python packages into virtualenv '", envname, "'...")
  reticulate::py_install(
    c("sentence-transformers", "torch", "scikit-learn", "scipy"),
    pip = TRUE, envname = envname
  )
  reticulate::use_virtualenv(envname, required = TRUE)
  message("lBISG setup complete. ",
          "You can now use predict_race(..., model = 'lBISG', lists = ...)")
}


#' Check that Python dependencies for lBISG are available
#' @keywords internal
ensure_lbisg_python <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required for model='lBISG'. ",
         "Install with: install.packages('reticulate')")
  }
  need <- c(sentence_transformers = "sentence-transformers", torch = "torch",
            sklearn = "scikit-learn", scipy = "scipy")
  for (mod in names(need)) {
    if (!reticulate::py_module_available(mod)) {
      stop("Python package '", need[[mod]], "' is required for model='lBISG'.\n",
           "Run setup_lbisg() to configure the environment.")
    }
  }
}


#' Load the Python lbisg_helper module
#' @keywords internal
get_lbisg_module <- function() {
  if (is.null(.lbisg_env$module)) {
    helper_path <- system.file("python", package = "wru")
    if (helper_path == "") stop("Cannot find inst/python/lbisg_helper.py in the wru package.")
    .lbisg_env$module <- reticulate::import_from_path("lbisg_helper", path = helper_path)
  }
  .lbisg_env$module
}


#' @section .predict_race_lbisg:
#' lBISG race prediction: recover P(race | name, geography) from group-specific
#' name lists via an embedding-based list-membership proxy task and a geographic
#' solve, with no name-by-race frequency table.
#' @rdname modfuns
#' @keywords internal
predict_race_lbisg <- function(
    voter.file,
    lists,
    year = "2020",
    census.geo = c("tract", "block", "block_group", "county", "place", "zcta"),
    census.key = Sys.getenv("CENSUS_API_KEY"),
    census.data = NULL,
    retry = 0,
    use.counties = FALSE,
    skip_bad_geos = FALSE,
    ebisg.model = "intfloat/multilingual-e5-large",
    n.folds = 5,
    epochs = 8,
    seed = 42
) {
  cfg <- resolve_ebisg_model(ebisg.model)
  eth <- c("whi", "bla", "his", "asi", "oth")

  ## Validate lists: named list keyed by race abbreviations, "oth" is the residual
  if (!is.list(lists) || is.null(names(lists)) || length(lists) < 1) {
    stop("'lists' must be a named list of character vectors, keyed by race ",
         "abbreviations. Provide lists for the groups to distinguish, e.g. ",
         "list(whi = ..., bla = ..., his = ..., asi = ...); 'oth' is the residual.")
  }
  groups <- names(lists)
  if (!all(groups %in% c("whi", "bla", "his", "asi"))) {
    stop("names(lists) must be a subset of c('whi','bla','his','asi'); ",
         "'oth' is handled as the residual and needs no list.")
  }
  if (!("surname" %in% names(voter.file))) {
    stop("voter.file must have a column named 'surname'.")
  }

  census.geo <- tolower(census.geo)
  census.geo <- rlang::arg_match(census.geo)
  vars.orig <- names(voter.file)

  ## Preliminary data checks (shared eBISG/BISG data preflight for name tables)
  wru_data_preflight()

  ## --- leakage-free external geographic prior P(R | G) via census -----------
  message("Proceeding with Census geographic data at ", census.geo, " level...")
  if (!("state" %in% names(voter.file))) {
    stop("voter.file must have a column named 'state' for lBISG (geography is required).")
  }
  if (is.null(census.data)) {
    census.key <- validate_key(census.key)
  } else {
    census_data_preflight(census.data, census.geo, year)
  }
  geo_id_names <- determine_geo_id_names(census.geo)
  if (!all(geo_id_names %in% names(voter.file))) {
    stop("To use ", census.geo, " as census.geo, voter.file needs the column(s): ",
         paste(geo_id_names, collapse = ", "))
  }
  voter.file <- census_helper_new(
    key = census.key, voter.file = voter.file, states = "all", geo = census.geo,
    age = FALSE, sex = FALSE, year = year, census.data = census.data, retry = retry,
    use.counties = use.counties, skip_bad_geos = skip_bad_geos
  )
  p_rg <- as.matrix(voter.file[, paste0("r_", groups), drop = FALSE])   # N x K, external P(R|G)

  ## --- geographic unit index for the solve ----------------------------------
  geo_key <- do.call(paste, c(voter.file[, c("state", geo_id_names), drop = FALSE], sep = "_"))
  geo_idx <- as.integer(factor(geo_key)) - 1L                          # 0-based contiguous

  ## --- surnames -> unique embeddings ----------------------------------------
  sur <- toupper(as.character(voter.file$surname))
  if (any(is.na(sur) | sur == "")) {
    stop("lBISG requires a non-missing surname for every row; filter empty surnames first.")
  }
  uniq <- unique(sur)
  ensure_lbisg_python()
  message("Embedding ", length(uniq), " unique surnames with ", cfg$transformer, "...")
  emb_u <- ebisg_embed_names(uniq, transformer = cfg$transformer)      # n_unique x dim
  idx <- match(sur, uniq) - 1L                                         # 0-based voter -> unique

  ## --- list-membership indicators -------------------------------------------
  K <- length(groups)
  onmat <- matrix(0L, nrow = nrow(voter.file), ncol = K)
  for (gi in seq_along(groups)) {
    onmat[, gi] <- as.integer(sur %in% toupper(trimws(lists[[groups[gi]]])))
  }
  message("List coverage: ",
          paste(sprintf("%s %.1f%%", groups, 100 * colMeans(onmat)), collapse = ", "))

  ## --- V-model (OOF) + route-B binned BISG (Python engine) ------------------
  message("Training list-membership model and solving the geographic system...")
  mod <- get_lbisg_module()
  P <- mod$predict_lbisg(
    emb_unique = emb_u, idx = as.integer(idx), onmat = onmat, p_rg = p_rg,
    geo_idx = as.integer(geo_idx), epochs = as.integer(epochs),
    n_folds = as.integer(n.folds), seed = as.integer(seed)
  )
  P <- as.matrix(P)                                                    # N x (K+1); last col = other

  ## --- map to wru's 5-class output ------------------------------------------
  preds <- matrix(0, nrow = nrow(voter.file), ncol = length(eth),
                  dimnames = list(NULL, paste0("pred.", eth)))
  for (gi in seq_along(groups)) preds[, paste0("pred.", groups[gi])] <- P[, gi]
  preds[, "pred.oth"] <- preds[, "pred.oth"] + P[, K + 1]
  preds <- preds / rowSums(preds)

  data.frame(cbind(voter.file[vars.orig], preds))
}
