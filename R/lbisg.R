#' List-powered BISG (lBISG)
#'
#' lBISG recovers calibrated group probabilities from short, approximate
#' group-specific name lists, with no name-by-group frequency table
#' (Chasalow, Dasanaike and Imai). Names are represented by text embeddings;
#' list membership is predicted from the embeddings out of fold, the list
#' scores are clustered, and the cluster-by-group probabilities are recovered
#' from the geographic system \eqn{P(B \mid G) = \sum_r P(B \mid R = r) P(R = r
#' \mid G)} by weighted least squares, before Bayes' rule with the geographic
#' prior. The number of clusters is chosen by the held-out log-likelihood ratio
#' \eqn{Q(K)}. When the geographic prior is unavailable, or available only for
#' coarse groups, it is first recovered from the list rates in each geography.
#'
#' @param names Character vector of surnames, one per person.
#' @param geo Vector of geographic unit identifiers, one per person.
#' @param lists Named list of character vectors: the name list of each group.
#'   Every group in \code{prior} needs a list, except at most one residual group
#'   (for example "other"), which is predicted without a list.
#' @param prior Optional matrix or data frame of \eqn{P(R = r \mid G)} with one
#'   row per person and one named column per group (rows sum to one).
#' @param coarse.prior Optional matrix or data frame of coarse group shares
#'   \eqn{P(C = c \mid G)}, one row per person and one named column per coarse
#'   group, used when \code{prior} is \code{NULL} and only coarse prevalence is
#'   known (Section 3.3). Requires \code{coarse.map}.
#' @param coarse.map Named character vector mapping each list group to its
#'   coarse group (names are list groups, values are columns of
#'   \code{coarse.prior}). The list groups within a coarse group must together
#'   make up that coarse group. One coarse group may have no list group: it is
#'   not subdivided, keeps its coarse share, and is predicted without a list.
#' @param first.names,first.lists Optional first names (one per person) and
#'   first-name lists keyed by the same groups. When supplied, lBISG is applied
#'   separately to each name field and the posteriors are combined under the
#'   lBIFSG assumption (Section 2.5).
#' @param K Optional number of clusters. If \code{NULL} (default), \eqn{K} is
#'   chosen by Algorithm 2 for each name field.
#' @param control A list from \code{\link{lbisg_control}}.
#' @param verbose Print progress.
#'
#' @return An object of class \code{"lbisg"}: a list with \code{posterior} (a
#'   data frame with one column \code{pred.<group>} per group), \code{prior}
#'   (the geographic prior used), and per name field the selected \code{K}, the
#'   \code{Q} curve, and the number of training epochs chosen in each fold.
#'   When the prior was recovered, \code{recovered} holds the estimated list
#'   coverages (\code{NA} for groups that were not subdivided, whose share is
#'   the coarse share).
#' @examples
#' \dontrun{
#' fit <- lbisg(voters$surname, voters$tract, lists = my_lists,
#'              prior = tract_shares)
#' head(fit$posterior)
#' }
#' @export
lbisg <- function(names, geo, lists, prior = NULL, coarse.prior = NULL,
                  coarse.map = NULL, first.names = NULL, first.lists = NULL,
                  K = NULL, control = lbisg_control(), verbose = TRUE) {
  control <- utils::modifyList(lbisg_control(), control)
  n <- length(names)
  if (length(geo) != n) {
    stop("'names' and 'geo' must have the same length.")
  }
  lists <- lbisg_check_lists(lists, "lists")
  groups <- names(lists)
  geo_idx <- as.integer(factor(as.character(geo))) - 1L
  ensure_lbisg_python()
  mod <- get_lbisg_module()

  ## Geographic prior P(R | G): supplied, or recovered from the lists
  recovered <- NULL
  if (!is.null(prior)) {
    prior <- as.matrix(prior)
    lbisg_check_shares(prior, "prior")
    if (nrow(prior) != n || is.null(colnames(prior))) {
      stop(
        "'prior' must have one row per person and one named column ",
        "per group."
      )
    }
    if (!all(groups %in% colnames(prior))) {
      stop(
        "Every list group needs a column in 'prior': missing ",
        paste(setdiff(groups, colnames(prior)), collapse = ", "), "."
      )
    }
    unlisted <- setdiff(colnames(prior), groups)
    if (length(unlisted) > 1) {
      stop(
        "Algorithm 1 requires a name list for every group; only one ",
        "residual group may be predicted without a list. Groups in 'prior' ",
        "without a list: ", paste(unlisted, collapse = ", "), "."
      )
    }
    prior <- prior[, c(groups, unlisted), drop = FALSE]
  } else {
    lbisg_check_exhaustive(control$exhaustive, coarse.prior)
    onm <- lbisg_list_matrix(names, lists)
    if (is.null(coarse.prior)) {
      if (verbose) {
        message("Recovering geographic prevalence from the list rates ",
                "(Section 3.2)...")
      }
      rec <- mod$recover_prevalence(
        onmat = onm, geo = geo_idx, lam = control$lambda
      )
    } else {
      coarse.prior <- as.matrix(coarse.prior)
      lbisg_check_coarse(coarse.prior, coarse.map, groups)
      if (verbose) {
        message("Recovering subgroup prevalence from the coarse prevalence ",
                "(Section 3.3)...")
      }
      rec <- mod$recover_prevalence(
        onmat = onm, geo = geo_idx,
        coarse_prior = lbisg_geo_rows(coarse.prior, geo_idx),
        coarse_of = match(coarse.map[groups], colnames(coarse.prior)) - 1L,
        lam = control$lambda
      )
    }
    prior <- as.matrix(rec[[1]])[geo_idx + 1, , drop = FALSE]
    colnames(prior) <- groups
    recovered <- stats::setNames(as.numeric(rec[[2]]), groups)
    if (!is.null(coarse.prior)) {
      # A coarse group that is not subdivided needs no list: its share is
      # the coarse share, and it is the residual group of Algorithm 1.
      unlisted <- setdiff(colnames(coarse.prior), coarse.map[groups])
      prior <- cbind(prior, coarse.prior[, unlisted, drop = FALSE])
    }
  }
  prior <- prior / rowSums(prior)
  if (!is.null(K) && K < ncol(prior)) {
    stop("'K' must be at least the number of groups (", ncol(prior), ").")
  }

  ## Algorithm 1 for each name field
  fields <- list(
    surname = lbisg_fit_field(names, lists, "surname", geo_idx, prior, K,
                              control, mod, verbose)
  )
  if (!is.null(first.names)) {
    if (is.null(first.lists)) {
      stop("'first.lists' is required with 'first.names'.")
    }
    first.lists <- lbisg_check_lists(first.lists, "first.lists")
    if (!setequal(names(first.lists), groups)) {
      stop("'first.lists' must have the same groups as 'lists'.")
    }
    fields$first <- lbisg_fit_field(first.names, first.lists[groups],
                                    "first name", geo_idx, prior, K,
                                    control, mod, verbose)
    post <- as.matrix(mod$combine_fields(
      unname(lapply(fields, `[[`, "posterior")), prior
    ))
  } else {
    post <- fields$surname$posterior
  }
  colnames(post) <- paste0("pred.", colnames(prior))

  structure(
    list(posterior = as.data.frame(post), prior = prior, fields = fields,
         recovered = recovered),
    class = "lbisg"
  )
}


#' Control parameters for lBISG
#'
#' @param n.folds Number of name folds \eqn{J} for the out-of-fold list scores
#'   (Algorithm 1).
#' @param geo.folds Number of geographic folds \eqn{J} for choosing \eqn{K}
#'   (Algorithm 2).
#' @param K.grid Candidate numbers of clusters for Algorithm 2. The default grid
#'   runs from 10 to 20,000 and is extended upward while the maximum of
#'   \eqn{Q(K)} sits at its top; values below the number of groups or at least
#'   the number of distinct list-score vectors are dropped.
#' @param max.epochs,patience The number of training epochs of each list model
#'   is chosen by cross-validation within the training folds; training stops
#'   once the mean held-out loss has not improved for \code{patience} epochs,
#'   or at \code{max.epochs}.
#' @param batch.size Minibatch size, in distinct names, for the list models.
#' @param seed Random seed.
#' @param lambda Cross-group list rate for the sensitivity analysis of the
#'   prevalence recovery (Appendix C); 0 assumes exclusive lists.
#' @param exhaustive Set to \code{TRUE} to state that the groups with lists
#'   make up the whole population being divided: every group when no prior is
#'   given, or each subdivided coarse group when only a coarse prior is given.
#'   Recovering the prior requires this (Section 3): if some members of a
#'   coarse group belong to none of the listed subgroups, the recovery assigns
#'   them to the listed subgroups anyway. Add a residual list for the remaining
#'   members (for example, the coarse group's names minus those on the
#'   subgroup lists) before setting it. Not used when the full prior is given.
#' @param embedding.model Sentence-transformer used for the name embeddings:
#'   any HuggingFace model ID (default E5-Large). No eBISG checkpoint is
#'   needed, because lBISG trains its own list models.
#' @return A named list.
#' @export
lbisg_control <- function(n.folds = 5, geo.folds = 5, K.grid = NULL,
                          max.epochs = 100, patience = 5, batch.size = 512,
                          seed = 42, lambda = 0, exhaustive = FALSE,
                          embedding.model = "intfloat/multilingual-e5-large") {
  list(
    n.folds = n.folds, geo.folds = geo.folds, K.grid = K.grid,
    max.epochs = max.epochs, patience = patience, batch.size = batch.size,
    seed = seed, lambda = lambda, exhaustive = exhaustive,
    embedding.model = embedding.model
  )
}


#' Recover geographic prevalence from name lists
#'
#' Section 3 of Chasalow, Dasanaike and Imai. Without any geographic prevalence
#' (\code{coarse.prior = NULL}), the lists must be exhaustive and exclusive and
#' \eqn{P(R \mid G)} is recovered from the list rates in each geography (Section
#' 3.2). With coarse group prevalence, the subgroup shares are recovered within
#' each coarse group by non-negative least squares weighted by the expected
#' number of coarse group members and normalized to the coarse share (Section
#' 3.3). \code{lambda > 0} gives the sensitivity analysis of Appendix C.
#'
#' @inheritParams lbisg
#' @param lambda Assumed cross-group list rate (0 for exclusive lists).
#' @param exhaustive Set to \code{TRUE} to state that the groups with lists
#'   make up the whole population being divided; see
#'   \code{\link{lbisg_control}}.
#' @return A list with \code{prevalence} (a data frame with one row per
#'   geography and one column per group) and \code{coverage} (the estimated
#'   \eqn{P(L_r = 1 \mid R = r)} of each list).
#' @export
lbisg_recover_prevalence <- function(names, geo, lists, coarse.prior = NULL,
                                     coarse.map = NULL, lambda = 0,
                                     exhaustive = FALSE) {
  lists <- lbisg_check_lists(lists, "lists")
  lbisg_check_exhaustive(exhaustive, coarse.prior)
  ensure_lbisg_python()
  mod <- get_lbisg_module()
  geo_f <- factor(as.character(geo))
  geo_idx <- as.integer(geo_f) - 1L
  onm <- lbisg_list_matrix(names, lists)

  if (is.null(coarse.prior)) {
    rec <- mod$recover_prevalence(onmat = onm, geo = geo_idx, lam = lambda)
  } else {
    coarse.prior <- as.matrix(coarse.prior)
    lbisg_check_coarse(coarse.prior, coarse.map, names(lists))
    rec <- mod$recover_prevalence(
      onmat = onm, geo = geo_idx,
      coarse_prior = lbisg_geo_rows(coarse.prior, geo_idx),
      coarse_of = match(coarse.map[names(lists)], colnames(coarse.prior)) - 1L,
      lam = lambda
    )
  }

  prev <- as.data.frame(as.matrix(rec[[1]]))
  names(prev) <- names(lists)
  if (!is.null(coarse.prior)) {
    unlisted <- setdiff(colnames(coarse.prior), coarse.map[names(lists)])
    geo_rows <- lbisg_geo_rows(coarse.prior, geo_idx)
    colnames(geo_rows) <- colnames(coarse.prior)
    prev <- cbind(prev, as.data.frame(geo_rows[, unlisted, drop = FALSE]))
  }
  list(
    prevalence = data.frame(geo = levels(geo_f), prev, check.names = FALSE),
    coverage = stats::setNames(as.numeric(rec[[2]]), names(lists))
  )
}


#' Label-free list quality
#'
#' Section 4.3 of Chasalow, Dasanaike and Imai. With the geographic prevalence
#' known, the matrix \eqn{\Pi_{r,r'} = P(L_{r'} = 1 \mid R = r)} is recovered
#' from \eqn{P(L_{r'} = 1 \mid G) = \sum_r \Pi_{r,r'} P(R = r \mid G)} by least
#' squares weighted by the people in each geography, without any group labels.
#'
#' @inheritParams lbisg
#' @param min.geo.n Geographies with fewer people are left out of the solve.
#' @return A list with \code{Pi} (groups by lists), its diagonal dominance
#'   \code{D}, and the estimated \code{coverage} and \code{precision} of each
#'   list.
#' @export
lbisg_list_quality <- function(names, geo, lists, prior, min.geo.n = 1) {
  lists <- lbisg_check_lists(lists, "lists")
  prior <- as.matrix(prior)
  if (!all(names(lists) %in% colnames(prior))) {
    stop("Every list group needs a column in 'prior'.")
  }
  ensure_lbisg_python()
  mod <- get_lbisg_module()
  geo_idx <- as.integer(factor(as.character(geo))) - 1L

  q <- mod$list_quality(
    onmat = lbisg_list_matrix(names, lists),
    geo = geo_idx,
    prior = prior / rowSums(prior),
    list_group = match(names(lists), colnames(prior)) - 1L,
    min_geo_n = as.integer(min.geo.n)
  )
  Pi <- as.matrix(q$Pi)
  dimnames(Pi) <- list(colnames(prior), names(lists))
  list(
    Pi = Pi,
    D = q$D,
    coverage = stats::setNames(as.numeric(q$coverage), names(lists)),
    precision = stats::setNames(as.numeric(q$precision), names(lists))
  )
}


#' @export
print.lbisg <- function(x, ...) {
  cat("lBISG posterior for", nrow(x$posterior), "people over groups:",
      paste(sub("^pred\\.", "", names(x$posterior)), collapse = ", "), "\n")
  for (f in names(x$fields)) {
    cat(" ", f, ": K =", x$fields[[f]]$K,
        "(clusters used", x$fields[[f]]$K.used, ")\n")
  }
  invisible(x)
}


# Fit Algorithm 1 (and Algorithm 2 when K is NULL) to one name field.
lbisg_fit_field <- function(nm, lst, label, geo_idx, prior, K, control, mod,
                            verbose) {
  nm <- lbisg_clean(nm)
  if (any(is.na(nm) | nm == "")) {
    stop(
      "lBISG requires a non-missing ", label, " for every person; ",
      "filter empty names first."
    )
  }
  uniq <- unique(nm)
  if (verbose) message("Embedding ", length(uniq), " distinct ", label, "s...")
  emb <- ebisg_embed_names(
    uniq, transformer = lbisg_transformer(control$embedding.model)
  )
  onm <- lbisg_list_matrix(uniq, lst)

  if (verbose) {
    cover <- colMeans(onm[match(nm, uniq), , drop = FALSE])
    message(
      "List coverage (", label, "): ",
      paste(sprintf("%s %.1f%%", names(lst), 100 * cover), collapse = ", ")
    )
  }
  if (any(colSums(onm) == 0)) {
    stop(
      "No ", label, " in the data appears on the list for: ",
      paste(names(lst)[colSums(onm) == 0], collapse = ", "), "."
    )
  }

  res <- mod$lbisg_field(
    emb_unique = emb,
    idx = match(nm, uniq) - 1L,
    onmat_unique = onm,
    geo = geo_idx,
    prior = prior,
    K = if (is.null(K)) NULL else as.integer(K),
    grid = if (is.null(control$K.grid)) NULL else as.integer(control$K.grid),
    n_folds = as.integer(control$n.folds),
    geo_folds_J = as.integer(control$geo.folds),
    max_epochs = as.integer(control$max.epochs),
    patience = as.integer(control$patience),
    batch_size = as.integer(control$batch.size),
    seed = as.integer(control$seed),
    verbose = verbose
  )

  Q <- NULL
  if (!is.null(res$Q)) {
    Q <- data.frame(K = sapply(res$Q, `[[`, 1), Q = sapply(res$Q, `[[`, 2))
  }
  list(
    posterior = as.matrix(res$posterior),
    K = res$K,
    K.used = res$K_used,
    Q = Q,
    epochs = unlist(res$epochs)
  )
}


# The sentence-transformer for the embeddings: any HuggingFace model ID, or a
# list with a 'transformer' element (as for eBISG). lBISG trains its own list
# models, so no eBISG checkpoint is needed.
lbisg_transformer <- function(model) {
  if (is.list(model)) model <- model$transformer
  if (!is.character(model) || length(model) != 1 || is.na(model)) {
    stop(
      "'embedding.model' must be a HuggingFace model ID, or a list with a ",
      "'transformer' element."
    )
  }
  model
}


# 0/1 matrix of list membership: one row per name, one column per list.
lbisg_list_matrix <- function(nm, lists) {
  nm <- lbisg_clean(nm)
  out <- vapply(lists, function(l) as.numeric(nm %in% l), numeric(length(nm)))
  matrix(out, nrow = length(nm), dimnames = list(NULL, names(lists)))
}


# One row per geography (0-based geo_idx) from a matrix with one row per person.
lbisg_geo_rows <- function(x, geo_idx) {
  first <- !duplicated(geo_idx)
  out <- matrix(0, max(geo_idx) + 1, ncol(x))
  out[geo_idx[first] + 1, ] <- x[first, , drop = FALSE]
  out
}


lbisg_check_coarse <- function(coarse.prior, coarse.map, groups) {
  lbisg_check_shares(coarse.prior, "coarse.prior")
  if (is.null(coarse.map) || !all(groups %in% names(coarse.map))) {
    stop("'coarse.map' must give the coarse group of every list group.")
  }
  if (!all(coarse.map[groups] %in% colnames(coarse.prior))) {
    stop("Every value of 'coarse.map' must be a column of 'coarse.prior'.")
  }
  unlisted <- setdiff(colnames(coarse.prior), coarse.map[groups])
  if (length(unlisted) > 1) {
    stop(
      "Algorithm 1 requires a name list for every group; only one coarse ",
      "group may be left without a list (as the residual group). Coarse ",
      "groups without a list: ", paste(unlisted, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}


# Each row of a prior holds the shares of every group in one geography, so it
# must sum to one. A row that sums to less usually means a group was left out,
# and rescaling it would silently inflate the other groups.
lbisg_check_shares <- function(x, arg) {
  if (!is.numeric(x) || anyNA(x) || any(x < 0)) {
    stop("'", arg, "' must contain non-negative shares with no missing values.")
  }
  off <- abs(rowSums(x) - 1) > 1e-3
  if (any(off)) {
    stop(
      "Each row of '", arg, "' must sum to 1, with one column for every ",
      "group (including a residual group for everyone else); ", sum(off),
      " of ", nrow(x), " rows do not."
    )
  }
  invisible(TRUE)
}


# Recovering the prior assumes the listed groups make up the whole population
# being divided. The software cannot check this, so the user has to state it.
lbisg_check_exhaustive <- function(exhaustive, coarse.prior) {
  if (isTRUE(exhaustive)) return(invisible(TRUE))
  what <- if (is.null(coarse.prior)) {
    "the listed groups make up the whole population"
  } else {
    "the listed subgroups of each subdivided coarse group make up that whole group"
  }
  stop(
    "Recovering geographic prevalence from the lists assumes that ", what,
    " (Section 3). If some members belong to none of the listed groups, ",
    "they are assigned to the listed groups anyway. Add a residual list for ",
    "them (for example, the coarse group's names minus those on the other ",
    "lists), then set exhaustive = TRUE ",
    "(control = lbisg_control(exhaustive = TRUE) in lbisg() and predict_race())."
  )
}


lbisg_clean <- function(x) toupper(trimws(as.character(x)))


lbisg_check_lists <- function(lists, arg) {
  if (!is.list(lists) || is.null(names(lists)) || any(names(lists) == "") ||
      length(lists) < 1) {
    stop("'", arg, "' must be a named list of character vectors, one per group.")
  }
  lapply(lists, function(l) unique(lbisg_clean(l[!is.na(l)])))
}


#' Set up Python environment for lBISG predictions
#'
#' Installs the Python packages lBISG needs (sentence-transformers and torch for
#' the embeddings and the list models, plus scikit-learn and scipy). Call once
#' before \code{predict_race(..., model = "lBISG")} or \code{\link{lbisg}}.
#' No pre-trained weights are downloaded: the list models are trained from the
#' lists you supply.
#'
#' @param envname Name of the Python virtual environment. Defaults to
#'   \code{"r-ebisg"} so the environment is shared with \code{\link{setup_ebisg}}.
#' @export
setup_lbisg <- function(envname = "r-ebisg") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "The 'reticulate' package is required for lBISG. ",
      "Install with: install.packages('reticulate')"
    )
  }
  message("Installing Python packages into virtualenv '", envname, "'...")
  reticulate::py_install(
    c("sentence-transformers", "torch", "scikit-learn", "scipy"),
    pip = TRUE, envname = envname
  )
  reticulate::use_virtualenv(envname, required = TRUE)
  message("lBISG setup complete.")
}


#' Check that Python dependencies for lBISG are available
#' @keywords internal
ensure_lbisg_python <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "The 'reticulate' package is required for model='lBISG'. ",
      "Install with: install.packages('reticulate')"
    )
  }
  need <- c(sentence_transformers = "sentence-transformers", torch = "torch",
            sklearn = "scikit-learn", scipy = "scipy")
  for (mod in names(need)) {
    if (!reticulate::py_module_available(mod)) {
      stop(
        "Python package '", need[[mod]], "' is required for model='lBISG'.\n",
        "Run setup_lbisg() to configure the environment."
      )
    }
  }
}


#' Load the Python lbisg_helper module
#' @keywords internal
get_lbisg_module <- function() {
  if (is.null(.lbisg_env$module)) {
    helper_path <- system.file("python", package = "wru")
    if (helper_path == "") {
      stop("Cannot find inst/python/lbisg_helper.py in the wru package.")
    }
    .lbisg_env$module <- reticulate::import_from_path(
      "lbisg_helper", path = helper_path
    )
  }
  .lbisg_env$module
}

# Session cache for the loaded Python lbisg_helper module.
.lbisg_env <- new.env(parent = emptyenv())


#' @section .predict_race_lbisg:
#' lBISG race prediction for \code{predict_race(model = "lBISG")}: the census
#' race shares at \code{census.geo} are the geographic prior, and
#' \code{\link{lbisg}} does the rest. A race group given as a list of
#' subgroup lists is subdivided, with the subgroup prior recovered from the
#' census share of that group (Section 3.3). Any of whi, bla, his and asi
#' without a list gets a list built from the Census name table.
#' @rdname modfuns
#' @keywords internal
predict_race_lbisg <- function(
    voter.file,
    lists,
    names.to.use = "surname",
    year = "2020",
    census.geo = c("tract", "block", "block_group", "county", "place", "zcta"),
    census.key = Sys.getenv("CENSUS_API_KEY"),
    census.data = NULL,
    retry = 0,
    use.counties = FALSE,
    skip_bad_geos = FALSE,
    control = NULL
) {
  eth <- c("whi", "bla", "his", "asi", "oth")
  ctl <- utils::modifyList(
    lbisg_control(), if (is.null(control)) list() else control
  )

  ## Lists: list(whi = , ...) or list(surname = <lists>, first = <lists>)
  first.lists <- NULL
  if (is.list(lists) && "surname" %in% names(lists) &&
      all(names(lists) %in% c("surname", "first"))) {
    first.lists <- lists$first
    lists <- lists$surname
  }
  lbisg_check_race_lists(lists, eth, "lists")
  use_first <- grepl("first", names.to.use)
  if (use_first) {
    if (is.null(first.lists)) {
      stop(
        "names.to.use includes first names; supply ",
        "lists = list(surname = ..., first = ...)."
      )
    }
    lbisg_check_race_lists(first.lists, eth, "first-name lists")
  }
  if (!("surname" %in% names(voter.file))) {
    stop("voter.file must have a column named 'surname'.")
  }
  if (use_first && !("first" %in% names(voter.file))) {
    stop("voter.file must have a column named 'first'.")
  }
  if (!("state" %in% names(voter.file))) {
    stop(
      "voter.file must have a column named 'state' for lBISG ",
      "(geography is required)."
    )
  }

  census.geo <- tolower(census.geo)
  census.geo <- rlang::arg_match(census.geo)
  vars.orig <- names(voter.file)

  ## Geographic prior: census race shares at census.geo
  message("Proceeding with Census geographic data at ", census.geo, " level...")
  if (is.null(census.data)) {
    census.key <- validate_key(census.key)
  } else {
    census_data_preflight(census.data, census.geo, year)
  }
  geo_id_names <- determine_geo_id_names(census.geo)
  if (!all(geo_id_names %in% names(voter.file))) {
    stop(
      "To use ", census.geo, " as census.geo, voter.file needs the column(s): ",
      paste(geo_id_names, collapse = ", ")
    )
  }
  voter.file <- census_helper_new(
    key = census.key, voter.file = voter.file, states = "all",
    geo = census.geo, age = FALSE, sex = FALSE, year = year,
    census.data = census.data, retry = retry, use.counties = use.counties,
    skip_bad_geos = skip_bad_geos
  )
  prior <- as.matrix(voter.file[, paste0("r_", eth), drop = FALSE])
  colnames(prior) <- eth
  geo_key <- do.call(
    paste, c(voter.file[, c("state", geo_id_names), drop = FALSE], sep = "_")
  )

  ## Fill in Census lists, then flatten subgroups
  sur <- lbisg_race_lists(lists, prior, year, "last")
  first <- if (use_first) lbisg_race_lists(first.lists, prior, year, "first")
  if (use_first && !identical(names(first$lists), names(sur$lists))) {
    stop(
      "The first-name lists must subdivide the same groups into the same ",
      "subgroups as the surname lists."
    )
  }
  subdivided <- unique(sur$coarse[names(sur$coarse) != sur$coarse])

  if (length(subdivided) == 0) {
    fit <- lbisg(
      names = voter.file$surname, geo = geo_key, lists = sur$lists,
      prior = prior,
      first.names = if (use_first) voter.file$first else NULL,
      first.lists = if (use_first) first$lists else NULL,
      control = ctl
    )
  } else {
    fit <- lbisg(
      names = voter.file$surname, geo = geo_key, lists = sur$lists,
      coarse.prior = prior, coarse.map = sur$coarse,
      first.names = if (use_first) voter.file$first else NULL,
      first.lists = if (use_first) first$lists else NULL,
      control = ctl
    )
  }

  ## Standard five columns (a subdivided group is the sum of its subgroups),
  ## then one column per subgroup
  post <- fit$posterior
  preds <- matrix(0, nrow(post), length(eth),
                  dimnames = list(NULL, paste0("pred.", eth)))
  for (g in names(sur$coarse)) {
    col <- paste0("pred.", sur$coarse[[g]])
    preds[, col] <- preds[, col] + post[[paste0("pred.", g)]]
  }
  for (g in setdiff(eth, sur$coarse)) {
    preds[, paste0("pred.", g)] <- post[[paste0("pred.", g)]]
  }
  sub_cols <- paste0("pred.", names(sur$coarse)[names(sur$coarse) != sur$coarse])
  out <- data.frame(cbind(voter.file[vars.orig], preds, post[sub_cols]))
  attr(out, "lbisg") <- list(
    K = sapply(fit$fields, `[[`, "K"),
    Q = lapply(fit$fields, `[[`, "Q"),
    epochs = lapply(fit$fields, `[[`, "epochs"),
    lists = sur$source,
    coverage = fit$recovered
  )
  out
}


# Check the structure of the race lists given to predict_race(model = "lBISG").
lbisg_check_race_lists <- function(lists, eth, what) {
  ok <- is.list(lists) && !is.null(names(lists)) && length(lists) > 0 &&
    all(names(lists) %in% eth)
  if (!ok) {
    stop(
      "The ", what, " must be a named list keyed by any of ",
      "c('whi','bla','his','asi','oth'). Each element is a vector of names, ",
      "or a named list of name vectors to divide that group into subgroups."
    )
  }
  for (g in names(lists)) {
    if (is.list(lists[[g]])) {
      sub <- names(lists[[g]])
      if (is.null(sub) || any(sub == "") || length(sub) < 2) {
        stop(
          "Subgroups of '", g, "' must be a named list of at least two ",
          "name vectors."
        )
      }
      if (any(sub %in% eth)) {
        stop("Subgroup names cannot be race abbreviations: ",
             paste(intersect(sub, eth), collapse = ", "), ".")
      }
    }
  }
  invisible(TRUE)
}


# Flatten the race lists. Returns the lists keyed by group or subgroup, the
# race group of each (coarse), and where each list came from (user or Census).
# Any of whi, bla, his and asi without a list gets a Census list; oth without
# a list is the residual group.
lbisg_race_lists <- function(lists, prior, year, field) {
  out <- list(); coarse <- character(); source <- character()
  for (g in c("whi", "bla", "his", "asi", "oth")) {
    x <- lists[[g]]
    if (is.list(x)) {
      for (s in names(x)) {
        out[[s]] <- x[[s]]; coarse[s] <- g; source[s] <- "user"
      }
    } else if (!is.null(x)) {
      out[[g]] <- x; coarse[g] <- g; source[g] <- "user"
    } else if (g != "oth") {
      out[[g]] <- NA; coarse[g] <- g; source[g] <- "census"
    }
  }
  auto <- names(source)[source == "census"]
  if (length(auto) > 0) {
    message(
      "Using lists built from the Census ", field, " name table for: ",
      paste(auto, collapse = ", ")
    )
    built <- lbisg_census_lists(auto, prior, year, field)
    out[auto] <- built[auto]
  }
  list(lists = out, coarse = coarse, source = source)
}


#' Name lists built from the Census name tables
#'
#' Builds the "Census lists" of Chasalow, Dasanaike and Imai from wru's Census
#' name tables: names with \eqn{P(R = r \mid S) \ge} \code{floor}, ranked by
#' their expected contribution, count times \eqn{(P(R = r \mid S) - P(R = r))},
#' and cut at \code{size}. \eqn{P(R \mid S)} combines the Census
#' \eqn{P(S \mid R)} with the expected number of people of each race in the
#' data (the column sums of \code{prior}).
#'
#' @param groups Race groups to build lists for (any of whi, bla, his, asi, oth).
#' @param prior Matrix of \eqn{P(R \mid G)} with columns whi, bla, his, asi, oth
#'   and one row per person.
#' @param year Census vintage of the name table.
#' @param field \code{"last"} or \code{"first"}.
#' @param floor,size Minimum \eqn{P(R = r \mid S)} and list length.
#' @return A named list of name vectors.
#' @keywords internal
lbisg_census_lists <- function(groups, prior, year = "2020", field = "last",
                               floor = 0.3, size = 1000) {
  eth <- c("whi", "bla", "his", "asi", "oth")
  names_to_use <- if (field == "first") "surname, first" else "surname"
  dict <- read_name_dictionaries(year, names_to_use)
  tbl <- if (field == "first") dict$census_first else dict$census_last
  if (is.null(tbl)) {
    stop(
      "There is no Census ", field, " name table for ", year, "; supply ",
      "first-name lists for every group."
    )
  }
  nm <- toupper(as.character(tbl[[1]]))
  p_s_r <- as.matrix(tbl[, paste0("c_", eth, "_", field)])
  n_r <- colSums(prior[, eth, drop = FALSE])
  counts <- sweep(p_s_r, 2, n_r, `*`)
  total <- rowSums(counts)
  keep <- total > 0
  p_r_s <- counts[keep, , drop = FALSE] / total[keep]
  base <- n_r / sum(n_r)
  out <- lapply(stats::setNames(groups, groups), function(g) {
    p <- p_r_s[, paste0("c_", g, "_", field)]
    score <- ifelse(p >= floor, total[keep] * (p - base[[g]]), -Inf)
    ord <- order(-score)
    ord <- ord[is.finite(score[ord])]
    nm[keep][utils::head(ord, size)]
  })
  out
}
