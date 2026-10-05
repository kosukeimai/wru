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
#'   \code{coarse.prior}). The groups must exhaust each coarse group.
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
#'   coverages.
#' @examples
#' \dontrun{
#' fit <- lbisg(voters$surname, voters$tract, lists = my_lists, prior = tract_shares)
#' head(fit$posterior)
#' }
#' @export
lbisg <- function(names, geo, lists, prior = NULL, coarse.prior = NULL, coarse.map = NULL,
                  first.names = NULL, first.lists = NULL, K = NULL,
                  control = lbisg_control(), verbose = TRUE) {
  control <- utils::modifyList(lbisg_control(), control)
  n <- length(names)
  if (length(geo) != n) stop("'names' and 'geo' must have the same length.")
  lists <- lbisg_check_lists(lists, "lists")
  groups <- names(lists)
  geo_idx <- as.integer(factor(as.character(geo))) - 1L
  ensure_lbisg_python()
  mod <- get_lbisg_module()

  ## --- geographic prior P(R | G) ------------------------------------------------------------------
  recovered <- NULL
  if (!is.null(prior)) {
    prior <- as.matrix(prior)
    if (nrow(prior) != n || is.null(colnames(prior))) {
      stop("'prior' must have one row per person and one named column per group.")
    }
    unlisted <- setdiff(colnames(prior), groups)
    if (!all(groups %in% colnames(prior))) {
      stop("Every list group needs a column in 'prior': missing ",
           paste(setdiff(groups, colnames(prior)), collapse = ", "), ".")
    }
    if (length(unlisted) > 1) {
      stop("Algorithm 1 requires a name list for every group; only one residual group may be ",
           "predicted without a list. Groups in 'prior' without a list: ",
           paste(unlisted, collapse = ", "), ".")
    }
    prior <- prior[, c(groups, unlisted), drop = FALSE]
  } else {
    onm <- sapply(lists, function(l) as.numeric(lbisg_clean(names) %in% l))
    onm <- matrix(onm, nrow = n, dimnames = list(NULL, groups))
    if (is.null(coarse.prior)) {
      if (verbose) message("Recovering geographic prevalence from the list rates (Section 3.2)...")
      rec <- mod$recover_prevalence(onmat = onm, geo = geo_idx, lam = control$lambda)
    } else {
      if (is.null(coarse.map) || !all(groups %in% names(coarse.map))) {
        stop("'coarse.map' must give the coarse group of every list group.")
      }
      coarse.prior <- as.matrix(coarse.prior)
      if (!all(coarse.map[groups] %in% colnames(coarse.prior))) {
        stop("Every value of 'coarse.map' must be a column of 'coarse.prior'.")
      }
      miss <- setdiff(colnames(coarse.prior), coarse.map[groups])
      if (length(miss)) {
        stop("The groups must exhaust each coarse group; no list group maps to: ",
             paste(miss, collapse = ", "), ".")
      }
      cg <- colnames(coarse.prior)
      gidx <- !duplicated(geo_idx)
      cp_geo <- matrix(0, max(geo_idx) + 1, length(cg))
      cp_geo[geo_idx[gidx] + 1, ] <- coarse.prior[gidx, , drop = FALSE]
      if (verbose) message("Recovering subgroup prevalence from the coarse prevalence (Section 3.3)...")
      rec <- mod$recover_prevalence(onmat = onm, geo = geo_idx, coarse_prior = cp_geo,
                                    coarse_of = match(coarse.map[groups], cg) - 1L,
                                    lam = control$lambda)
    }
    theta <- as.matrix(rec[[1]])
    prior <- theta[geo_idx + 1, , drop = FALSE]
    colnames(prior) <- groups
    recovered <- stats::setNames(as.numeric(rec[[2]]), groups)
  }
  prior <- prior / rowSums(prior)

  ## --- Algorithm 1 for each name field ---------------------------------------------------------
  fit_field <- function(nm, lst, label) {
    nm <- lbisg_clean(nm)
    if (any(is.na(nm) | nm == "")) {
      stop("lBISG requires a non-missing ", label, " for every person; filter empty names first.")
    }
    uniq <- unique(nm)
    if (verbose) message("Embedding ", length(uniq), " distinct ", label, "s...")
    emb <- ebisg_embed_names(uniq, transformer = resolve_ebisg_model(control$embedding.model)$transformer)
    onm <- sapply(names(lst), function(g) as.numeric(uniq %in% lst[[g]]))
    onm <- matrix(onm, nrow = length(uniq))
    if (verbose) {
      message("List coverage (", label, "): ", paste(sprintf("%s %.1f%%", names(lst),
              100 * colMeans(onm[match(nm, uniq), , drop = FALSE])), collapse = ", "))
    }
    if (any(colSums(onm) == 0)) {
      stop("No ", label, " in the data appears on the list for: ",
           paste(names(lst)[colSums(onm) == 0], collapse = ", "), ".")
    }
    res <- mod$lbisg_field(
      emb_unique = emb, idx = match(nm, uniq) - 1L, onmat_unique = onm, geo = geo_idx,
      prior = prior, K = if (is.null(K)) NULL else as.integer(K),
      grid = if (is.null(control$K.grid)) NULL else as.integer(control$K.grid),
      n_folds = as.integer(control$n.folds), geo_folds_J = as.integer(control$geo.folds),
      max_epochs = as.integer(control$max.epochs), patience = as.integer(control$patience),
      batch_size = as.integer(control$batch.size), seed = as.integer(control$seed), verbose = verbose
    )
    q <- res$Q
    list(posterior = as.matrix(res$posterior), K = res$K, K.used = res$K_used,
         Q = if (is.null(q)) NULL else data.frame(K = sapply(q, `[[`, 1), Q = sapply(q, `[[`, 2)),
         epochs = unlist(res$epochs))
  }

  fields <- list(surname = fit_field(names, lists, "surname"))
  if (!is.null(first.names)) {
    if (is.null(first.lists)) stop("'first.lists' is required with 'first.names'.")
    first.lists <- lbisg_check_lists(first.lists, "first.lists")
    if (!setequal(names(first.lists), groups)) {
      stop("'first.lists' must have the same groups as 'lists'.")
    }
    fields$first <- fit_field(first.names, first.lists[groups], "first name")
    post <- as.matrix(mod$combine_fields(unname(lapply(fields, `[[`, "posterior")), prior))
  } else {
    post <- fields$surname$posterior
  }
  colnames(post) <- paste0("pred.", colnames(prior))
  structure(list(posterior = as.data.frame(post), prior = prior, fields = fields,
                 recovered = recovered),
            class = "lbisg")
}


#' Control parameters for lBISG
#'
#' @param n.folds Number of name folds \eqn{J} for the out-of-fold list scores (Algorithm 1).
#' @param geo.folds Number of geographic folds \eqn{J} for choosing \eqn{K} (Algorithm 2).
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
#' @param embedding.model Sentence-transformer used for the name embeddings.
#' @return A named list.
#' @export
lbisg_control <- function(n.folds = 5, geo.folds = 5, K.grid = NULL, max.epochs = 100,
                          patience = 5, batch.size = 512, seed = 42, lambda = 0,
                          embedding.model = "intfloat/multilingual-e5-large") {
  list(n.folds = n.folds, geo.folds = geo.folds, K.grid = K.grid, max.epochs = max.epochs,
       patience = patience, batch.size = batch.size, seed = seed, lambda = lambda,
       embedding.model = embedding.model)
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
#' @return A list with \code{prevalence} (a data frame with one row per
#'   geography and one column per group) and \code{coverage} (the estimated
#'   \eqn{P(L_r = 1 \mid R = r)} of each list).
#' @export
lbisg_recover_prevalence <- function(names, geo, lists, coarse.prior = NULL, coarse.map = NULL,
                                     lambda = 0) {
  lists <- lbisg_check_lists(lists, "lists")
  ensure_lbisg_python()
  mod <- get_lbisg_module()
  f <- factor(as.character(geo)); geo_idx <- as.integer(f) - 1L
  onm <- matrix(sapply(lists, function(l) as.numeric(lbisg_clean(names) %in% l)),
                nrow = length(names))
  if (is.null(coarse.prior)) {
    rec <- mod$recover_prevalence(onmat = onm, geo = geo_idx, lam = lambda)
  } else {
    coarse.prior <- as.matrix(coarse.prior); cg <- colnames(coarse.prior)
    gidx <- !duplicated(geo_idx)
    cp_geo <- matrix(0, nlevels(f), length(cg))
    cp_geo[geo_idx[gidx] + 1, ] <- coarse.prior[gidx, , drop = FALSE]
    rec <- mod$recover_prevalence(onmat = onm, geo = geo_idx, coarse_prior = cp_geo,
                                  coarse_of = match(coarse.map[names(lists)], cg) - 1L, lam = lambda)
  }
  prev <- as.data.frame(as.matrix(rec[[1]])); names(prev) <- names(lists)
  list(prevalence = data.frame(geo = levels(f), prev, check.names = FALSE),
       coverage = stats::setNames(as.numeric(rec[[2]]), names(lists)))
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
#'   \code{D}, and the estimated \code{coverage} and \code{precision} of each list.
#' @export
lbisg_list_quality <- function(names, geo, lists, prior, min.geo.n = 1) {
  lists <- lbisg_check_lists(lists, "lists")
  prior <- as.matrix(prior)
  if (!all(names(lists) %in% colnames(prior))) stop("Every list group needs a column in 'prior'.")
  ensure_lbisg_python()
  mod <- get_lbisg_module()
  geo_idx <- as.integer(factor(as.character(geo))) - 1L
  onm <- matrix(sapply(lists, function(l) as.numeric(lbisg_clean(names) %in% l)), nrow = length(names))
  q <- mod$list_quality(onmat = onm, geo = geo_idx, prior = prior / rowSums(prior),
                        list_group = match(names(lists), colnames(prior)) - 1L,
                        min_geo_n = as.integer(min.geo.n))
  Pi <- as.matrix(q$Pi); dimnames(Pi) <- list(colnames(prior), names(lists))
  list(Pi = Pi, D = q$D, coverage = stats::setNames(as.numeric(q$coverage), names(lists)),
       precision = stats::setNames(as.numeric(q$precision), names(lists)))
}


#' @export
print.lbisg <- function(x, ...) {
  cat("lBISG posterior for", nrow(x$posterior), "people over groups:",
      paste(sub("^pred\\.", "", names(x$posterior)), collapse = ", "), "\n")
  for (f in names(x$fields)) {
    cat(" ", f, ": K =", x$fields[[f]]$K, "(clusters used", x$fields[[f]]$K.used, ")\n")
  }
  invisible(x)
}


lbisg_clean <- function(x) toupper(trimws(as.character(x)))

lbisg_check_lists <- function(lists, arg) {
  if (!is.list(lists) || is.null(names(lists)) || any(names(lists) == "") || length(lists) < 1) {
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
    stop("The 'reticulate' package is required for lBISG. ",
         "Install with: install.packages('reticulate')")
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

.lbisg_env <- new.env(parent = emptyenv())


#' @section .predict_race_lbisg:
#' lBISG race prediction for \code{predict_race(model = "lBISG")}: the census
#' race shares at \code{census.geo} are the geographic prior, and
#' \code{\link{lbisg}} does the rest.
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
  first.lists <- NULL
  if (is.list(lists) && all(names(lists) %in% c("surname", "first")) && "surname" %in% names(lists)) {
    first.lists <- lists$first; lists <- lists$surname
  }
  if (!is.list(lists) || is.null(names(lists)) || !all(names(lists) %in% eth)) {
    stop("'lists' must be a named list of surname vectors keyed by c('whi','bla','his','asi'), ",
         "optionally with 'oth', or list(surname = ..., first = ...) of such lists.")
  }
  need <- c("whi", "bla", "his", "asi")
  if (!all(need %in% names(lists))) {
    stop("lBISG requires a name list for every group (Algorithm 1); missing: ",
         paste(setdiff(need, names(lists)), collapse = ", "),
         ". Only 'oth' may be left as the residual group without a list.")
  }
  use_first <- grepl("first", names.to.use)
  if (use_first && is.null(first.lists)) {
    stop("names.to.use includes first names; supply lists = list(surname = ..., first = ...).")
  }
  if (!("surname" %in% names(voter.file))) stop("voter.file must have a column named 'surname'.")
  if (use_first && !("first" %in% names(voter.file))) stop("voter.file must have a column named 'first'.")
  if (!("state" %in% names(voter.file))) {
    stop("voter.file must have a column named 'state' for lBISG (geography is required).")
  }

  census.geo <- tolower(census.geo)
  census.geo <- rlang::arg_match(census.geo)
  vars.orig <- names(voter.file)

  message("Proceeding with Census geographic data at ", census.geo, " level...")
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
  prior <- as.matrix(voter.file[, paste0("r_", eth), drop = FALSE])
  colnames(prior) <- eth
  geo_key <- do.call(paste, c(voter.file[, c("state", geo_id_names), drop = FALSE], sep = "_"))

  ctl <- utils::modifyList(lbisg_control(), if (is.null(control)) list() else control)
  fit <- lbisg(
    names = voter.file$surname, geo = geo_key, lists = lists, prior = prior,
    first.names = if (use_first) voter.file$first else NULL,
    first.lists = if (use_first) first.lists else NULL,
    control = ctl
  )
  preds <- as.matrix(fit$posterior[, paste0("pred.", eth)])
  out <- data.frame(cbind(voter.file[vars.orig], preds))
  attr(out, "lbisg") <- list(K = sapply(fit$fields, `[[`, "K"),
                             Q = lapply(fit$fields, `[[`, "Q"),
                             epochs = lapply(fit$fields, `[[`, "epochs"))
  out
}
