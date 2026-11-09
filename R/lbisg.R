#' List-powered BISG (lBISG)
#'
#' Predicts group membership from names and geography using a short name list
#' for each group instead of a name-by-group frequency table. Each name is
#' embedded with a sentence-transformer, and a model for each list predicts
#' whether the name is on that list. Names with similar predictions are grouped
#' into clusters. The share of each cluster within each group is estimated from
#' how the clusters vary with the geographic prior, and the probabilities for
#' each person are then obtained by Bayes' rule. When the prior is unknown, or
#' known only for coarser groups, the prior is first estimated from the rate at
#' which people in each geography have a name on each list. Chasalow, Dasanaike
#' and Imai (2026) describe the method at
#' \url{https://arxiv.org/abs/2610.06273}.
#'
#' @param names Character vector of surnames, one per person.
#' @param geo Vector of geographic unit identifiers, one per person.
#' @param lists Named list of character vectors: the name list of each group.
#'   Each group in \code{prior} requires a list, with the exception of at most
#'   one residual group (for example "other"), which is predicted without a
#'   list.
#' @param prior Optional matrix or data frame of \eqn{P(R = r \mid G)} with one
#'   row per person and one named column per group (rows sum to one).
#' @param coarse.prior Optional matrix or data frame of coarse group shares
#'   \eqn{P(C = c \mid G)}, one row per person and one named column per coarse
#'   group, used when \code{prior} is \code{NULL} and only coarse prevalence is
#'   known. Requires \code{coarse.map}.
#' @param coarse.map Named character vector mapping each list group to the
#'   coarse group that contains it (names are list groups, values are columns of
#'   \code{coarse.prior}). The list groups within a coarse group must together
#'   constitute that coarse group. A single coarse group may have no list group.
#'   That coarse group is not divided, retains the share of that coarse group,
#'   and is
#'   predicted without a list.
#' @param first.names,first.lists Optional first names (one per person) and
#'   first-name lists for the same groups. Each name field is fit separately,
#'   and the two posteriors are combined assuming that the first-name and
#'   surname clusters are independent given group and geography.
#' @param K Optional number of clusters. If \code{NULL} (default), \eqn{K} is
#'   selected for each name field by maximizing the held-out criterion
#'   \eqn{Q(K)}.
#' @param control A list from \code{\link{lbisg_control}}.
#' @param verbose Print progress.
#' @param pretrained Optional pre-trained list models: the path of a surname
#'   model file (\code{.pt}), or a list with paths \code{surname} and,
#'   optionally, \code{first}. The list scores then come from these models and
#'   no list model is fit to \code{names}; \code{lists} and \code{first.lists}
#'   are not used. \code{prior} is required, and its groups need not match the
#'   lists of the model, because the scores are used only to cluster the names.
#'   A model file holds one network per list (embedding -> 256 -> 128 -> 1,
#'   ReLU) as stacked weights \code{W1}, \code{b1}, \code{W2}, \code{b2},
#'   \code{W3}, \code{b3}, with the list names in \code{groups}; names are
#'   upper-cased with internal spaces collapsed before they are embedded.
#'
#' @return An object of class \code{"lbisg"}: a list with \code{posterior} (a
#'   data frame with one column \code{pred.<group>} per group), \code{prior}
#'   (the geographic prior used), and \code{fields}, which contains for each
#'   name field the selected \code{K}, the \code{Q} curve, and the number of
#'   training epochs selected in each fold. When the prior was estimated,
#'   \code{recovered} contains the estimated list coverages. With
#'   \code{gamma = 0}, the coverage is \code{NA} for groups that were not
#'   divided, for which the share is the share of the coarse group.
#' @examples
#' \dontrun{
#' fit <- lbisg(voters$surname, voters$tract, lists = my_lists,
#'              prior = tract_shares)
#' head(fit$posterior)
#' }
#' @export
lbisg <- function(names, geo, lists = NULL, prior = NULL, coarse.prior = NULL,
                  coarse.map = NULL, first.names = NULL, first.lists = NULL,
                  K = NULL, control = lbisg_control(), verbose = TRUE,
                  pretrained = NULL) {
  control <- utils::modifyList(lbisg_control(), control)
  n <- length(names)
  if (length(geo) != n) {
    stop("'names' and 'geo' must have the same length.")
  }
  if (!is.null(pretrained)) {
    return(lbisg_fit_pretrained(names, geo, prior, first.names, K, control,
                                verbose, pretrained))
  }
  lists <- lbisg_check_lists(lists, "lists")
  groups <- names(lists)
  geo_idx <- as.integer(factor(as.character(geo))) - 1L
  ensure_lbisg_python()
  mod <- get_lbisg_module()

  ## Geographic prior P(R | G): supplied, or estimated from the lists
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
        "Each list group requires a column in 'prior': missing ",
        paste(setdiff(groups, colnames(prior)), collapse = ", "), "."
      )
    }
    unlisted <- setdiff(colnames(prior), groups)
    if (length(unlisted) > 1) {
      stop(
        "Each group in 'prior' requires a list, and only one residual group may ",
        "have none. ",
        "Groups without a list: ", paste(unlisted, collapse = ", "), "."
      )
    }
    prior <- prior[, c(groups, unlisted), drop = FALSE]
  } else {
    lbisg_check_exhaustive(control$exhaustive, coarse.prior)
    onm <- lbisg_list_matrix(names, lists)
    if (is.null(coarse.prior)) {
      if (verbose) {
        message("Estimating geographic prevalence from the lists...")
      }
      rec <- mod$recover_prevalence(
        onmat = onm, geo = geo_idx, lam = control$lambda,
        gamma = control$gamma
      )
    } else {
      coarse.prior <- as.matrix(coarse.prior)
      lbisg_check_coarse(coarse.prior, coarse.map, groups)
      if (verbose) {
        message("Estimating subgroup prevalence from the coarse prior...")
      }
      rec <- mod$recover_prevalence(
        onmat = onm, geo = geo_idx,
        coarse_prior = lbisg_geo_rows(coarse.prior, geo_idx),
        coarse_of = match(coarse.map[groups], colnames(coarse.prior)) - 1L,
        lam = control$lambda, gamma = control$gamma
      )
    }
    prior <- as.matrix(rec[[1]])[geo_idx + 1, , drop = FALSE]
    colnames(prior) <- groups
    recovered <- stats::setNames(as.numeric(rec[[2]]), groups)
    if (!is.null(coarse.prior)) {
      # A coarse group that is not divided retains the coarse share and does
      # not require a list.
      unlisted <- setdiff(colnames(coarse.prior), coarse.map[groups])
      prior <- cbind(prior, coarse.prior[, unlisted, drop = FALSE])
    }
  }
  prior <- prior / rowSums(prior)
  if (!is.null(K) && K < ncol(prior)) {
    stop("'K' must be at least the number of groups (", ncol(prior), ").")
  }

  ## Fit each name field
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
#' @param n.folds Number of folds of distinct names. Each name is scored by a
#'   list model that is trained on the other folds.
#' @param K.method How geographic units are held out when \eqn{K} is selected.
#'   With \code{"split"} (the default), each unit is held out with probability
#'   \code{val.frac}. With \code{"crossfit"}, the units are divided into
#'   \code{geo.folds} folds of approximately equal numbers of people, and each
#'   fold is held out once.
#' @param val.frac Probability that a geographic unit is held out
#'   (\code{K.method = "split"}).
#' @param split.seed Seed for the selection of held-out units
#'   (\code{K.method = "split"}).
#' @param geo.folds Number of folds of geographic units
#'   (\code{K.method = "crossfit"}).
#' @param K.grid Candidate numbers of clusters. The default ranges from 10 to
#'   20,000. As long as \eqn{Q(K)} is largest at the top of the grid, the grid
#'   is extended upward by a factor of 1.5. Values below the number of groups,
#'   or at least the number of distinct list-score vectors, are excluded.
#' @param max.epochs,patience The number of training epochs for each list model
#'   is selected by cross-validation. Training is stopped when the mean held-out
#'   loss has not improved for \code{patience} epochs, or at
#'   \code{max.epochs}.
#' @param batch.size Minibatch size, in distinct names, for the list models.
#' @param seed Random seed.
#' @param lambda For a sensitivity analysis of the estimated geographic prior,
#'   the assumed rate at which members of one group have a name on the list
#'   of another group. The default of 0 corresponds to no such overlap. With a
#'   coarse prior, \code{lambda} is the rate within a coarse group.
#' @param gamma With a coarse prior, the assumed rate between groups in
#'   different coarse groups. Must be between 0 and \code{lambda}.
#' @param exhaustive Set to \code{TRUE} to confirm that the groups with lists
#'   constitute the whole population or, with a coarse prior, that the
#'   subgroups constitute each divided coarse group. Estimation of the prior
#'   requires this assumption, because anyone outside the listed groups is
#'   assigned to them. If necessary, add a residual list, such as a list of the
#'   names in the coarse group that do not appear on any subgroup list. Ignored
#'   when
#'   \code{prior} is supplied.
#' @param embedding.model Sentence-transformer used for the name embeddings:
#'   any HuggingFace model ID (default E5-Large). No eBISG checkpoint is
#'   required, because the list models are trained on the supplied lists.
#' @param save.models Optional file prefix. The list models of each name field
#'   are then also fit to all the distinct names and saved as
#'   \code{<prefix>_surname.pt} and \code{<prefix>_first.pt}, which can be
#'   passed to \code{\link{lbisg}} as \code{pretrained} in later runs.
#' @return A named list.
#' @export
lbisg_control <- function(n.folds = 5, K.method = c("split", "crossfit"),
                          val.frac = 0.2, split.seed = 0, geo.folds = 5,
                          K.grid = NULL, max.epochs = 100, patience = 5,
                          batch.size = 512, seed = 42, lambda = 0,
                          gamma = 0, exhaustive = FALSE,
                          embedding.model = "intfloat/multilingual-e5-large",
                          save.models = NULL) {
  K.method <- match.arg(K.method)
  if (!(val.frac > 0 && val.frac < 1)) {
    stop("'val.frac' must be between 0 and 1.")
  }
  if (!(lambda >= 0 && lambda < 1)) stop("'lambda' must be in [0, 1).")
  if (!(gamma >= 0 && gamma <= lambda)) {
    stop("'gamma' must be between 0 and 'lambda'.")
  }
  list(
    n.folds = n.folds, K.method = K.method, val.frac = val.frac,
    split.seed = split.seed, geo.folds = geo.folds, K.grid = K.grid,
    max.epochs = max.epochs, patience = patience, batch.size = batch.size,
    seed = seed, lambda = lambda, gamma = gamma, exhaustive = exhaustive,
    embedding.model = embedding.model, save.models = save.models
  )
}


#' Recover geographic prevalence from name lists
#'
#' Estimates the share of each group in each geographic unit from the rate at
#' which people in that unit have a name on each list, without group labels.
#' With no prior, the lists must cover the whole population and not overlap.
#' With a coarse prior, the subgroup shares within each coarse group are
#' estimated by non-negative least squares and scaled to the coarse share.
#' \code{lambda} and \code{gamma} allow some overlap between lists, for a
#' sensitivity analysis. A warning means that an estimated list coverage is not
#' between \code{lambda} and 1. Such a coverage violates the assumptions of the
#' recovery.
#'
#' @inheritParams lbisg
#' @inheritParams lbisg_control
#' @return A list with \code{prevalence} (a data frame with one row per
#'   geography and one column per group) and \code{coverage} (the estimated
#'   \eqn{P(L_r = 1 \mid R = r)} of each list).
#' @export
lbisg_recover_prevalence <- function(names, geo, lists, coarse.prior = NULL,
                                     coarse.map = NULL, lambda = 0,
                                     gamma = 0, exhaustive = FALSE) {
  lists <- lbisg_check_lists(lists, "lists")
  lbisg_check_exhaustive(exhaustive, coarse.prior)
  ensure_lbisg_python()
  mod <- get_lbisg_module()
  geo_f <- factor(as.character(geo))
  geo_idx <- as.integer(geo_f) - 1L
  onm <- lbisg_list_matrix(names, lists)

  if (is.null(coarse.prior)) {
    rec <- mod$recover_prevalence(onmat = onm, geo = geo_idx, lam = lambda,
                                  gamma = gamma)
  } else {
    coarse.prior <- as.matrix(coarse.prior)
    lbisg_check_coarse(coarse.prior, coarse.map, names(lists))
    rec <- mod$recover_prevalence(
      onmat = onm, geo = geo_idx,
      coarse_prior = lbisg_geo_rows(coarse.prior, geo_idx),
      coarse_of = match(coarse.map[names(lists)], colnames(coarse.prior)) - 1L,
      lam = lambda, gamma = gamma
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
#' Estimates, without group labels, the rate \eqn{\Pi_{r,r'}} at which members
#' of group \eqn{r} have a name on the list of group \eqn{r'}, from how list
#' membership varies with the known geographic prior.
#'
#' @inheritParams lbisg
#' @param min.geo.n Geographies with fewer than \code{min.geo.n} people are
#'   excluded from the least squares fit of \code{Pi}.
#' @return A list with \code{Pi} (groups by lists), the diagonal dominance
#'   \code{D} of \code{Pi}, and the estimated \code{coverage} and
#'   \code{precision} of each list.
#' @export
lbisg_list_quality <- function(names, geo, lists, prior, min.geo.n = 1) {
  lists <- lbisg_check_lists(lists, "lists")
  prior <- as.matrix(prior)
  if (!all(names(lists) %in% colnames(prior))) {
    stop("Each list group requires a column in 'prior'.")
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


# Fit lBISG to one name field, and select K when K is NULL.
lbisg_fit_field <- function(nm, lst, label, geo_idx, prior, K, control, mod,
                            verbose, pretrained = NULL) {
  nm <- lbisg_clean(nm)
  # Pre-trained models were fit on names with internal spaces collapsed.
  if (!is.null(pretrained)) nm <- gsub("\\s+", " ", nm)
  if (any(is.na(nm) | nm == "")) {
    stop(
      "lBISG requires a non-missing ", label, " for each person. ",
      "Remove rows with empty names first."
    )
  }
  uniq <- unique(nm)
  if (verbose) message("Embedding ", length(uniq), " distinct ", label, "s...")
  emb <- ebisg_embed_names(
    uniq, transformer = lbisg_transformer(control$embedding.model)
  )
  if (!is.null(pretrained)) {
    sc <- mod$pretrained_scores(emb, pretrained)
    res <- mod$lbisg_field_scores(
      F = as.matrix(sc[[1]])[match(nm, uniq), , drop = FALSE],
      geo = geo_idx, prior = prior,
      K = if (is.null(K)) NULL else as.integer(K),
      grid = if (is.null(control$K.grid)) NULL else as.integer(control$K.grid),
      geo_folds_J = as.integer(control$geo.folds), verbose = verbose,
      k_method = control$K.method, val_frac = as.numeric(control$val.frac),
      split_seed = as.integer(control$split.seed)
    )
    Q <- NULL
    if (!is.null(res$Q)) {
      Q <- data.frame(K = sapply(res$Q, `[[`, 1), Q = sapply(res$Q, `[[`, 2))
    }
    return(list(posterior = as.matrix(res$posterior), K = res$K,
                K.used = res$K_used, Q = Q, epochs = NULL,
                lists = unlist(sc[[2]])))
  }
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
    verbose = verbose,
    k_method = control$K.method,
    val_frac = as.numeric(control$val.frac),
    split_seed = as.integer(control$split.seed),
    save_path = if (is.null(control$save.models)) NULL else
      paste0(control$save.models, "_",
             if (label == "surname") "surname" else "first", ".pt"),
    groups = names(lst),
    embedding = lbisg_transformer(control$embedding.model)
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


# lBISG with pre-trained list models: the list scores of each name field come
# from a model fit on the lists alone; the clusters, K and the posterior are
# then estimated from the people in the data as usual.
lbisg_fit_pretrained <- function(names, geo, prior, first.names, K, control,
                                 verbose, pretrained) {
  if (is.character(pretrained) && length(pretrained) == 1) {
    pretrained <- list(surname = pretrained)
  }
  if (!is.list(pretrained) || is.null(pretrained$surname)) {
    stop("'pretrained' must be the path of a surname model, or a list with ",
         "paths 'surname' and, optionally, 'first'.")
  }
  for (f in c("surname", "first")) {
    p <- pretrained[[f]]
    if (!is.null(p) && !file.exists(p)) {
      stop("Pre-trained ", f, " model not found: ", p)
    }
    if (!is.null(p)) pretrained[[f]] <- normalizePath(p)
  }
  if (is.null(prior)) {
    stop("Pre-trained list models require 'prior', because the geographic ",
         "prior can only be estimated from the name lists themselves.")
  }
  n <- length(names)
  prior <- as.matrix(prior)
  lbisg_check_shares(prior, "prior")
  if (nrow(prior) != n || is.null(colnames(prior))) {
    stop("'prior' must have one row per person and one named column per ",
         "group.")
  }
  prior <- prior / rowSums(prior)
  if (!is.null(K) && K < ncol(prior)) {
    stop("'K' must be at least the number of groups (", ncol(prior), ").")
  }
  geo_idx <- as.integer(factor(as.character(geo))) - 1L
  ensure_lbisg_python()
  mod <- get_lbisg_module()

  fields <- list(
    surname = lbisg_fit_field(names, NULL, "surname", geo_idx, prior, K,
                              control, mod, verbose,
                              pretrained = pretrained$surname)
  )
  if (!is.null(first.names) && !is.null(pretrained$first)) {
    fields$first <- lbisg_fit_field(first.names, NULL, "first name", geo_idx,
                                    prior, K, control, mod, verbose,
                                    pretrained = pretrained$first)
    post <- as.matrix(mod$combine_fields(
      unname(lapply(fields, `[[`, "posterior")), prior
    ))
  } else {
    if (!is.null(first.names)) {
      warning("'first.names' is ignored because 'pretrained' has no 'first' ",
              "model.")
    }
    post <- fields$surname$posterior
  }
  colnames(post) <- paste0("pred.", colnames(prior))
  structure(
    list(posterior = as.data.frame(post), prior = prior, fields = fields,
         recovered = NULL),
    class = "lbisg"
  )
}


#' Fit list models on name lists alone
#'
#' Fits a network for membership in each list on the names of the lists
#' themselves, with no names from a population: a name is labelled 1 for each
#' list it is on and 0 for the others, so each list is contrasted with the
#' other lists. The number of training epochs is chosen by cross-validation
#' over folds of the names. The models are saved to \code{file} and can be
#' passed to \code{\link{lbisg}} as \code{pretrained}, which then needs no
#' lists.
#'
#' @param lists Named list of character vectors: the name list of each group.
#'   At least two lists are needed.
#' @param file Path of the model file to write (\code{.pt}).
#' @param max.epochs,patience Limits on the number of epochs: training of the
#'   cross-validation models stops once the mean held-out loss has not
#'   improved for \code{patience} epochs, or at \code{max.epochs}.
#' @param batch.size Minibatch size, in names.
#' @param n.folds Number of folds of names for choosing the number of epochs.
#' @param seed Random seed.
#' @param embedding.model Sentence-transformer for the name embeddings; use
#'   the same model when the file is used in \code{\link{lbisg}}.
#' @param verbose Print progress.
#' @return The path of the model file, invisibly.
#' @examples
#' \dontrun{
#' lbisg_train_lists(my_lists, "surname_models.pt")
#' fit <- lbisg(df$surname, df$tract, prior = tract_shares,
#'              pretrained = "surname_models.pt")
#' }
#' @export
lbisg_train_lists <- function(lists, file, max.epochs = 100, patience = 100,
                              batch.size = 64, n.folds = 5, seed = 42,
                              embedding.model =
                                "intfloat/multilingual-e5-large",
                              verbose = TRUE) {
  lists <- lbisg_check_lists(lists, "lists")
  if (length(lists) < 2) {
    stop("At least two lists are needed: each list is contrasted with the ",
         "others.")
  }
  lists <- lapply(lists, function(l) unique(gsub("\\s+", " ", lbisg_clean(l))))
  uniq <- sort(unique(unlist(lists)))
  uniq <- uniq[!is.na(uniq) & uniq != ""]
  ensure_lbisg_python()
  mod <- get_lbisg_module()
  if (verbose) message("Embedding ", length(uniq), " distinct list names...")
  emb <- ebisg_embed_names(uniq,
                           transformer = lbisg_transformer(embedding.model))
  res <- mod$fit_list_model(
    emb = emb, onmat = lbisg_list_matrix(uniq, lists),
    counts = rep(1, length(uniq)), groups = names(lists),
    path = path.expand(file), J = as.integer(n.folds),
    max_epochs = as.integer(max.epochs), patience = as.integer(patience),
    batch_size = as.integer(batch.size), seed = as.integer(seed),
    embedding = lbisg_transformer(embedding.model), verbose = verbose
  )
  invisible(file)
}


# Embedding model: a HuggingFace model ID, or a list with a 'transformer'
# element as for eBISG.
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
    stop("'coarse.map' must specify the coarse group of each list group.")
  }
  if (!all(coarse.map[groups] %in% colnames(coarse.prior))) {
    stop("Every value of 'coarse.map' must be a column of 'coarse.prior'.")
  }
  unlisted <- setdiff(colnames(coarse.prior), coarse.map[groups])
  if (length(unlisted) > 1) {
    stop(
      "Each coarse group requires a list, and only one coarse group may have ",
      "none. ",
      "Coarse groups without a list: ", paste(unlisted, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}


# Each row of a prior must sum to one. A row that sums to less usually means
# that a group is missing, and rescaling the row would inflate the other
# groups.
lbisg_check_shares <- function(x, arg) {
  if (!is.numeric(x) || anyNA(x) || any(x < 0)) {
    stop("'", arg, "' must contain non-negative shares with no missing values.")
  }
  off <- abs(rowSums(x) - 1) > 1e-3
  if (any(off)) {
    stop(
      "Each row of '", arg, "' must sum to 1, with one column for each ",
      "group, including a residual group for everyone else. ", sum(off),
      " of ", nrow(x), " rows do not."
    )
  }
  invisible(TRUE)
}


# Estimation of the prior requires the assumption that the listed groups
# constitute the whole population. Because this assumption cannot be checked,
# the user has to confirm it.
lbisg_check_exhaustive <- function(exhaustive, coarse.prior) {
  if (isTRUE(exhaustive)) return(invisible(TRUE))
  what <- if (is.null(coarse.prior)) {
    "the listed groups constitute the whole population"
  } else {
    "the listed subgroups of each divided coarse group constitute that coarse group"
  }
  stop(
    "Estimating geographic prevalence from the lists assumes that ", what,
    ". Anyone outside the listed groups would be assigned to one of them. If ",
    "necessary, add a residual list (for example, the names in the coarse ",
    "group that appear on none of the subgroup lists), then set ",
    "control = lbisg_control(exhaustive = TRUE)."
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
#' Installs the Python packages that lBISG requires: sentence-transformers,
#' torch, scikit-learn and scipy. Run once before
#' \code{predict_race(..., model = "lBISG")} or \code{\link{lbisg}}. No model
#' weights are downloaded, because the list models are trained on the supplied
#' lists.
#'
#' @param envname Name of the Python virtual environment. Defaults to
#'   \code{"r-ebisg"}, the environment that \code{\link{setup_ebisg}} also
#'   uses.
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
#' lBISG race prediction for \code{predict_race(model = "lBISG")}, with the
#' census race shares at \code{census.geo} as the geographic prior. A race
#' group given as a list of subgroup lists is divided into those subgroups,
#' and the prior for the subgroups is estimated from the census share of the
#' group. Any of whi, bla, his and asi without a list is assigned a list
#' constructed from the Census name table.
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
        "names.to.use includes first names. Supply ",
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
      "To use ", census.geo, " as census.geo, voter.file requires the column(s): ",
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

  ## Standard five columns (a divided group is the sum of the subgroups),
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
# race group of each list (coarse), and the source of each list (user or
# Census). Any of whi, bla, his and asi without a list is assigned a Census
# list.
# If oth has no list, oth is the residual group.
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


#' Name lists constructed from the Census name tables
#'
#' Constructs a list for each group from the Census name tables in wru, as in
#' Chasalow, Dasanaike and Imai (2026). Each list contains the names with
#' \eqn{P(R = r \mid S) \ge} \code{floor}, ranked by count times
#' \eqn{P(R = r \mid S) - P(R = r)}, up to a length of \code{size}.
#' \eqn{P(R \mid S)} is computed from the Census \eqn{P(S \mid R)} and the
#' expected number of people of each race in the data (the column sums of
#' \code{prior}).
#'
#' @param groups Race groups for which lists are constructed (any of whi, bla,
#'   his, asi, oth).
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
      "There is no Census ", field, " name table for ", year, ". Supply ",
      "first-name lists for each group."
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
