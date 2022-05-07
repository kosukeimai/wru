#' Internal model fitting functions
#'
#' These functions are intended for internal use only. Users should use the
#' \code{race_predict} interface rather any of these functions directly.
#'
#' These functions fit different versions of WRU. \code{.predict_race_old} fits
#' the original WRU model, also known as BISG with census-based surname dictionary.
#' \code{.predict_race_new} fits a new version of BISG which uses a new, augmented
#' surname dictionary, and can also accommodate the use of first and middle
#' name information. Finally, \code{.predict_race_me} fits a fully Bayesian Improved
#' Surname Geocoding model (fBISG), which fits a model with measurement-error
#' correction of erroneous zeros in census tables, in addition to also accommodating
#' the augmented surname dictionary, and the first and middle name
#' dictionaries when making predictions.
#'
#' @param voter.file See documentation in \code{race_predict}.
#' @param census.surname See documentation in \code{race_predict}.
#' @param surname.only See documentation in \code{race_predict}.
#' @param surname.year See documentation in \code{race_predict}.
#' @param name.data See documentation in \code{race_predict}.
#' @param census.geo See documentation in \code{race_predict}.
#' @param census.key See documentation in \code{race_predict}.
#' @param census.data See documentation in \code{race_predict}.
#' @param age See documentation in \code{race_predict}.
#' @param sex See documentation in \code{race_predict}.
#' @param year See documentation in \code{race_predict}.
#' @param party See documentation in \code{race_predict}.
#' @param retry See documentation in \code{race_predict}.
#' @param impute.missing See documentation in \code{race_predict}.
#' @param names.to.use See documentation in \code{race_predict}.
#' @param race.init See documentation in \code{race_predict}.
#' @param name.dictionaries See documentation in \code{race_predict}.
#' @param control See documentation in \code{race_predict}.
#' @param use_counties A logical, defaulting to FALSE. Should census data be filtered by counties available in \var{census.data}?
#' @param ... Additional arguments. Currently only useful for \code{.predict_race_me}.
#'
#' @return See documentation in \code{race_predict}.
#'
#' @name modfuns
NULL

#' @section .predict_race_old:
#' Original WRU race prediction function, implementing classical BISG with census-based
#' surname dictionary.
#' @importFrom stats rmultinom
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @rdname modfuns
.predict_race_old <- function(voter.file,
                              census.surname = TRUE, surname.only = FALSE, surname.year = 2010, name.dictionaries = NULL,
                              census.geo, census.key, census.data = NA, age = FALSE, sex = FALSE, year = "2010",
                              party, retry = 3, impute.missing = TRUE, use_counties = FALSE, ...) {

  # warning: 2020 census data only support prediction when both age and sex are equal to FALSE
  if ((sex == TRUE || age == TRUE) && (year == "2020")) {
    stop("Warning: only predictions with both age and sex equal to FALSE are supported when using 2020 census data.")
  }

  if (!missing(census.geo) && (census.geo == "precinct")) {
    # geo <- "precinct"
    stop("Error: census_helper function does not currently support merging precinct-level data.")
  }

  vars.orig <- names(voter.file)

  if (surname.only == TRUE) {
    message("Proceeding with surname-only predictions...")
    if (!("surname" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named surname")
    }
  } else {
    if (missing(census.geo) || is.null(census.geo) || is.na(census.geo) || census.geo %in% c("county", "tract", "block", "place") == FALSE) {
      stop("census.geo must be either 'county', 'tract', 'block', or 'place'")
    } else {
      message(paste("Proceeding with Census geographic data at", census.geo, "level..."))
    }
    if (missing(census.data) || is.null(census.data) || is.na(census.data)) {
      if (missing(census.key) || is.null(census.key) || is.na(census.key)) {
        stop("Please provide a valid Census API key using census.key option.")
      } else {
        message("Downloading Census geographic data using provided API key...")
      }
    } else {
      if (!("state" %in% names(voter.file))) {
        stop("voter.file object needs to have a column named state.")
      }
      if (sum(toupper(unique(as.character(voter.file$state))) %in% toupper(names(census.data)) == FALSE) > 0) {
        message("census.data object does not include all states in voter.file object.")
        if (missing(census.key) || is.null(census.key) || is.na(census.key)) {
          stop("Please provide either a valid Census API key or valid census.data object that covers all states in voter.file object.")
        } else {
          message("Downloading Census geographic data for states not included in census.data object...")
        }
      } else {
        message("Using Census geographic data from provided census.data object...")
      }
    }
  }

  eth <- c("whi", "bla", "his", "asi", "oth")

  ## Merge in Pr(Race | Surname) if necessary
  if (census.surname) {
    if (!(surname.year %in% c(2000, 2010, 2020))) {
      stop(paste(surname.year, "is not a valid surname.year. It should be 2000, 2010 (default) or 2020."))
    }
    voter.file <- merge_surnames(voter.file, surname.year = surname.year, name.data = NULL, impute.missing = impute.missing)
  } else {
    # Check if voter.file has the necessary data
    if (is.null(name.dictionaries) | !("surname" %in% names(name.dictionaries))) {
      stop("User must provide a 'name.dictionaries', with named element 'surname'.")
    }
    for (k in 1:length(eth)) {
      if ((paste("c", eth[k], sep = "_") %in% names(name.dictionaries[["surname"]])) == FALSE) {
        stop(paste("name.dictionaries element 'surname' needs to have columns named ", paste(paste("c", eth, sep = "_"), collapse = " and "), ".", sep = ""))
      }
    }
    name.dictionaries[["surname"]] <- apply(name.dictionaries[["surname"]], 1, function(x) x / sum(x, na.rm = TRUE))
    name.dictionaries[["surname"]][is.na(name.dictionaries[["surname"]])] <- 0
    voter.file <- merge_surnames(voter.file, surname.year = surname.year, name.data = name.dictionaries[["surname"]], impute.missing = impute.missing)
  }

  ## Surname-Only Predictions
  if (surname.only) {
    for (k in 1:length(eth)) {
      voter.file[paste("pred", eth[k], sep = ".")] <- voter.file[paste("p", eth[k], sep = "_")] / apply(voter.file[paste("p", eth, sep = "_")], 1, sum)
    }
    pred <- paste("pred", eth, sep = ".")
    return(voter.file[c(vars.orig, pred)])
  }

  ## Merge in Pr(Party | Race) if necessary
  if (missing(party) == FALSE) {
    voter.file$PID <- voter.file[, party]
    voter.file <- merge(voter.file, get("pid")[names(get("pid")) %in% "party" == F], by = "PID", all.x = TRUE)
  }

  if (census.geo == "place") {
    if (!("place" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named place.")
    }
    voter.file <- census_helper(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "place",
      age = age,
      sex = sex,
      year = year,
      census.data = census.data,
      retry = retry
    )
  }

  if (census.geo == "block") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% names(voter.file)) || !("block" %in% names(voter.file))) {
      stop("voter.file object needs to have columns named block, tract, and county.")
    }
    voter.file <- census_helper(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "block",
      age = age,
      sex = sex,
      year = year,
      census.data = census.data,
      retry = retry,
      use_counties = use_counties
    )
  }

  if (census.geo == "precinct") {
    geo <- "precinct"
    stop("Error: census_helper function does not currently support precinct-level data.")
  }

  if (census.geo == "tract") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% names(voter.file))) {
      stop("voter.file object needs to have columns named tract and county.")
    }
    voter.file <- census_helper(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "tract",
      age = age,
      sex = sex,
      year = year,
      census.data = census.data, 
      retry = retry,
      use_counties = use_counties
    )
  }

  if (census.geo == "county") {
    if (!("county" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named county.")
    }
    voter.file <- census_helper(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "county",
      age = age,
      sex = sex,
      year = year,
      census.data = census.data, 
      retry = retry
    )
  }

  ## Pr(Race | Surname, Geolocation)
  if (missing(party)) {
    for (k in 1:length(eth)) {
      voter.file[paste("u", eth[k], sep = "_")] <- voter.file[paste("p", eth[k], sep = "_")] * voter.file[paste("r", eth[k], sep = "_")]
    }
    voter.file$u_tot <- apply(voter.file[paste("u", eth, sep = "_")], 1, sum, na.rm = TRUE)
    for (k in 1:length(eth)) {
      voter.file[paste("q", eth[k], sep = "_")] <- voter.file[paste("u", eth[k], sep = "_")] / voter.file$u_tot
    }
  }

  ## Pr(Race | Surname, Geolocation, Party)
  if (missing(party) == FALSE) {
    for (k in 1:length(eth)) {
      voter.file[paste("u", eth[k], sep = "_")] <- voter.file[paste("p", eth[k], sep = "_")] * voter.file[paste("r", eth[k], sep = "_")] * voter.file[paste("r_pid", eth[k], sep = "_")]
    }
    voter.file$u_tot <- apply(voter.file[paste("u", eth, sep = "_")], 1, sum, na.rm = TRUE)
    for (k in 1:length(eth)) {
      voter.file[paste("q", eth[k], sep = "_")] <- voter.file[paste("u", eth[k], sep = "_")] / voter.file$u_tot
    }
  }

  for (k in 1:length(eth)) {
    voter.file[paste("pred", eth[k], sep = ".")] <- voter.file[paste("q", eth[k], sep = "_")]
  }
  pred <- paste("pred", eth, sep = ".")

  return(voter.file[c(vars.orig, pred)])
}

#' @section .predict_race_new :
#' New race prediction function, implementing classical BISG with augmented
#' surname dictionary, as well as first and middle name information.
#' @rdname modfuns
.predict_race_new <- function(voter.file, names.to.use, year = "2020",
                              census.geo, census.key, name.dictionaries,
                              census.data, retry = 0, impute.missing = TRUE, 
                              use_counties = FALSE, ...) {

  # check the geography
  if (!missing(census.geo) && (census.geo == "precinct")) {
    stop("Error: census_helper function does not currently support merging precinct-level data.")
  }

  vars.orig <- names(voter.file)

  # check the names
  if (names.to.use == "surname") {
    message("Proceeding with last name-only predictions...")
    if (!("surname" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named 'surname'.")
    }
  } else if (names.to.use == "surname, first") {
    message("Proceeding with first and last name-only predictions...")
    if (!("surname" %in% names(voter.file)) || !("first" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named 'surname' and a column called 'first'.")
    }
  } else if (names.to.use == "surname, first, middle") {
    message("Proceeding with first, last, and middle name predictions...")
    if (!("surname" %in% names(voter.file)) || !("first" %in% names(voter.file)) ||
      !("middle" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named 'surname', a column called 'first', and a column called 'middle'.")
    }
  }

  if (any(!is.null(name.dictionaries))) {
    if (!is.null(name.dictionaries[["surname"]])) {
      stopifnot(identical(names(name.dictionaries[["surname"]]), names(last_c)))
    }
    if (!is.null(name.dictionaries[["first"]])) {
      stopifnot(identical(names(name.dictionaries[["first"]]), names(first_c)))
    }
    if (!is.null(name.dictionaries[["middle"]])) {
      stopifnot(identical(names(name.dictionaries[["middle"]]), names(mid_c)))
    }
  }

  # check the geographies
  if (missing(census.geo) || is.null(census.geo) || is.na(census.geo) || census.geo %in% c("county", "tract", "block", "place") == FALSE) {
    stop("census.geo must be either 'county', 'tract', 'block', or 'place'")
  } else {
    message(paste("Proceeding with Census geographic data at", census.geo, "level..."))
  }
  if (missing(census.data) || is.null(census.data) || is.na(census.data)) {
    if (missing(census.key) || is.null(census.key) || is.na(census.key)) {
      stop("Please provide a valid Census API key using census.key option.")
    } else {
      message("Downloading Census geographic data using provided API key...")
    }
  } else {
    if (!("state" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named state.")
    }
    if (sum(toupper(unique(as.character(voter.file$state))) %in% toupper(names(census.data)) == FALSE) > 0) {
      message("census.data object does not include all states in voter.file object.")
      if (missing(census.key) || is.null(census.key) || is.na(census.key)) {
        stop("Please provide either a valid Census API key or valid census.data object that covers all states in voter.file object.")
      } else {
        message("Downloading Census geographic data for states not included in census.data object...")
      }
    } else {
      message("Using Census geographic data from provided census.data object...")
    }
  }

  eth <- c("whi", "bla", "his", "asi", "oth")

  ## Merge in Pr(Name | Race)
  voter.file <- merge_names(voter.file, names.to.use,
    name.dictionaries[["surname"]], name.dictionaries[["first"]], name.dictionaries[["middle"]],
    clean.names = FALSE, impute.missing
  )

  if (census.geo == "place") {
    if (!("place" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named place.")
    }
    voter.file <- census_helper_new(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "place",
      census.data = census.data, 
      retry = retry,
      use_counties = use_counties
    )
  }

  if (census.geo == "block") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% names(voter.file)) || !("block" %in% names(voter.file))) {
      stop("voter.file object needs to have columns named block, tract, and county.")
    }
    voter.file <- census_helper_new(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "block",
      census.data = census.data, 
      retry = retry,
      use_counties = use_counties
    )
  }

  if (census.geo == "precinct") {
    geo <- "precinct"
    stop("Error: census_helper function does not currently support precinct-level data.")
  }

  if (census.geo == "tract") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% names(voter.file))) {
      stop("voter.file object needs to have columns named tract and county.")
    }
    voter.file <- census_helper_new(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "tract",
      census.data = census.data, 
      retry = retry, 
      use_counties = use_counties
    )
  }

  if (census.geo == "county") {
    if (!("county" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named county.")
    }
    voter.file <- census_helper_new(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "county",
      census.data = census.data, retry = retry
    )
  }

  # Pr(Race | Surname, Geolocation)
  preds <- voter.file[, grep("_last", names(voter.file))] * voter.file[, grep("r_", names(voter.file))]
  if (grepl("first", names.to.use)) {
    preds <- preds * voter.file[, grep("_first", names(voter.file))]
  }
  if (grepl("middle", names.to.use)) {
    preds <- preds * voter.file[, grep("_middle", names(voter.file))]
  }
  preds <- apply(preds, 2, function(x) {
    x / rowSums(preds)
  })
  colnames(preds) <- paste("pred", eth, sep = ".")

  return(data.frame(cbind(voter.file[c(vars.orig)], preds)))
}


#' @section .predict_race_me:
#' New race prediction function, implementing fBISG (i.e. measurement
#' error correction, fully Bayesian model) with augmented
#' surname dictionary, as well as first and middle name information.
#' @rdname modfuns
.predict_race_me <- function(voter.file,
                             names.to.use,
                             census.geo,
                             race.init = NULL,
                             name.dictionaries = NULL,
                             impute.missing = TRUE,
                             control = NULL,
                             ...) {
  ## Form control list
  ctrl <- list(
    iter = 1000,
    thin = 1,
    verbose = TRUE,
    me.correct = TRUE,
    race.marginal = c(.6665, .0853, .1367, .0797, .0318), # TODO: Default for what census year?
    seed = sample(1:1000, 1) 
  )
  ctrl$burnin <- floor(ctrl$iter / 2)
  ctrl[names(control)] <- control

  ## Preliminary Data quality checks
  wru_data_preflight()

  n_race <- 5
  if (!(names.to.use %in% c(c("surname"), c("surname, first"), c("surname, first, middle")))) {
    stop("'names.to.use' must be one of 'surname', 'surname, first', or 'surname, first, middle'")
  }
  name_types <- gsub(" ", "", strsplit(names.to.use, ",")[[1]])
  if (!all(name_types %in% names(voter.file))) {
    stop("When used, 'surname', 'first', and 'middle' must be variable names in voter.file.")
  }
  ## Other quick checks...
  stopifnot(
    census.geo %in% c("county", "tract", "block", "place"),
    all(!is.na(voter.file$last))
  )
  if (!is.logical(ctrl$me.correct)) {
    stop("me.correct must be Boolean.")
  }
  if (any(!is.null(name.dictionaries))) {
    if (!is.null(name.dictionaries[["surname"]])) {
      stopifnot(identical(names(name.dictionaries[["surname"]]), names(last_c)))
      # stopifnot(all(is.integer(name.dictionaries[["surname"]][,-1])))
    }
    if (!is.null(name.dictionaries[["first"]])) {
      stopifnot(identical(names(name.dictionaries[["first"]]), names(first_c)))
      # stopifnot(all(is.integer(name.dictionaries[["first"]][,-1])))
    }
    if (!is.null(name.dictionaries[["middle"]])) {
      stopifnot(identical(names(name.dictionaries[["middle"]]), names(mid_c)))
      # stopifnot(all(is.integer(name.dictionaries[["middle"]][,-1])))
    }
  }

  orig.names <- names(voter.file)
  orig.state <- voter.file$state
  voter.file$state <- toupper(voter.file$state)
  voter.file$rec_id_ <- 1:nrow(voter.file)


  ## Set RNG seed
  set.seed(ctrl$seed)

  ## Initial race
  race_pred_args <- list(
    census.data = NULL,
    census.geo = census.geo,
    census.key = NULL,
    model = "BISG_surname",
    name.dictionaries = name.dictionaries,
    retry = 0
  )
  
  args_usr <- list(...)
  ## level of geo estimation
  geo_id_names <- c("state", switch(race_pred_args$census.geo,
    "county" = c("county"),
    "tract" = c("county", "tract"),
    "block" = c("county", "tract", "block"),
    "place" = c("place"),
    "zipcode" = c("zipcode")
  ))


  race_pred_args[names(args_usr)] <- args_usr
  all_states <- unique(voter.file$state)
  if (is.null(race_pred_args$census.data)) {
    if (is.null(race_pred_args$census.key)) {
      stop("Geographic data is required. When `census.data' is NULL, you must provide a census API Key using `census.key' so I can download the required data.")
    }
    race_pred_args$census.data <- get_census_data(
      race_pred_args$census.key,
      all_states,
      race_pred_args$census.geo,
      race_pred_args$retry
    )
  } else {
    race_pred_args$census.data <- race_pred_args$census.data[all_states]
  }
  race.suff <- c("whi", "bla", "his", "asi", "oth")
  if (is.null(race.init)) {
    if (ctrl$verbose) {
      cat("Using predict_race() to obtain initial race predictions...\n")
    }
    race_pred_args$voter.file <- voter.file
    race_pred <- do.call(predict_race, race_pred_args)
    race.init <- apply(race_pred[, paste0("pred_", race.suff)], 1, which.max)
    rm(race_pred)
  }
  if (any(is.na(race.init))) {
    stop("Some initial race values are NA.\n
         If you didn't provide initial values, check the results of calling predict_race() on the voter.file you want me to work on.\n
         The most likely reason for getting a missing race prediction is having a missing geolocation value.")
  }
  geo_id <- do.call(paste, voter.file[, geo_id_names])

  if (ctrl$verbose) {
    cat("Forming Pr(race | location) tables from census data...\n")
  }
  tmp_tabs <- lapply(
    race_pred_args$census.data,
    function(x) {
      all_names <- names(x[[race_pred_args$census.geo]])
      tmp <- x[[race_pred_args$census.geo]][, c(geo_id_names, grep("P00", all_names, value = TRUE))]
      tmp$r_whi <- tmp$P005003
      tmp$r_bla <- tmp$P005004
      tmp$r_his <- tmp$P005010
      tmp$r_asi <- (tmp$P005006 + tmp$P005007)
      tmp$r_oth <- (tmp$P005005 + tmp$P005008 + tmp$P005009)
      tmp$ccc <- 1:nrow(tmp)
      all_names <- names(tmp)
      ## Totals
      tmp_la <- tmp[, c(geo_id_names, grep("r_", all_names, value = TRUE))]
      return(list(tots = tmp_la))
    }
  )
  N_rg <- do.call(rbind, lapply(tmp_tabs, function(x) x$tots))
  N_rg_geo <- do.call(paste, N_rg[, geo_id_names])
  N_rg <- N_rg[N_rg_geo %in% geo_id, ]
  rm(race_pred_args)
  ## Subset to geo's in vf
  if (ctrl$me.correct) {
    zero_ind <- rowSums(N_rg[, grep("r_", names(N_rg), value = TRUE)]) < 1
    n_zero <- sum(zero_ind)
    N_rg[zero_ind, grep("r_", names(N_rg), value = TRUE)] <- t(rmultinom(n_zero, 1, ctrl$race.marginal))
    alpha_ <- t(apply(
      as.matrix(N_rg[, grep("r_", names(N_rg), value = TRUE)]), 1,
      function(x) {
        oprop <- x / sum(x)
        zero_ind <- x == 0
        x[zero_ind] <- 0.5 / (sum(x) + 5 / 2)
        zeta_n <- sum(x[zero_ind])
        x[!zero_ind] <- oprop[!zero_ind] * (1.0 - zeta_n)
        return(x)
      }
    ))
  } else {
    alpha_ <- apply(as.matrix(N_rg[, grep("r_", names(N_rg), value = TRUE)]), 1, function(x) x / sum(x, na.rm = TRUE))
    alpha_[is.na(alpha_)] <- 0
  }
  if (nrow(N_rg) != length(unique(geo_id))) {
    stop("Some records in voter.file have geographic locations that I wasn't able to find in the census.data.\n
          Records may have mis-matched geographic units that do not exist in the census.")
  }


  ## Split data by geographic cluster
  voter.file$state_cluster <- geo_id
  N_rg$state_cluster <- do.call(paste, N_rg[, geo_id_names])
  alpha_ <- split(as.data.frame(alpha_), N_rg$state_cluster)
  N_rg <- split(N_rg, N_rg$state_cluster)
  geo_id <- split(geo_id, voter.file$state_cluster)
  r_g_t <- mapply(function(tab_, tot_, gid_, g_n_) {
    Nrg_geo_new <- do.call(paste, tot_[, g_n_])
    geo_ <- match(gid_, Nrg_geo_new)
    tot_ <- t(tot_[, grep("r_", colnames(tot_))]) ## Races in rows
    tab_ <- t(tab_[, grep("r_", colnames(tab_))]) ## ditto
    return(list(
      geo_ = geo_,
      alpha_ = tab_,
      N_rg_ = tot_
    ))
  }, alpha_, N_rg, geo_id,
  MoreArgs = list(g_n_ = geo_id_names),
  SIMPLIFY = FALSE
  )

  orig_ord <- split(voter.file$rec_id_, voter.file$state_cluster)
  n_groups <- length(orig_ord)
  ## Create name indeces
  name_data <- vector("list", 3)
  names(name_data) <- c("surname", "first", "middle")
  if (ctrl$verbose) {
    cat("Pre-processing names...\n")
  }
  for (ntype in c("surname", "first", "middle")) {
    if (ntype %in% name_types) {
      ntab <- switch(ntype,
        surname = as.data.frame(if (!is.null(name.dictionaries[["surname"]])) {
          name.dictionaries[["surname"]]
        } else {
          last_c
        }),
        first = as.data.frame(if (!is.null(name.dictionaries[["first"]])) {
          name.dictionaries[["first"]]
        } else {
          first_c
        }),
        middle = as.data.frame(if (!is.null(name.dictionaries[["middle"]])) {
          name.dictionaries[["middle"]]
        } else {
          mid_c
        })
      )
      kw_names <- toupper(ntab[, 1])
      proc_names_vf <- .name_preproc(voter.file[, ntype], c(kw_names))
      u_vf_names <- unique(proc_names_vf)
      kw_in_vf <- kw_names %in% proc_names_vf
      n_miss_r <- sum(!(proc_names_vf %in% kw_names)) * ctrl$race.marginal
      u_kw <- kw_names[kw_in_vf]
      n_u_kw <- length(u_kw)
      reord <- order(match(u_vf_names, u_kw))
      u_vf_names <- u_vf_names[reord]
      w_names <- match(proc_names_vf, u_vf_names)
      w_names <- split(w_names, voter.file$state_cluster)
      M_ <- as.matrix(ntab[which(kw_in_vf), -1])
      pi_ <- apply(M_, 2, function(x) x / sum(x, na.rm = TRUE))
      if (impute.missing) {
        pi_miss <- colMeans(pi_, na.rm = TRUE)
      } else {
        pi_miss <- rep(1, n_race)
      }
      pi_[is.na(pi_)] <- 0
      name_data[[ntype]] <- list(
        record_name_id = w_names,
        pi_ = t(pi_),
        pi_miss = pi_miss
      )
    } else {
      name_data[[ntype]] <- list(
        record_name_id = replicate(n_groups, vector("integer"), simplify = FALSE),
        pi_ = matrix(NA, 0, 0),
        pi_miss = array(NA, 5)
      )
    }
  }

  ## Build missing distribution
  pi_miss <- do.call(cbind, lapply(name_data, function(x) {
    x$pi_miss
  }))

  ## Split inits by cluster
  race.init <- split(race.init, voter.file$state_cluster)


  ## Name selector
  which.names <- switch(names.to.use,
    "surname" = 0L,
    "surname, first" = 1L,
    "surname, first, middle" = 2L
  )

  ## Run Gibbs sampler
  if (ctrl$verbose) {
    cat("Sampling races...\n")
    pb <- txtProgressBar(min = 0, max = n_groups, style = 3)
  }
  race_samples <- lapply(
    seq.int(n_groups),
    function(cluster) {
      tmp <- sample_me(
        name_data[["surname"]]$record_name_id[[cluster]] - 1L,
        name_data[["first"]]$record_name_id[[cluster]] - 1L,
        name_data[["middle"]]$record_name_id[[cluster]] - 1L,
        r_g_t[[cluster]]$geo_ - 1L,
        r_g_t[[cluster]]$N_rg_,
        r_g_t[[cluster]]$alpha_,
        name_data[["surname"]]$pi_,
        name_data[["first"]]$pi_,
        name_data[["middle"]]$pi_,
        pi_miss,
        which.names,
        ctrl$iter,
        ctrl$burnin,
        ctrl$me.correct,
        race.init[[cluster]] - 1L,
        0
      )
      if (ctrl$verbose) {
        setTxtProgressBar(pb, cluster)
      }
      return(cbind(orig_ord[[cluster]], tmp))
    }
  )
  if (ctrl$verbose) {
    close(pb)
  }
  if (ctrl$verbose) {
    cat("Post-processing results and wrapping up.\n")
  }
  ## Get posterior race probabilities and append to voter.file
  race_samples <- do.call(rbind, race_samples)
  race_samples <- race_samples[order(race_samples[, 1]), -1]
  race_probs <- proportions(race_samples, 1)
  colnames(race_probs) <- paste0("pred.", race.suff)
  voter.file <- cbind(voter.file[, orig.names], race_probs)
  voter.file$state <- orig.state
  attr(voter.file, "RNGseed") <- ctrl$seed

  ## Return expanded voter.file with RNG see attribute
  return(voter.file)
}
