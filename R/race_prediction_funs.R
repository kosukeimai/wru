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
#' @param ctrl See \code{control} in documentation for \code{race_predict}.
#' @param use.counties A logical, defaulting to FALSE. Should census data be filtered by counties available in \var{census.data}?
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
#' @keywords internal

.predict_race_old <- function(voter.file,
                              census.surname = TRUE, surname.only = FALSE, surname.year = 2010, name.dictionaries = NULL,
                              census.geo, census.key, census.data = NULL, age = FALSE, sex = FALSE, year = "2020",
                              party, retry = 3, impute.missing = TRUE, use.counties = FALSE) {
  
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
  
  if (census.geo == "block_group") {
    if (!("block_group" %in% names(voter.file)) || !("county" %in% names(voter.file)) || !("tract" %in% names(voter.file))) {
      stop("voter.file object needs to have columns named block, tract, and county.")
    }
    voter.file <- census_helper(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = "block_group",
      age = age,
      sex = sex,
      year = year,
      census.data = census.data,
      retry = retry,
      use.counties = use.counties
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
      use.counties = use.counties
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
      use.counties = use.counties
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
predict_race_new <- function(voter.file, names.to.use, year = "2020",age = FALSE, sex = FALSE, 
                             census.geo, census.key = NULL, name.dictionaries, surname.only=FALSE,
                             census.data = NULL, retry = 0, impute.missing = TRUE, census.surname = FALSE,
                             use.counties = FALSE) {
  
  # Check years
  if (!(year %in% c("2000", "2010", "2020"))){
    stop("Year should be one of 2000, 2010, or 2020.")
  }
  # Define 2020 race marginal
  race.margin <- c(r_whi=0.5783619, r_bla=0.1205021, r_his=0.1872988,
                   r_asi=0.06106737, r_oth=0.05276981)
  # check the geography
  if (!missing(census.geo) && (census.geo == "precinct")) {
    stop("Error: census_helper function does not currently support merging precinct-level data.")
  }
  
  vars.orig <- names(voter.file)
  
  # check the names
  if (names.to.use == "surname") {
    message("Proceeding with last name predictions...")
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
  
  ## Preliminary Data quality checks
  wru_data_preflight()

  path <- ifelse(getOption("wru_data_wd", default = FALSE), getwd(), tempdir())

  first_c <- readRDS(paste0(path, "/wru-data-first_c.rds"))
  mid_c <- readRDS(paste0(path, "/wru-data-mid_c.rds"))
  if(census.surname){
    last_c <- readRDS(paste0(path, "/wru-data-census_last_c.rds"))
  } else {
    last_c <- readRDS(paste0(path, "/wru-data-last_c.rds"))
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
  if (surname.only == FALSE) {
    if (!(census.geo %in% c("county", "tract","block_group", "block", "place"))) {
      stop("census.geo must be either 'county', 'tract', 'block', 'block_group', or 'place'")
    } else {
      message(paste("Proceeding with Census geographic data at", census.geo, "level..."))
    }
    if (is.null(census.data)) {
      if (missing(census.key) || is.null(census.key) || is.na(census.key)) {
        stop("Please provide a valid Census API key using census.key option.")
      } else {
        message("Downloading Census geographic data using provided API key...")
      }
    } else {
      if (!("state" %in% names(voter.file))) {
        stop("voter.file object needs to have a column named state.")
      }
      census_data_preflight(census.data, census.geo, year)
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
    
    geo_id_names <- switch(
      census.geo,
      "county" = c("county"),
      "tract" = c("county", "tract"),
      "block_group" = c("county", "tract", "block_group"),
      "block" = c("county", "tract", "block"),
      "place" = c("place")
    )
    
    if (!all(geo_id_names %in% names(voter.file))) {
      stop(message("To use",census.geo,"as census.geo, voter.file needs to include the following column(s):",
               paste(geo_id_names, collapse=", ")))
    }
    
    voter.file <- census_helper_new(
      key = census.key,
      voter.file = voter.file,
      states = "all",
      geo = census.geo,
      age = age, 
      sex = sex,
      year = year,
      census.data = census.data, 
      retry = retry,
      use.counties = use.counties
    )
  }
  
  eth <- c("whi", "bla", "his", "asi", "oth")
  
  ## Merge in Pr(Name | Race)
  voter.file <- merge_names(voter.file = voter.file,
                            namesToUse = names.to.use,
                            census.surname = census.surname, 
                            table.surnames=name.dictionaries[["surname"]], 
                            table.first=name.dictionaries[["first"]],
                            table.middle=name.dictionaries[["middle"]],
                            clean.names = TRUE,
                            impute.missing = impute.missing,
                            model = 'BISG')
  
  if (surname.only == TRUE) {
    # Pr(Race | Surname)
    preds <- voter.file[, grep("_last$", names(voter.file))] * 
      matrix(race.margin, nrow=nrow(voter.file), ncol=length(race.margin), byrow = TRUE)
  } else {
    # Pr(Race | Surname, Geolocation)
    preds <-  voter.file[, grep("_last$", names(voter.file))]  * voter.file[, grep("^r_", names(voter.file))]
    if (grepl("first", names.to.use)) {
      preds <- preds * voter.file[, grep("_first$", names(voter.file))]
    }
    if (grepl("middle", names.to.use)) {
      preds <- preds * voter.file[, grep("_middle$", names(voter.file))]
    }
  }
  
  ## Normalize (recycle marginal)
  preds <- preds/rowSums(preds)
  ## Revert to Pr(Race|Surname) for missing predictions
  if(impute.missing){
    miss_ind <- !is.finite(preds$c_whi_last)
    if(any(miss_ind)){
      preds[miss_ind,] <- voter.file[miss_ind, grep("_last$", names(voter.file))] * 
        matrix(race.margin, nrow=nrow(voter.file[miss_ind,]), ncol=length(race.margin), byrow = TRUE)
    }
  }
  colnames(preds) <- paste("pred", eth, sep = ".")
  
  return(data.frame(cbind(voter.file[c(vars.orig)], preds)))
}


#' @section .predict_race_me:
#' New race prediction function, implementing fBISG (i.e. measurement
#' error correction, fully Bayesian model) with augmented
#' surname dictionary, as well as first and middle name information.
#' @rdname modfuns
predict_race_me <- function(voter.file, names.to.use, year = "2020",age = FALSE, sex = FALSE, 
                            census.geo, census.key, name.dictionaries, surname.only=FALSE,
                            census.data = NULL, retry = 0, impute.missing = TRUE, census.surname = FALSE,
                            use.counties = FALSE, race.init, ctrl) 
{
  if(!is.null(census.data)) {
    census_data_preflight(census.data, census.geo, year)
  }
  
  n_race <- 5
  if (!(names.to.use %in% c(c("surname"), c("surname, first"), c("surname, first, middle")))) {
    stop("'names.to.use' must be one of 'surname', 'surname, first', or 'surname, first, middle'")
  }
  name_types <- gsub(" ", "", strsplit(names.to.use, ",")[[1]])
  if (!all(name_types %in% names(voter.file))) {
    stop("When used, 'surname', 'first', and 'middle' must be variable names in voter.file.")
  }
  
  ## Preliminary Data quality checks
  wru_data_preflight()
  path <- ifelse(getOption("wru_data_wd", default = FALSE), getwd(), tempdir())
  
  if(census.surname){
    last_c <- readRDS(paste0(path, "/wru-data-census_last_c.rds"))
  } else {
    last_c <- readRDS(paste0(path, "/wru-data-last_c.rds"))
  }
  if (!is.null(name.dictionaries[["surname"]])) {
    stopifnot(identical(names(name.dictionaries[["surname"]]), names(last_c)))
    last_c <- name.dictionaries[["surname"]]
  } 
  if("first" %in% name_types){
    first_c <- readRDS(paste0(path, "/wru-data-first_c.rds"))
    if (!is.null(name.dictionaries[["first"]])){
      stopifnot(identical(names(name.dictionaries[["first"]]), names(first_c)))
      first_c <- name.dictionaries[["first"]]
    }
  } 
  if("middle" %in% name_types){
    mid_c <- readRDS(paste0(path, "/wru-data-mid_c.rds"))
    if (!is.null(name.dictionaries[["middle"]])){
      stopifnot(identical(names(name.dictionaries[["middle"]]), names(mid_c)))
      mid_c <- name.dictionaries[["middle"]]
    }
  } 
  
  ## Other quick checks...
  if (!(census.geo %in% c("county", "tract","block_group", "block", "place"))) {
    stop("census.geo must be either 'county', 'tract', 'block', 'block_group', or 'place'")
  }
  stopifnot(
    all(!is.na(voter.file$surname))
  )
  
  orig.names <- names(voter.file)
  orig.state <- voter.file$state
  voter.file$state <- toupper(voter.file$state)
  voter.file$rec_id_ <- 1:nrow(voter.file)
  
  
  ## Set RNG seed
  set.seed(ctrl$seed)
  if(!(ctrl$usr_seed) & (ctrl$verbose)){
    message("fBISG relies on MCMC; for reproducibility, I am setting RNG seed and returning it as attribute 'RNGseed'.\n",
            "To silence this message, you can set a seed explicitly by defining the 'seed' element in the control list.")
  }
  
  ## Initial race
  race_pred_args <- list(
    census.data = NULL,
    names.to.use = names.to.use,
    census.geo = census.geo,
    census.key = NULL,
    model = "BISG",
    name.dictionaries = name.dictionaries,
    retry = 0
  )
  
  ## level of geo estimation
  geo_id_names <- c("state", switch(census.geo,
                                    "county" = c("county"),
                                    "tract" = c("county", "tract"),
                                    "block_group" = c("county", "tract", "block_group"),
                                    "block" = c("county", "tract", "block"),
                                    "place" = c("place"),
                                    "zipcode" = c("zipcode")
  ))
  
  
  #race_pred_args[names(args_usr)] <- args_usr
  all_states <- unique(voter.file$state)
  census.data <- census.data[all_states]
  race.suff <- c("whi", "bla", "his", "asi", "oth")

  geo_id <- do.call(paste, voter.file[, geo_id_names])
  
  if (ctrl$verbose) {
    message("Forming Pr(race | location) tables from census data...\n")
  }
  if(year == "2020") {
    vars_ <- c(
      pop_white = 'P2_005N', pop_black = 'P2_006N',
      pop_aian = 'P2_007N', pop_asian = 'P2_008N', 
      pop_nhpi = 'P2_009N', pop_other = 'P2_010N', 
      pop_two = 'P2_011N', pop_hisp = 'P2_002N'
    ) 
  } else {
    vars_ <- c(
      pop_white = 'P005003', pop_black = 'P005004',
      pop_aian = 'P005005', pop_asian = 'P005006',
      pop_nhpi = 'P005007', pop_other = 'P005008', 
      pop_two = 'P005009', pop_hisp = 'P005010'
    )
  }
  tmp_tabs <- lapply(
    census.data,
    function(x) {
      all_names <- names(x[[census.geo]])
      tmp <- x[[census.geo]][, c(geo_id_names, grep("P00|P2_0", all_names, value = TRUE))]
      tmp$r_whi <- tmp[, vars_["pop_white"]]
      tmp$r_bla <- tmp[, vars_["pop_black"]]
      tmp$r_his <- tmp[, vars_["pop_hisp"]]
      tmp$r_asi <- (tmp[, vars_["pop_asian"]] + tmp[, vars_["pop_nhpi"]])
      tmp$r_oth <- (tmp[, vars_["pop_aian"]] + tmp[, vars_["pop_other"]] + tmp[, vars_["pop_two"]])
      all_names <- names(tmp)
      ## Totals
      tmp_la <- tmp[, c(geo_id_names, grep("^r_", all_names, value = TRUE))]
      return(list(tots = tmp_la))
    }
  )
  N_rg <- do.call(rbind, lapply(tmp_tabs, function(x) x$tots))
  N_rg_geo <- do.call(paste, N_rg[, geo_id_names])
  ## Subset to geo's in vf
  N_rg <- N_rg[N_rg_geo %in% geo_id, ]
  rm(race_pred_args)
  if (nrow(N_rg) != length(unique(geo_id))) {
    stop("Some records in voter.file have geographic locations that I wasn't able to find in the census.data.\n
          Records may have mis-matched geographic units that do not exist in the census.")
  }
  
  
  ## Split data by geographic cluster
  voter.file$state_cluster <- geo_id
  N_rg$state_cluster <- do.call(paste, N_rg[, geo_id_names])
  N_rg <- split(N_rg, N_rg$state_cluster)
  geo_id <- split(geo_id, voter.file$state_cluster)
  r_g_t <- mapply(function(tot_, gid_, g_n_) {
    Nrg_geo_new <- do.call(paste, tot_[, g_n_])
    geo_ <- match(gid_, Nrg_geo_new)
    tot_ <- t(tot_[, grep("^r_", colnames(tot_))]) ## Races in rows
    return(list(
      geo_ = geo_,
      #alpha_ = as.matrix(tab_),
      N_rg_ = tot_
    ))
  }, N_rg, geo_id,
  MoreArgs = list(g_n_ = geo_id_names),
  SIMPLIFY = FALSE
  )
  
  orig_ord <- split(voter.file$rec_id_, voter.file$state_cluster)
  n_groups <- length(orig_ord)
  ## Create name indeces
  name_data <- vector("list", 3)
  names(name_data) <- c("surname", "first", "middle")
  if (ctrl$verbose) {
    message("Pre-processing names...\n")
  }
  for (ntype in c("surname", "first", "middle")) {
    if (ntype %in% name_types) {
      ntab <- switch(ntype,
                     surname = last_c,
                     first = first_c,
                     middle = mid_c)
      kw_names <- toupper(ntab[, 1])
      proc_names_vf <- .name_preproc(voter.file[[ntype]], c(kw_names))
      u_vf_names <- unique(proc_names_vf)
      kw_in_vf <- kw_names %in% proc_names_vf
      u_kw <- kw_names[kw_in_vf]
      n_u_kw <- length(u_kw)
      reord <- order(match(u_vf_names, u_kw))
      u_vf_names <- u_vf_names[reord]
      w_names <- match(proc_names_vf, u_vf_names)
      w_names <- split(w_names, voter.file$state_cluster)
      pi_ <- as.matrix(ntab[which(kw_in_vf), -1])
      #pi_ <- apply(M_, 2, function(x) x / sum(x, na.rm = TRUE))
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
  pi.miss <- do.call(cbind, lapply(name_data, function(x) {
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
    message("Sampling races...\n")
    pb <- txtProgressBar(min = 0, max = n_groups, style = 3)
  }
  race_samples <- lapply(seq.int(n_groups),
                         function(cluster) {
                           tmp <- sample_me(
                             name_data[["surname"]]$record_name_id[[cluster]] - 1L,
                             name_data[["first"]]$record_name_id[[cluster]] - 1L,
                             name_data[["middle"]]$record_name_id[[cluster]] - 1L,
                             r_g_t[[cluster]]$geo_ - 1L,
                             r_g_t[[cluster]]$N_rg_,
                             name_data[["surname"]]$pi_,
                             name_data[["first"]]$pi_,
                             name_data[["middle"]]$pi_,
                             pi.miss,
                             which.names,
                             ctrl$iter,
                             ctrl$burnin,
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
    message("Post-processing results and wrapping up.\n")
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
