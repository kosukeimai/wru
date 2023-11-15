#' Race prediction function.
#'
#' \code{predict_race} makes probabilistic estimates of individual-level race/ethnicity.
#'
#' This function implements the Bayesian race prediction methods outlined in
#' Imai and Khanna (2015). The function produces probabilistic estimates of
#' individual-level race/ethnicity, based on surname, geolocation, and party.
#' @param voter.file An object of class \code{data.frame}.
#' Must contain a row for each individual being predicted,
#' as well as a field named \code{\var{surname}} containing each individual's surname.
#' If using geolocation in predictions, \code{\var{voter.file}} must contain a field named
#' \code{\var{state}}, which contains the two-character abbreviation for each individual's
#' state of residence (e.g., \code{"nj"} for New Jersey).
#' If using Census geographic data in race/ethnicity predictions,
#' \code{\var{voter.file}} must also contain at least one of the following fields:
#' \code{\var{county}}, \code{\var{tract}}, \code{\var{block_group}}, \code{\var{block}}, 
#' and/or \code{\var{place}}.
#' These fields should contain character strings matching U.S. Census categories.
#' County is three characters (e.g., \code{"031"} not \code{"31"}),
#' tract is six characters, block group is usually a single character and block
#'  is four characters. Place is five characters.
#' See below for other optional fields.
#' @param census.surname A \code{TRUE}/\code{FALSE} object. If \code{TRUE},
#'  function will call \code{merge_surnames} to merge in Pr(Race | Surname)
#'  from U.S. Census Surname List (2000, 2010, or 2020) and Spanish Surname List.
#'  If \code{FALSE}, user must provide a \code{name.dictionary} (see below).
#'  Default is \code{TRUE}.
#' @param surname.only A \code{TRUE}/\code{FALSE} object. If \code{TRUE}, race predictions will
#'  only use surname data and calculate Pr(Race | Surname). Default is \code{FALSE}.
#' @param census.geo An optional character vector specifying what level of
#' geography to use to merge in U.S. Census geographic data. Currently
#' \code{"county"}, \code{"tract"}, \code{"block_group"}, \code{"block"}, and \code{"place"} 
#' are supported.
#' Note: sufficient information must be in user-defined \code{\var{voter.file}} object.
#' If \code{\var{census.geo} = "county"}, then \code{\var{voter.file}}
#' must have column named \code{county}.
#' If \code{\var{census.geo} = "tract"}, then \code{\var{voter.file}}
#' must have columns named \code{county} and \code{tract}.
#' And if \code{\var{census.geo} = "block"}, then \code{\var{voter.file}}
#' must have columns named \code{county}, \code{tract}, and \code{block}.
#' If \code{\var{census.geo} = "place"}, then \code{\var{voter.file}}
#' must have column named \code{place}.
#' Specifying \code{\var{census.geo}} will call \code{census_helper} function
#' to merge Census geographic data at specified level of geography.
#' 
#' @param census.key A character object specifying user's Census API key.
#'   Required if `census.geo` is specified, because a valid Census API key is
#'   required to download Census geographic data.
#'   
#'   If [`NULL`], the default, attempts to find a census key stored in an
#'   [environment variable][Sys.getenv] named `CENSUS_API_KEY`.
#'   
#' @param census.data A list indexed by two-letter state abbreviations,
#' which contains pre-saved Census geographic data.
#' Can be generated using \code{get_census_data} function.
#' @param age An optional \code{TRUE}/\code{FALSE} object specifying whether to
#' condition race predictions on age (in addition to surname and geolocation).
#' Default is \code{FALSE}. Must be same as \code{\var{age}} in \code{\var{census.data}} object.
#' May only be set to \code{TRUE} if \code{census.geo} option is specified.
#' If \code{TRUE}, \code{\var{voter.file}} should include a numerical variable \code{\var{age}}.
#' @param sex optional \code{TRUE}/\code{FALSE} object specifying whether to
#' condition race predictions on sex (in addition to surname and geolocation).
#' Default is \code{FALSE}. Must be same as \code{\var{sex}} in \code{\var{census.data}} object.
#' May only be set to \code{TRUE} if \code{census.geo} option is specified.
#' If \code{TRUE}, \code{\var{voter.file}} should include a numerical variable \code{\var{sex}},
#' where \code{\var{sex}} is coded as 0 for males and 1 for females.
#' @param year An optional character vector specifying the year of U.S. Census geographic
#' data to be downloaded. Use \code{"2010"}, or \code{"2020"}. Default is \code{"2020"}.
#' @param party An optional character object specifying party registration field
#' in \code{\var{voter.file}}, e.g., \code{\var{party} = "PartyReg"}.
#' If specified, race/ethnicity predictions will be conditioned
#' on individual's party registration (in addition to geolocation).
#' Whatever the name of the party registration field in \code{\var{voter.file}},
#' it should be coded as 1 for Democrat, 2 for Republican, and 0 for Other.
#' @param retry The number of retries at the census website if network interruption occurs.
#' @param impute.missing Logical, defaults to TRUE. Should missing be imputed?
#' @param use.counties A logical, defaulting to FALSE. Should census data be filtered by counties 
#' available in \var{census.data}?
#' @param model Character string, either "BISG" (default) or "fBISG" (for error-correction, 
#' fully-Bayesian model).
#' @param name.dictionaries Optional named list of \code{data.frame}'s 
#' containing counts of names by race. Any of the following named elements 
#' are allowed: "surname", "first", "middle". When present, the objects must 
#' follow the same structure as \code{last_c}, \code{first_c},
#'\code{mid_c}, respectively.
#' @param names.to.use One of 'surname', 'surname, first', or  'surname, first,
#'  middle'. Defaults to 'surname'.
#' @param race.init Vector of initial race for each observation in voter.file.
#' Must be an integer vector, with 1=white, 2=black, 3=hispanic, 4=asian, and 
#' 5=other. Defaults to values obtained using \code{model="BISG_surname"}.
#' @param control List of control arguments only used when \code{model="fBISG"}, including
#' \itemize{
#'  \item{iter}{ Number of MCMC iterations. Defaults to 1000.}
#'  \item{burnin}{ Number of iterations discarded as burnin. Defaults to half of \code{iter}.}
#'  \item{verbose}{ Print progress information. Defaults to \code{TRUE}.}
#'  \item{me.correct}{ Boolean. Should the model correcting measurement error for \code{races|geo}? Defaults to \code{TRUE}.}
#'  \item{seed}{ RNG seed. If \code{NULL}, a seed is generated and returned as an attribute for reproducibility.}
#' }
#'
#' @return Output will be an object of class \code{data.frame}. It will
#'  consist of the original user-input \code{voter.file} with additional columns with
#'  predicted probabilities for each of the five major racial categories:
#'  \code{\var{pred.whi}} for White,
#'  \code{\var{pred.bla}} for Black,
#'  \code{\var{pred.his}} for Hispanic/Latino,
#'  \code{\var{pred.asi}} for Asian/Pacific Islander, and
#'  \code{\var{pred.oth}} for Other/Mixed.
#'
#' @examples
#' \donttest{
#' #' data(voters)
#' try(predict_race(voter.file = voters, surname.only = TRUE))
#' \dontrun{
#' try(predict_race(voter.file = voters, census.geo = "tract", census.key = "..."))
#' }
#' \dontrun{
#' try(predict_race(
#'   voter.file = voters, census.geo = "place", census.key = "...", year = "2020"))
#' }
#' \dontrun{
#' CensusObj <- try(get_census_data("...", state = c("NY", "DC", "NJ")))
#' try(predict_race(
#'   voter.file = voters, census.geo = "tract", census.data = CensusObj, party = "PID")
#'   )
#' }
#' \dontrun{
#' CensusObj2 <- try(get_census_data(key = "...", state = c("NY", "DC", "NJ"), age = T, sex = T))
#' try(predict_race(
#'   voter.file = voters, census.geo = "tract", census.data = CensusObj2, age = T, sex = T))
#' }
#' \dontrun{
#' CensusObj3 <- try(get_census_data(key = "...", state = c("NY", "DC", "NJ"), census.geo = "place"))
#' try(predict_race(voter.file = voters, census.geo = "place", census.data = CensusObj3))
#' }
#' }

#' @export

predict_race <- function(voter.file, census.surname = TRUE, surname.only = FALSE,
                         census.geo, census.key = NULL, census.data = NULL, age = FALSE,
                         sex = FALSE, year = "2020", party = NULL, retry = 3, impute.missing = TRUE,
                         use.counties = FALSE, model = "BISG", race.init = NULL, name.dictionaries = NULL,
                         names.to.use = "surname", control = NULL) {
  
  message("Predicting race for ", year)
  
  ## Check model type
  if (!(model %in% c("BISG", "fBISG"))) {
    stop(
      paste0(
        "'model' must be one of 'BISG' (for standard BISG results, or results",
        " with all name data without error correction) or 'fBISG' (for the",
        " fully Bayesian/error correction model that accommodates all name data)."
      )
    )
  }
  
  if (any(unique(voter.file$state) %in% c("AS","GU","MP","PR","VI"))) {
    stop(
      paste0(
        "The wru package does not support US territories", 
        " please filter these from your voter.file data")
    )
  }
  
  
  # block_group is missing, pull from block
  if((surname.only == FALSE) && !(missing(census.geo)) && (census.geo == "block_group") && !("block_group" %in% names(voter.file))) {
    voter.file$block_group <- substring(voter.file$block, 1, 1)
  }
  
  # Adjust voter.file with caseid for ordering at the end
  voter.file$caseid <- 1:nrow(voter.file)
  
  if((surname.only==FALSE) && is.null(census.key) && is.null(census.data)) {
    k <- Sys.getenv("CENSUS_API_KEY")
    
    if(k == "") 
      stop(
        "Please provide a valid Census API key using census.key option.",
        " Or set CENSUS_API_KEY in your .Renviron or .Rprofile"
      )
    
    census.key <- k
  }
  
  if(surname.only==FALSE && is.null(census.data)) {
    # Otherwise predict_race_new and predict_race_me will both
    # attempt to pull census_data
    voter.file$state <- toupper(voter.file$state)
    states <- unique(voter.file$state)
    county.list <- split(voter.file$county, voter.file$state)
    county.list <- lapply(county.list, function(x) unique(x))
    census.data <- get_census_data(
      census.key, states, age, 
      sex, year, census.geo, 
      retry, county.list
    )
  }
  
  if((model == "BISG") | (surname.only==TRUE)){
    if((surname.only==TRUE) & (model == "fBISG")){
      warning("Surname-only model only available with model = BISG.")
    }
    preds <- predict_race_new(voter.file = voter.file,
                              names.to.use = names.to.use,
                              year = year,
                              age = age, sex = sex, # not implemented, default to F
                              census.geo = census.geo,
                              census.key = census.key,
                              name.dictionaries = name.dictionaries,
                              surname.only=surname.only,
                              census.data = census.data,
                              retry = retry,
                              impute.missing = impute.missing,
                              census.surname = census.surname,
                              use.counties = use.counties)
  } else {
    ctrl <- list(
      iter = 1000,
      thin = 1,
      verbose = TRUE,
      seed = sample(1:1000, 1) 
    )
    ctrl$burnin <- floor(ctrl$iter / 2)
    ctrl[names(control)] <- control
    ctrl$usr_seed <- ifelse(is.null(control$seed), FALSE, TRUE)

    if (is.null(race.init)) {
      if(ctrl$verbose){
        message("Using `predict_race` to obtain initial race prediction priors with BISG model")
      }
      race.init <-  predict_race_new(voter.file = voter.file,
                                     names.to.use = names.to.use,
                                     year = year,
                                     age = age, sex = sex, # not implemented, default to F
                                     census.geo = census.geo,
                                     census.key = census.key,
                                     name.dictionaries = name.dictionaries,
                                     surname.only=surname.only,
                                     census.data = census.data,
                                     retry = retry,
                                     impute.missing = TRUE,
                                     census.surname = census.surname,
                                     use.counties = use.counties)
      race.init <- max.col(
        race.init[, paste0("pred.", c("whi", "bla", "his", "asi", "oth"))],
        ties.method = "random"
      )
    }
    if (any(is.na(race.init))) {
      stop("Some initial race values are NA.\n
         If you didn't provide initial values, check the results of calling predict_race() on the voter.file you want me to work on.\n
         The most likely reason for getting a missing race prediction is having a geolocation that does not match \n 
         locations on the census. If this problem persists, try impute.missing = TRUE or model = fBISG.")
    }
    
    preds <- predict_race_me(voter.file = voter.file,
                             names.to.use = names.to.use,
                             year = year, age = age, sex = age, 
                             census.geo = census.geo,
                             census.key = census.key,
                             name.dictionaries = name.dictionaries,
                             surname.only = surname.only,
                             census.data = census.data, retry = retry,
                             impute.missing = impute.missing,
                             census.surname = census.surname,
                             use.counties = use.counties, race.init = race.init,
                             ctrl = ctrl)
  }
  seed_attr <- attr(preds, "RNGseed")
  preds <- preds[order(preds$caseid),setdiff(names(preds), "caseid")]
  attr(preds, "RNGseed") <- seed_attr
  preds
}


