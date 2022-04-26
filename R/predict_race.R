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
#' \code{\var{county}}, \code{\var{tract}}, \code{\var{block}}, and/or \code{\var{place}}. 
#' These fields should contain character strings matching U.S. Census categories. 
#' County is three characters (e.g., \code{"031"} not \code{"31"}), 
#' tract is six characters, and block is four characters. Place is five characters. 
#' See below for other optional fields.
#' @param census.surname A \code{TRUE}/\code{FALSE} object. If \code{TRUE}, 
#'  function will call \code{merge_surnames} to merge in Pr(Race | Surname) 
#'  from U.S. Census Surname List (2000 or 2010) and Spanish Surname List. 
#'  If \code{FALSE}, \code{voter.file} object must contain additional fields specifying 
#'  Pr(Race | Surname), named as follows: \code{\var{p_whi}} for Whites, 
#'  \code{\var{p_bla}} for Blacks, \code{\var{p_his}} for Hispanics/Latinos, 
#'  \code{\var{p_asi}} for Asians, and/or \code{\var{p_oth}} for Other. 
#'  Default is \code{TRUE}.
#' @param surname.only A \code{TRUE}/\code{FALSE} object. If \code{TRUE}, race predictions will 
#'  only use surname data and calculate Pr(Race | Surnname). Default is \code{FALSE}.
#' @param surname.year A number to specify the year of the census surname statistics. 
#' These surname statistics is stored in the data, and will be automatically loaded.
#' The default value is \code{2010}, which means the surname statistics from the 
#' 2010 census will be used. Currently, the other available choice is \code{2000}.
#' @param census.geo An optional character vector specifying what level of 
#' geography to use to merge in U.S. Census 2010 geographic data. Currently
#' \code{"county"}, \code{"tract"}, \code{"block"}, and \code{"place"} are supported.
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
#' @param census.key A character object specifying user's Census API 
#'  key. Required if \code{\var{census.geo}} is specified, because 
#'  a valid Census API key is required to download Census geographic data.
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
#' @param party An optional character object specifying party registration field 
#' in \code{\var{voter.file}}, e.g., \code{\var{party} = "PartyReg"}. 
#' If specified, race/ethnicity predictions will be conditioned 
#' on individual's party registration (in addition to geolocation). 
#' Whatever the name of the party registration field in \code{\var{voter.file}}, 
#' it should be coded as 1 for Democrat, 2 for Republican, and 0 for Other.
#' @param retry The number of retries at the census website if network interruption occurs.
#' @param impute.missing See \code{\link[wru]{merge_surnames}}. 
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns with 
#'  predicted probabilities for each of the five major racial categories: 
#'  \code{\var{pred.whi}} for White, 
#'  \code{\var{pred.bla}} for Black, 
#'  \code{\var{pred.his}} for Hispanic/Latino, 
#'  \code{\var{pred.asi}} for Asian/Pacific Islander, and 
#'  \code{\var{pred.oth}} for Other/Mixed.
#'  
#' @examples
#' data(voters)
#' predict_race(voters, surname.only = TRUE)
#' predict_race(voter.file = voters, surname.only = TRUE)
#' \dontrun{predict_race(voter.file = voters, census.geo = "tract", census.key = "...")}
#' \dontrun{predict_race(voter.file = voters, census.geo = "tract", census.key = "...", age = T)}
#' \dontrun{predict_race(voter.file = voters, census.geo = "place", census.key = "...", sex = T)}
#' \dontrun{CensusObj <- get_census_data("...", state = c("NY", "DC", "NJ")); 
#' predict_race(voter.file = voters, census.geo = "tract", census.data = CensusObj, party = "PID")}
#' \dontrun{CensusObj2 <- get_census_data(key = "...", state = c("NY", "DC", "NJ"), age = T, sex = T); 
#' predict_race(voter.file = voters, census.geo = "tract", census.data = CensusObj2, age = T, sex = T)}
#' \dontrun{CensusObj3 <- get_census_data(key = "...", state = c("NY", "DC", "NJ"), census.geo = "place");
#' predict_race(voter.file = voters, census.geo = "place", census.data = CensusObj3)}
#' @export

## Race Prediction Function
predict_race <- function(voter.file, 
                           census.surname = TRUE, surname.only = FALSE, surname.year = 2010, 
                           census.geo, census.key, census.data = NA, age = FALSE, sex = FALSE, party, impute.missing = TRUE, retry = 3) {
  
  if (!missing(census.geo) && (census.geo == "precinct")) {
    # geo <- "precinct"
    stop('Error: census_helper function does not currently support merging precinct-level data.')
  }
  
  vars.orig <- names(voter.file)
  
  if (surname.only == T) {
    message("Proceeding with surname-only predictions...")
    if (!("surname" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named surname")
    }
  } else {
    if (missing(census.geo) || is.null(census.geo) || is.na(census.geo) || census.geo %in% c("county", "tract", "block", "place") == F) {
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
    if (surname.year == 2010) {
      voter.file <- merge_surnames(voter.file, impute.missing = impute.missing)
    } else {
      if (surname.year == 2000) {
        voter.file <- merge_surnames(voter.file, surname.year = surname.year, 
                                     impute.missing = impute.missing)
      } else {
        stop(paste(surname.year, "is not a valid surname.year. It should be either 2000 or 2010 (default)."))
      }
    }
  } else {
    # Check if voter.file has the necessary data
    for (k in 1:length(eth)) {
      if (paste("p", eth[k], sep = "_") %in% names(voter.file) == F) {
        stop(paste("voter.file object needs to have columns named ", paste(paste("p", eth, sep = "_"), collapse = " and "), ".", sep = ""))
      }
    }
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
  if (missing(party) == F) {
    voter.file$PID <- voter.file[, party]
    voter.file <- merge(voter.file, get("pid")[names(get("pid")) %in% "party" == F], by = "PID", all.x = T)  
  }
  
  if (census.geo == "place") {
    if (!("place" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named place.")
    }
    voter.file <- census_helper(key = census.key, 
                                voter.file = voter.file, 
                                states = "all", 
                                geo = "place", 
                                age = age, 
                                sex = sex, 
                                census.data = census.data, retry = retry)
  }
  
  if (census.geo == "block") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% names(voter.file)) || !("block" %in% names(voter.file))) {
      stop("voter.file object needs to have columns named block, tract, and county.")
    }
    voter.file <- census_helper(key = census.key, 
                                voter.file = voter.file, 
                                states = "all", 
                                geo = "block", 
                                age = age, 
                                sex = sex, 
                                census.data = census.data, retry = retry)
  }
  
  if (census.geo == "precinct") {
    geo <- "precinct"
    stop('Error: census_helper function does not currently support precinct-level data.')
  }
  
  if (census.geo == "tract") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% names(voter.file))) {
      stop("voter.file object needs to have columns named tract and county.")
    }
    voter.file <- census_helper(key = census.key, 
                                voter.file = voter.file, 
                                states = "all", 
                                geo = "tract", 
                                age = age, 
                                sex = sex, 
                                census.data = census.data, retry = retry)
  }
  
  if (census.geo == "county") {
    if (!("county" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named county.")
    }
    voter.file <- census_helper(key = census.key, 
                                voter.file = voter.file, 
                                states = "all", 
                                geo = "county", 
                                age = age, 
                                sex = sex, 
                                census.data = census.data, retry = retry)
  }
  
  ## Pr(Race | Surname, Geolocation)
  if (missing(party)) {
    for (k in 1:length(eth)) {
      voter.file[paste("u", eth[k], sep = "_")] <- voter.file[paste("p", eth[k], sep = "_")] * voter.file[paste("r", eth[k], sep = "_")]
    }
    voter.file$u_tot <- apply(voter.file[paste("u", eth, sep = "_")], 1, sum, na.rm = T)
    for (k in 1:length(eth)) {
      voter.file[paste("q", eth[k], sep = "_")] <- voter.file[paste("u", eth[k], sep = "_")] / voter.file$u_tot
    }
  }
  
  ## Pr(Race | Surname, Geolocation, Party)
  if (missing(party) == F) {
    for (k in 1:length(eth)) {
      voter.file[paste("u", eth[k], sep = "_")] <- voter.file[paste("p", eth[k], sep = "_")] * voter.file[paste("r", eth[k], sep = "_")] * voter.file[paste("r_pid", eth[k], sep = "_")]
    }
    voter.file$u_tot <- apply(voter.file[paste("u", eth, sep = "_")], 1, sum, na.rm = T)
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
