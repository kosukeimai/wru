#' Race prediction function.
#'
#' \code{predict_race} makes probabilistic estimates of individual-level race/ethnicity.
#'
#' This function implements the Bayesian race prediction methods outlined in 
#' Imai and Khanna (2015). The function produces probabilistic estimates of 
#' individual-level race/ethnicity, based on surname, geolocation, and party.
#' @param voter.file An object of class \code{data.frame}. Must contain a row for each individual being 
#' predicted, as well as a field named \code{\var{surname}} containing each individual's surname.
#' If using geolocation in predictions, \code{\var{voter.file}} must contain a field named \code{\var{state}}, 
#' which contains the two-character abbreviation for each individual's state of residence (e.g., "nj" for New Jersey). 
#' If using geolocation, \code{\var{voter.file}} must also contain at least one of the following fields: 
#' \code{\var{county}}, \code{\var{tract}}, and/or \code{\var{block}}. 
#' These fields should contain character strings matching U.S. Census categories. 
#' County is three characters (e.g., "031" not "31"), tract is six characters, and block is four characters. 
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
#' geography to use to merge in U.S. Census 2010 data. Can be one of 
#' \code{"county"}, \code{"tract"}, or \code{"block"}. 
#' Note: sufficient information must be in the input data, voter.file. 
#' For example when census.geo is \code{tract}, voter.file must have columns 
#' named \code{county} and \code{tract}.
#' Function calls \code{census_helper_api} to merge in Census data at specified level. 
#' If left unspecified, \code{voter.file} must contain additional fields 
#' specifying Pr(Geolocation | Race), including any of the following: 
#' \code{\var{r_whi}}, \code{\var{r_bla}}, \code{\var{r_his}}, 
#' \code{\var{r_asi}}, and/or \code{\var{r_oth}}. 
#' @param census.key A character object specifying user's Census API 
#'  key. Required if \code{census} is specified, because 
#'  \code{census.helper} function requires a Census API key to operate.
#' @param census.data A Census data object, a list indexed by state names, 
#' which contains Census geographic data.
#' @param demo An optional \code{TRUE}/\code{FALSE} object specifying whether to 
#' condition race predictions on individual age and sex (in addition to geolocation). 
#' Default is \code{FALSE}. 
#' May only be set to \code{TRUE} if \code{census} option is specified. 
#' If \code{TRUE}, \code{voter.file} should include numerical variables 
#' \code{\var{age}} and \code{\var{sex}}, where \code{\var{sex}} coded as 0 for 
#' males and 1 for females.
#' @param party An optional character object specifying party registration field in \code{\var{voter.file}}, 
#' e.g., \code{\var{party} = "PartyReg"}. If specified, race/ethnicity predictions will be conditioned 
#' on individual's party registration (in addition to geolocation). 
#' Whatever the name of the party registration field in \code{\var{voter.file}}, 
#' it should be coded as 1 for Democrat, 2 for Republican, and 0 for Other.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns that 
#'  contain predicted probabilities for each race in \code{races}.
#'
#' @examples
#' data(voters)
#' predict_race(voters, surname.only = T)
#' predict_race(voter.file = voters, races = "asian", surname.only = TRUE)
#' \dontrun{predict_race(voter.file = voters, races = c("white", "black", "latino"), 
#' census.geo = "tract", census.key = "...", demo = TRUE)}
#' \dontrun{predict_race(voter.file = voters, races = c("white", "black", "latino", "asian", "other"), 
#' census.geo = "tract", census.key = "...", party = "PID")}
#' \dontrun{censusObj <- get_census_data("...", state = c("NY", "DC", "NJ")); 
#' predict_race(voter.file = voters, races = c("white", "black", "latino", "asian", "other"), 
#' census.geo = "tract", census.data = censusObj, party = "PID")}
#' \dontrun{censusObj <- get_census_data("...", state = c("NY", "DC", "NJ"), demo = TRUE); 
#' predict_race(voter.file = voters, races = c("white", "black", "latino", "asian", "other"), 
#' census.geo = "tract", census.data = censusObj, party = "PID", demo = TRUE)}
#' @export

## Race Prediction Function
predict_race <- function(voter.file, 
                      census.surname = TRUE, surname.only = FALSE, surname.year = 2010, 
                      census.geo, census.key, census.data = NA, demo = FALSE, party) {
  
  if (!missing(census.geo) && (census.geo == "precinct")) {
    # geo <- "precinct"
    stop('Error: census.helper function does not currently support merging precinct-level data.')
  }
  
  vars.orig <- names(voter.file)
  
  if (surname.only == T) {
    print("Proceeding with surname-only predictions...")
    if (!("surname" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named surname")
    }
    if (demo == TRUE) {
      if (missing(census.geo) || is.null(census.geo) || is.na(census.geo) || census.geo %in% c("county", "tract", "block") == F) {
        stop('Cannot set demo to TRUE without specifying census option')
      } else {
        print(paste("Proceeding with census.geo = ", census.geo," and demo = ", demo, "...", sep = ""))
      }
    }
  } else {
    print("Proceeding to use Census geographic data to predict race/ethnicity...")
    if (missing(census.geo) || is.null(census.geo) || is.na(census.geo) || census.geo %in% c("county", "tract", "block") == F) {
      stop("census.geo must be either 'county', 'tract', or 'block'")
    } else {
      print(paste("Proceeding with census.geo = ", census.geo,"...", sep = ""))
    }
    if (missing(census.data) || is.null(census.data) || is.na(census.data)) {
      if (missing(census.key) || is.null(census.key) || is.na(census.key)) {
        stop("Need to provide a valid Census API key")
      } else {
        print("Proceeding with provided Census API key to download Census geographic data...")
      }
    } else {
      if (!("state" %in% names(voter.file))) {
        stop("Voter data frame needs to have a column named state.")
      }
      if (sum(toupper(unique(as.character(voter.file$state))) %in% toupper(names(census.data)) == FALSE) > 0) {
        print("census.data object does not include all states in voter.file object...")
        if (missing(census.key) || is.null(census.key) || is.na(census.key)) {
          stop("Need to provide either a valid Census API key, or a valid census object that covers all the states (using two-letter abbreviations) in the voter data")
        } else {
          print("Proceeding with provided Census API key to download the Census geographic data for states not included in census.data object...")
        }
      } else {
        print("Proceeding by using Census statistics from provided census.data object ...")
      }
    }
  }

  eth <- c("whi", "bla", "his", "asi", "oth")

  ## Merge in Pr(Race | Surname) if necessary
  if (census.surname) {
    if (surname.year == 2010) {
      voter.file <- merge_surnames(voter.file)
    } else {
      if (surname.year == 2000) {
        voter.file <- merge_surnames(voter.file, surname.year = surname.year)
      } else {
        stop(paste(surname.year, "is not a valid surname.year. It should be either 2000 or 2010 (default)."))
      }
    }
  } else {
    # Check if voter.file has the nessary data
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
  
  if (census.geo == "block") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% names(voter.file)) || !("block" %in% names(voter.file))) {
      stop("voter.file needs to have columns named block, tract, and county.")
    }
    oldw <- getOption("warn")
    options(warn = -1)
    warning("Extracting U.S. Census 2010 block-level data -- may take a long time!")

    voter.file <- census_helper_api(key = census.key, 
                                    voter.file = voter.file, 
                                    states = "all", 
                                    geo = "block", 
                                    demo = demo,
                                    census.data = census.data)
    options(warn = oldw)
  }

  if (census.geo == "precinct") {
    geo <- "precinct"
    stop('Error: census.helper function does not currently support merging precinct-level data.')
  }

  if (census.geo == "tract") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% names(voter.file))) {
      stop("voter.file needs to have columns named tract and county.")
    }
    oldw <- getOption("warn")
    options(warn = -1)
    warning("Extracting U.S. Census 2010 tract-level data -- may take a long time!")
    
    voter.file <- census_helper_api(key = census.key, 
                                     voter.file = voter.file, 
                                     states = "all", 
                                     geo = "tract", 
                                     demo = demo, 
                                     census.data = census.data)
    options(warn = oldw)
  }
  
  if (census.geo == "county") {
    if (!("county" %in% names(voter.file))) {
      stop("voter.file needs to have a column named county.")
    }
    oldw <- getOption("warn")
    options(warn = -1)
    warning("Extracting U.S. Census 2010 county-level data -- may take a long time!")
    
    voter.file <- census_helper_api(key = census.key, 
                                     voter.file = voter.file, 
                                     states = "all", 
                                     geo = "county", 
                                     demo = demo, 
                                     census.data = census.data)
    options(warn = oldw)
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
