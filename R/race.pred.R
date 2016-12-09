#' Race prediction function.
#'
#' \code{race.pred} makes probabilistic estimates of individual-level race/ethnicity.
#'
#' This function implements the Bayesian race prediction methods outlined in 
#' Imai and Khanna (2015). The function produces probabilistic estimates of 
#' individual-level race/ethnicity, based on surname, geolocation, and party.
#' @param voters An object of class \code{data.frame}. Must contain a row for each individual being 
#' predicted, as well as a field named \code{\var{surname}} containing each individual's surname.
#' If using geolocation in predictions, \code{\var{voters}} must contain a field named \code{\var{state}}, 
#' which contains the two-character abbreviation for each individual's state of residence (e.g., "nj" for New Jersey). 
#' If using geolocation, \code{\var{voters}} must also contain at least one of the following fields: 
#' \code{\var{county}}, \code{\var{tract}}, and/or \code{\var{block}}. 
#' These fields should contain character strings matching U.S. Census categories. 
#' County is three characters (e.g., "031" not "31"), tract is six characters, and block is four characters. 
#' See below for other optional fields.
#' @param races A character vector specifying which racial groups to generate 
#'  predicted probabilities for. Can include any subset of the default vector, 
#'  which is \code{c("white", "black", "latino", "asian", "other")}.
#' @param name.clean A \code{TRUE}/\code{FALSE} object. If \code{TRUE}, function will call
#'  \code{name.clean} to merge in data from U.S. Census 2000 Surname List 
#'  and Spanish Surname List. If \code{FALSE}, voters object must contain additional fields 
#'  specifying Pr(Race | Surname), named as follows: 
#'  \code{\var{p_whi}} for Whites, \code{\var{p_bla}} for Blacks, 
#'  \code{\var{p_his}} for Hispanics/Latinos, \code{\var{p_asi}} for Asians, 
#'  and/or \code{\var{p_oth}} for Other. Default is \code{TRUE}.
#' @param census An optional character vector specifying what level of 
#' geography to use to merge in U.S. Census 2010 data. Can be one of 
#' \code{"county"}, \code{"tract"}, or \code{"block"}. 
#' Function calls \code{census.helper.api} to merge in Census data at specified level. 
#' If left unspecified, \code{voters} must contain additional fields 
#' specifying Pr(Geolocation | Race), including any of the following: 
#' \code{\var{r_whi}}, \code{\var{r_bla}}, \code{\var{r_his}}, 
#' \code{\var{r_asi}}, and/or \code{\var{r_oth}}. 
#' @param census.key A character object specifying user's Census API 
#'  key. Required if \code{census} is specified, because 
#'  \code{census.helper} function requires a Census API key to operate.
#' @param demo An optional \code{TRUE}/\code{FALSE} object specifying whether to 
#' condition race predictions on individual age and sex (in addition to geolocation). 
#' Default is \code{FALSE}. 
#' May only be set to \code{TRUE} if \code{census} option is specified. 
#' If \code{TRUE}, \code{voters} should include numerical variables 
#' \code{\var{age}} and \code{\var{sex}}, where \code{\var{sex}} coded as 0 for 
#' males and 1 for females.
#' @param party An optional character object specifying party registration field in \code{\var{voters}}, 
#' e.g., \code{\var{party} = "PartyReg"}. If specified, race/ethnicity predictions will be conditioned 
#' on individual's party registration (in addition to geolocation). 
#' Whatever the name of the party registration field in \code{\var{voters}}, 
#' it should be coded as 1 for Democrat, 2 for Republican, and 0 for Other.
#' @param census.data A census data object, a list indexed by state names, 
#' which contains census data on demo, county, tract and block.
#' @param surname.only A \code{TRUE}/\code{FALSE} object. If \code{TRUE}, race predictions will 
#'  only use surname data and calculate Pr(Race | Surnname). Default is \code{FALSE}.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns that 
#'  contain predicted probabilities for each race in \code{races}.
#'
#' @examples
#' data(voters)
#' race.pred(voters = voters, races = c("asian"), surname.only = TRUE)
#' \dontrun{race.pred(voters = voters, races = c("white", "black", "latino"), 
#' census = "tract", census.key = "...", demo = TRUE)}
#' \dontrun{race.pred(voters = voters, races = c("white", "black", "latino", "asian", "other"), 
#' census = "tract", census.key = "...", party = "PID")}
#' \dontrun{race.pred(voters = voters, races = c("white", "black", "latino", "asian", "other"), 
#' census = "tract", census.data = censusObjs, party = "PID")}
#' @export

## Race Prediction Function
race.pred <- function(voters, races = c("white", "black", "latino", "asian", "other"), 
                      name.clean = TRUE, surname.only = FALSE, 
                      census = "", census.key = "", demo = FALSE, party, census.data = NA) {
  
  vars.orig <- names(voters)
  
  ## Subset user-specified races (maximum of five)
  eth <- c("whi", "bla", "his", "asi", "oth")[c("white", "black", "latino", "asian", "other") %in% races]
  
  if (census == "" & demo == TRUE) {
    stop('Cannot set demo to TRUE without specifying census option.')
  }

  ## Merge in Pr(Race | Surname) if necessary
  if (name.clean == TRUE) {
    voters <- name.clean(voters)
  }
  
  ## Surname-Only Predictions
  if (surname.only == TRUE) {
    for (k in 1:length(eth)) {
      voters[paste("pred", eth[k], sep = ".")] <- voters[paste("p", eth[k], sep = "_")]
    }
    pred <- paste("pred", eth, sep = ".")
    return(voters[c(vars.orig, pred)])
  }

  ## Merge in Pr(Party | Race) if necessary
  if (missing(party) == F) {
    voters$PID <- voters[, party]
    voters <- merge(voters, get("pid")[names(get("pid")) %in% "party" == F], by = "PID", all.x = T)  
  }
  
  if (census == "block") {
    oldw <- getOption("warn")
    options(warn = -1)
    warning("Extracting U.S. Census 2010 block-level data -- may take a long time!")

    #    
    #    if (is.na(census.data)) {
    #      voters <- census.helper.api.online(key = census.key, 
    #                                  voters = voters, 
    #                                  states = "all", 
    #                                  geo = "block", 
    #                                  demo = demo)
    #    } else {
    #      voters <- census.helper.api.local(voters = voters, 
    #                                        state = toupper(unique(voters$state)[1]), 
    #                                        geo = "block", 
    #                                        demo = demo, 
    #                                        census.data = census.data)
    #    }
    #
    
    voters <- census.helper.api(key = census.key, 
                                voters = voters, 
                                states = "all", 
                                geo = "block", 
                                demo = demo,
                                census.data = census.data)
    options(warn = oldw)
  }

  if (census == "precinct") {
    geo <- "precinct"
    stop('Error: census.helper function does not currently support merging precinct-level data.')
  }

  if (census == "tract") {
    oldw <- getOption("warn")
    options(warn = -1)
    warning("Extracting U.S. Census 2010 tract-level data -- may take a long time!")
    
    #   
    #    if (is.na(census.data)) {
    #      voters <- census.helper.api.online(key = census.key, 
    #                                  voters = voters, 
    #                                  states = "all", 
    #                                  geo = "tract", 
    #                                  demo = demo)
    #    } else {
    #      voters <- census.helper.api.local(voters = voters, 
    #                                        state = toupper(unique(voters$state)[1]), 
    #                                        geo = "tract", 
    #                                        demo = demo, 
    #                                        census.data = census.data)
    #    }
    #
    
    voters <- census.helper.api(key = census.key, 
                                     voters = voters, 
                                     states = "all", 
                                     geo = "tract", 
                                     demo = demo, 
                                     census.data = census.data)
    options(warn = oldw)
  }
  
  if (census == "county") {
    oldw <- getOption("warn")
    options(warn = -1)
    warning("Extracting U.S. Census 2010 county-level data -- may take a long time!")
    
    #   
    #    if (is.na(census.data)) {
    #      voters <- census.helper.api.online(key = census.key, 
    #                                  voters = voters, 
    #                                  states = "all", 
    #                                  geo = "county", 
    #                                  demo = demo)
    #    } else {
    #      voters <- census.helper.api.local(voters = voters, 
    #                                        state = toupper(unique(voters$state)[1]), 
    #                                        geo = "county", 
    #                                        demo = demo, 
    #                                        census.data = census.data)
    #    }
    #
    
    voters <- census.helper.api(key = census.key, 
                                     voters = voters, 
                                     states = "all", 
                                     geo = "county", 
                                     demo = demo, 
                                     census.data = census.data)
    options(warn = oldw)
  }
  
  ## Pr(Race | Surname, Geolocation)
  if (missing(party)) {
    for (k in 1:length(eth)) {
      voters[paste("u", eth[k], sep = "_")] <- voters[paste("p", eth[k], sep = "_")] * voters[paste("r", eth[k], sep = "_")]
    }
    voters$u_tot <- apply(voters[paste("u", eth, sep = "_")], 1, sum, na.rm = T)
    for (k in 1:length(eth)) {
      voters[paste("q", eth[k], sep = "_")] <- voters[paste("u", eth[k], sep = "_")] / voters$u_tot
    }
  }
  
  ## Pr(Race | Surname, Geolocation, Party)
  if (missing(party) == F) {
    for (k in 1:length(eth)) {
      voters[paste("u", eth[k], sep = "_")] <- voters[paste("p", eth[k], sep = "_")] * voters[paste("r", eth[k], sep = "_")] * voters[paste("r_pid", eth[k], sep = "_")]
    }
    voters$u_tot <- apply(voters[paste("u", eth, sep = "_")], 1, sum, na.rm = T)
    for (k in 1:length(eth)) {
      voters[paste("q", eth[k], sep = "_")] <- voters[paste("u", eth[k], sep = "_")] / voters$u_tot
    }
  }

  for (k in 1:length(eth)) {
    voters[paste("pred", eth[k], sep = ".")] <- voters[paste("q", eth[k], sep = "_")]
  }
  pred <- paste("pred", eth, sep = ".")
  
  return(voters[c(vars.orig, pred)])
}
