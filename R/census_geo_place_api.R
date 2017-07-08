#' Census Data download function.
#'
#' \code{census_geo_place_api} retrieves U.S. Census geographic data for a given state.
#'
#' This function allows users to download U.S. Census 2010 geographic data, 
#' at either the county, tract, or block level, for a particular state. 
#'
#' @param key A required character object. Must contain user's Census API
#'  key, which can be requested \href{https://api.census.gov/data/key_signup.html}{here}.
#' @param state A required character object specifying which state to extract Census data for, 
#' e.g., \code{"NJ"}.
#' @param geo A character object specifying what aggregation level to use. 
#'  Use \code{"place"}. Default is \code{"place"}. 
#'  Warning: extracting block-level data takes very long.
#' @param age A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  age or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Age | Race). 
#'  If \code{\var{sex}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param sex A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  sex or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race). 
#'  If \code{TRUE}, function will return Pr(Geolocation, Sex | Race). 
#'  If \code{\var{age}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param retry The number of retries at the census website if network interruption occurs.
#' @return Output will be an object of class \code{list}, indexed by state names. It will 
#'  consist of the original user-input data with additional columns of Census geographic data.
#'
#' @examples
#' \dontshow{data(voters)}
#' \dontrun{census_geo_api(key = "...", states = c("NJ", "DE"), geo = "block")}
#' \dontrun{census_geo_api(key = "...", states = "FL", geo = "tract", age = TRUE, sex = TRUE)}
#'
#' @references
#' Relies on get_census_api, get_census_api_2, and vec_to_chunk functions authored by Nicholas Nagle, 
#' available \href{https://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html}{here}.
#' 
#' @export
census_geo_place_api <- function(key, state, geo = "place", age = FALSE, sex = FALSE, retry = 0) {
  
  if (missing(key)) {
    stop('Must enter U.S. Census API key, which can be requested at https://api.census.gov/data/key_signup.html.')
  }
  
  state <- toupper(state)
  
  df.out <- NULL
  
  fips.codes <- get("State.FIPS")
  state.fips <- fips.codes[fips.codes$State == state, "FIPS"]
  
  if (age == F & sex == F) {
    num <- ifelse(3:10 != 10, paste("0", 3:10, sep = ""), "10")
    vars <- paste("P00500", num, sep = "")
  }
  
  if (age == F & sex == T) {
    eth.let <- c("I", "B", "H", "D", "E", "F", "C")
    num <- as.character(c("01", "02", "26"))
    vars <- NULL
    for (e in 1:length(eth.let)) {
      vars <- c(vars, paste("P012", eth.let[e], "0", num, sep = ""))
    }
  }
  
  if (age == T & sex == F) {
    eth.let <- c("I", "B", "H", "D", "E", "F", "C")
    num <- as.character(c(c("01", "03", "04", "05", "06", "07", "08", "09"), seq(10, 25), seq(27, 49)))
    vars <- NULL
    for (e in 1:length(eth.let)) {
      vars <- c(vars, paste("P012", eth.let[e], "0", num, sep = ""))
    }
  }
  
  if (age == T & sex == T) {
    eth.let <- c("I", "B", "H", "D", "E", "F", "C")
    num <- as.character(c(c("01", "03", "04", "05", "06", "07", "08", "09"), seq(10, 25), seq(27, 49)))
    vars <- NULL
    for (e in 1:length(eth.let)) {
      vars <- c(vars, paste("P012", eth.let[e], "0", num, sep = ""))
    }
  }
  
  if (geo == "place") {
    geo.merge <- c("state", "place")
    region <- paste("for=place:*&in=state:", state.fips, sep = "")
    census <- get_census_api("https://api.census.gov/data/2010/sf1?", key = key, vars = vars, region = region, retry)
  }
  

  

    

    
  }
  
  census$state <- state
  
  if (age == F & sex == F) {
    
    ## Calculate Pr(Geolocation | Race)
    census$r_whi <- census$P0050003 / sum(census$P0050003) #Pr(Tract|White)
    census$r_bla <- census$P0050004 / sum(census$P0050004) #Pr(Tract|Black)
    census$r_his <- census$P0050010 / sum(census$P0050010) #Pr(Tract|Latino)
    census$r_asi <- (census$P0050006 + census$P0050007) / (sum(census$P0050006) + sum(census$P0050007)) #Pr(Tract | Asian or NH/PI)
    census$r_oth <- (census$P0050005 + census$P0050008 + census$P0050009) / (sum(census$P0050005) + sum(census$P0050008) + sum(census$P0050009)) #Pr(Tract | AI/AN, Other, or Mixed)
    
  }
  
  if (age == F & sex == T) {
    
    ## Calculate Pr(Geolocation, Sex | Race)
    eth.cen <- c("whi", "bla", "his", "asi", "oth")
    eth.let <- c("I", "B", "H", "D", "F")
    
    for (i in 1:length(eth.cen)) {
      if (i != 4 & i != 5) {
        census[paste("r_mal", eth.cen[i], sep = "_")] <- census[paste("P012", eth.let[i], "002", sep = "")] / sum(census[paste("P012", eth.let[i], "001", sep = "")])
        census[paste("r_fem", eth.cen[i], sep = "_")] <- census[paste("P012", eth.let[i], "026", sep = "")] / sum(census[paste("P012", eth.let[i], "001", sep = "")])
      }
      if (i == 4) {
        ## Combine Asian and Native Hawaiian/Pacific Islander
        census[paste("r_mal", eth.cen[i], sep = "_")] <- (census$P012D002 + census$P012E002) / sum(census$P012D001 + census$P012E001)
        census[paste("r_fem", eth.cen[i], sep = "_")] <- (census$P012D026 + census$P012E026) / sum(census$P012D001 + census$P012E001)
      }
      if (i == 5) {
        ## Combine American India/Alaska Native and Other
        census[paste("r_mal", eth.cen[i], sep = "_")] <- (census$P012C002 + census$P012F002) / sum(census$P012C001 + census$P012F001)
        census[paste("r_fem", eth.cen[i], sep = "_")] <- (census$P012C026 + census$P012F026) / sum(census$P012C001 + census$P012F001)
      }
    }
  }
  
  if (age == T & sex == F) {
    
    ## Calculate Pr(Geolocation, Age Category | Race)
    eth.cen <- c("whi", "bla", "his", "asi", "oth")
    eth.let <- c("I", "B", "H", "D", "F")
    age.cat <- c(seq(1, 23), seq(1, 23))
    age.cen <- as.character(c(c("03", "04", "05", "06", "07", "08", "09"), seq(10, 25), seq(27, 49)))
    
    for (i in 1:length(eth.cen)) {
      for (j in 1:23) {
        if (i != 4 & i != 5) {
          census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012", eth.let[i], "0", age.cen[j], sep = "")] + census[paste("P012", eth.let[i], "0", age.cen[j + 23], sep = "")]) / sum(census[paste("P012", eth.let[i], "001", sep = "")])
        }
        if (i == 4) {
          ## Combine Asian and Native Hawaiian/Pacific Islander
          census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012D0", age.cen[j], sep = "")] + census[paste("P012D0", age.cen[j + 23], sep = "")] + census[paste("P012E0", age.cen[j], sep = "")] + census[paste("P012E0", age.cen[j + 23], sep = "")]) / sum(census$P012D001 + census$P012E001)
        }
        if (i == 5) {
          ## Combine American India/Alaska Native and Other
          census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012C0", age.cen[j], sep = "")] + census[paste("P012C0", age.cen[j + 23], sep = "")] + census[paste("P012F0", age.cen[j], sep = "")] + census[paste("P012F0", age.cen[j + 23], sep = "")]) / sum(census$P012C001 + census$P012F001)
        }
      }
    }
  }
  
  if (age == T & sex == T) {
    
    ## Calculate Pr(Geolocation, Sex, Age Category | Race)
    eth.cen <- c("whi", "bla", "his", "asi", "oth")
    eth.let <- c("I", "B", "H", "D", "F")
    sex.let <- c("mal", "fem")
    age.cat <- c(seq(1, 23), seq(1, 23))
    age.cen <- as.character(c(c("03", "04", "05", "06", "07", "08", "09"), seq(10, 25), seq(27, 49)))
    
    for (i in 1:length(eth.cen)) {
      for (k in 1:length(sex.let)) {
        for (j in 1:23) {
          if (k == 2) {
            j <- j + 23
          }
          if (i != 4 & i != 5) {
            census[paste("r", sex.let[k], age.cat[j], eth.cen[i], sep = "_")] <- census[paste("P012", eth.let[i], "0", age.cen[j], sep = "")] / sum(census[paste("P012", eth.let[i], "001", sep = "")])
          }
          if (i == 4) {
            ## Combine Asian and Native Hawaiian/Pacific Islander
            census[paste("r", sex.let[k], age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012D0", age.cen[j], sep = "")] + census[paste("P012E0", age.cen[j], sep = "")]) / sum(census$P012D001 + census$P012E001)
          }
          if (i == 5) {
            ## Combine American India/Alaska Native and Other
            census[paste("r", sex.let[k], age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012C0", age.cen[j], sep = "")] + census[paste("P012F0", age.cen[j], sep = "")]) / sum(census$P012C001 + census$P012F001)
          }
        }
      }
    }
  }
  
  return(census)
}
