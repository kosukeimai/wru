#' Census Data download function.
#'
#' \code{census_data} retrieves U.S. Census geographic data.
#'
#' This function allows users to download U.S. Census 2010 geographic data, 
#' at either county, tract, or block level. 
#'
#' @param key A required character object. Must contain user's Census API
#'  key, which can be requested \href{http://api.census.gov/data/key_signup.html}{here}.
#' @param state to extract Census data for, e.g. \code{"NJ"}.
#' @param geo A character object specifying what aggregation level to use. 
#'  Use \code{"county"}, \code{"tract"}, or \code{"block"}. Default is \code{"tract"}. 
#'  Warning: extracting block-level data takes very long.
#' @param demo A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  demographics (i.e., age and sex) or not. If \code{TRUE}, function will return 
#'  Pr(Geolocation, Age, Sex | Race). If \code{FALSE}, function wil return 
#'  Pr(Geolocation | Race). Default is \code{FALSE}.
#' @return Output will be an object of class \code{list}, indexed by state names. It will 
#'  consist of the original user-input data with additional columns of 
#'  Census data.
#'
#' @examples
#' \dontshow{data(voters)}
#' \dontrun{census_data(key = "...", states = c("NJ", "DE"), geo = "block")}
#' \dontrun{census_data(key = "...", states = "FL", geo = "tract", 
#' demo = TRUE)}
#'
#' @references
#' Relies on get_census_api, get_census_api_2, and vec_to_chunk functions authored by Nicholas Nagle, 
#' available \href{http://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html}{here}.
#' 
#' @export
census_data <- function(key, state, geo = "tract", demo = FALSE) {

  if (missing(key)) {
    stop('Must enter U.S. Census API key, which can be requested at http://api.census.gov/data/key_signup.html.')
  }
  
  state <- toupper(state)
  
  df.out <- NULL
  
  {
    print(paste("State", state))
    fips.codes <- get("State.FIPS")
    state.fips <- fips.codes[fips.codes$State == state, "FIPS"]

    if (demo == F) {
      num <- ifelse(3:10 != 10, paste("0", 3:10, sep = ""), "10")
      vars <- paste("P00500", num, sep = "")
    }
    
    if (demo == T) {
      eth.let <- c("I", "B", "H", "D", "E", "F", "C")
      num <- as.character(c(c("01", "07", "08", "09"), seq(10, 25), seq(31, 49)))
      vars <- NULL
      for (e in 1:length(eth.let)) {
        vars <- c(vars, paste("P012", eth.let[e], "0", num, sep = ""))
      }
    }
    
    if (geo == "county") {
      geo.merge <- c("state", "county")
      region <- paste("for=county:*&in=state:", state.fips, sep = "")
      census <- get_census_api("http://api.census.gov/data/2010/sf1?", 
                             key = key, vars = vars, region = region)
    }
    
    if (geo == "tract") {

      geo.merge <- c("state", "county", "tract")
      
      region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
      county_df <- get_census_api("http://api.census.gov/data/2010/sf1?", key = key, vars = vars, region = region_county)
      county_list <- county_df$county
      
      census <- NULL
      for (c in 1:length(county_list)) {
        print(paste("County ", c, " of ", length(county_list), ": ", county_list[c], sep = ""))
        region_county <- paste("for=tract:*&in=state:", state.fips, "+county:", county_list[c], sep = "")
        census.temp <- get_census_api("http://api.census.gov/data/2010/sf1?", 
                                    key = key, vars = vars, region = region_county)
        census <- rbind(census, census.temp)
      }
      rm(census.temp)
    }
    
    if (geo == "block") {
      
      geo.merge <- c("state", "county", "tract", "block")
      
      region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
      county_df <- get_census_api("http://api.census.gov/data/2010/sf1?", key = key, vars = vars, region = region_county)
      county_list <- county_df$county

      census <- NULL
      
      for (c in 1:length(county_list)) {
        print(paste("County ", c, " of ", length(county_list), ": ", county_list[c], sep = ""))
        
        region_tract <- paste("for=tract:*&in=state:", state.fips, "+county:", county_list[c], sep = "")
        print(region_tract)
        tract_df <- get_census_api("http://api.census.gov/data/2010/sf1?", 
                                 key = key, vars = vars, region = region_tract)
        tract_list <- tract_df$tract

        for (t in 1:length(tract_list)) {
          print(paste("Tract ", t, " of ", length(tract_list), ": ", tract_list[t], sep = ""))
          
          region_block <- paste("for=block:*&in=state:", state.fips, "+county:", county_list[c], "+tract:", tract_list[t], sep = "")
          census.temp <- get_census_api("http://api.census.gov/data/2010/sf1?", 
                                      key = key, vars = vars, region = region_block)
          census <- rbind(census, census.temp)
        }
      }
      rm(census.temp)
      
    }

    if (demo == F) {
      
      census$state <- state
      
      ## Calculate Pr(Geolocation | Race)
      census$r_whi <- census$P0050003 / sum(census$P0050003) #Pr(Tract|White)
      census$r_bla <- census$P0050004 / sum(census$P0050004) #Pr(Tract|Black)
      census$r_his <- census$P0050010 / sum(census$P0050010) #Pr(Tract|Latino)
      census$r_asi <- (census$P0050006 + census$P0050007) / (sum(census$P0050006) + sum(census$P0050007)) #Pr(Tract | Asian or NH/PI)
      census$r_oth <- (census$P0050005 + census$P0050008 + census$P0050009) / (sum(census$P0050005) + sum(census$P0050008) + sum(census$P0050009)) #Pr(Tract | AI/AN, Other, or Mixed)
      
    }
    
    if (demo == T) {
      
      census$state <- state
      
      ## Calculate Pr(Tract, Sex, Age Category | Race)
      eth.cen <- c("whi", "bla", "his", "asi", "oth")
      eth.let <- c("I", "B", "H", "D", "F")
      sex <- c("mal", "fem")
      age.cat <- c(seq(1, 23), seq(1, 23))
      age.cen <- as.character(c(c("03", "04", "05", "06", "07", "08", "09"), seq(10, 25), seq(27, 49)))
      
      for (i in 1:length(eth.cen)) {
        for (k in 1:length(sex)) {
          for (j in 1:23) {
            if (k == 2) {
              j <- j + 23
            }
            if (i != 4 & i != 5) {
              census[paste("r", sex[k], age.cat[j], eth.cen[i], sep = "_")] <- census[paste("P012", eth.let[i], "0", age.cen[j], sep = "")] / sum(census[paste("P012", eth.let[i], "001", sep = "")])
            }
            if (i == 4) {
              ## Combine Asian and Native Hawaiian/Pacific Islander
              census[paste("r", sex[k], age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012D0", age.cen[j], sep = "")] + census[paste("P012E0", age.cen[j], sep = "")]) / sum(census$P012D001 + census$P012E001)
            }
            if (i == 5) {
              ## Combine American India/Alaska Native and Other
              census[paste("r", sex[k], age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012C0", age.cen[j], sep = "")] + census[paste("P012F0", age.cen[j], sep = "")]) / sum(census$P012C001 + census$P012F001)
            }
          }
        }
      }
      
    }
  
  }
  
  return(census)
}

#' Title return
#' Multilevel Census Data download function.
#'
#' \code{get_census_data} returns a Census data object for a state.
#' 
#' @param key A required character object. Must contain user's Census API
#'  key, which can be requested \href{http://api.census.gov/data/key_signup.html}{here}.
#' @param states which states to extract 
#'  Census data for, e.g. \code{c("NJ", "NY")}.
#' @param demo A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  demographics (i.e., age and sex) or not. If \code{TRUE}, function will return 
#'  Pr(Geolocation, Age, Sex | Race). If \code{FALSE}, function wil return 
#'  Pr(Geolocation | Race). Default is \code{FALSE}.
#'
#' @return Output will be an census object of class which is a list consist of \code{state}, 
#' \code{demo}, \code{county level census}, \code{tract level census} and \code{block level census}. 
#' Having the census data available could make \code{census.helper.api} runs more efficient.
#' 
#' @export
#'
#' @examples \dontrun{get_census_data(key = "...", states = c("NJ", "DE"), demo = TRUE)}
get_census_data <- function(key, states, demo = FALSE) {
  
  if (missing(key)) {
    stop('Must enter U.S. Census API key, which can be requested at http://api.census.gov/data/key_signup.html.')
  }
  
  states <- toupper(states)
  
  CensusObj <- NULL
  for (s in states) {
      county = census_data(key, s, geo = "county", demo)
      tract = census_data(key, s, geo = "tract", demo)
      block = census_data(key, s, geo = "block", demo)
      CensusObj[[s]] <- list(state = s, demo = demo, county = county, tract = tract, block = block)
  }
  return(CensusObj)
}
