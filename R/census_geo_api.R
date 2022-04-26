#' Census Data download function.
#'
#' \code{census_geo_api} retrieves U.S. Census geographic data for a given state.
#'
#' This function allows users to download U.S. Census 2010 geographic data, 
#' at either the county, tract, block, or place level, for a particular state. 
#'
#' @param key A required character object. Must contain user's Census API
#'  key, which can be requested \href{https://api.census.gov/data/key_signup.html}{here}.
#' @param state A required character object specifying which state to extract Census data for, 
#' e.g., \code{"NJ"}.
#' @param geo A character object specifying what aggregation level to use. 
#'  Use \code{"county"}, \code{"tract"}, \code{"block"}, or \code{"place"}. 
#'  Default is \code{"tract"}. Warning: extracting block-level data takes very long.
#' @param age A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  age or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Age | Race). 
#'  If \code{\var{sex}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param sex A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  sex or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race). 
#'  If \code{TRUE}, function will return Pr(Geolocation, Sex | Race). 
#'  If \code{\var{age}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param retry The number of retries at the census website if network interruption occurs.
#' @param save_temp File indicating where to save the temporary outputs. 
#' Defaults to NULL. If specified, the function will look for an .RData file
#' with the same format as the expected output. 
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
#' @importFrom furrr future_map_dfr
#' @importfrom purrr map_dfr
#' @export
census_geo_api <- function(key, state, geo = "tract", age = FALSE, sex = FALSE, retry = 3, save_temp = NULL) {
  
  if (missing(key)) {
    stop('Must enter U.S. Census API key, which can be requested at https://api.census.gov/data/key_signup.html.')
  }
  
  state <- toupper(state)
  
  df.out <- NULL
  
  fips.codes <- get("State.FIPS")
  state.fips <- fips.codes[fips.codes$State == state, "FIPS"]
  state.fips <- ifelse(nchar(state.fips) == 1, paste0("0", state.fips), state.fips)
  
  if (age == F & sex == F) {
    num <- ifelse(3:10 != 10, paste("0", 3:10, sep = ""), "10")
    vars <- paste("P0050", num, sep = "")
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
    census <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", key = key, vars = vars, region = region, retry)
  }
  
  if (geo == "county") {
    geo.merge <- c("state", "county")
    region <- paste("for=county:*&in=state:", state.fips, sep = "")
    census <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", key = key, vars = vars, region = region, retry)
  }
  
  if (geo == "tract") {
    
    geo.merge <- c("state", "county", "tract")
    
    region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
    county_df <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", key = key, vars = vars, region = region_county, retry)
    county_list <- county_df$county
    census <- NULL
    temp <- check_temp_save(county_list, save_temp, census)
    county_list <- temp$county_list
    census <- temp$census
    
    message('Running tract by county...')
    
    census_tracts <- furrr::future_map_dfr(1:length(county_list), function(county) {
      message(paste("County ", county, " of ", length(county_list), ": ", county_list[county], sep = ""))
      region_county <- paste("for=tract:*&in=state:", state.fips, "+county:", county_list[county], sep = "")
      census.temp <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", key = key, vars = vars, region = region_county, retry)
      }
    )
    
    census <- rbind(census, census_tracts)
    rm(census_tracts)
  }
  
  if (geo == "block") {
    
    geo.merge <- c("state", "county", "tract", "block")
    
    region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
    county_df <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", key = key, vars = vars, region = region_county, retry)
    county_list <- county_df$county
    census <- NULL
    temp <- check_temp_save(county_list, save_temp, census)
    county_list <- temp$county_list
    census <- temp$census
    
    message('Running block by county...')
    
    census_blocks <- furrr::future_map_dfr(
      1:length(county_list), 
      function(county) {
        message(paste("County ", county, " of ", length(county_list), ": ", county_list[county], sep = ""))
        
        region_tract <- paste("for=tract:*&in=state:", state.fips, "+county:", county_list[county], sep = "")
        message(region_tract)
        tract_df <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", key = key, vars = vars, region = region_tract, retry)
        tract_list <- tract_df$tract
        
        purrr::map_dfr(1:length(tract_list), function(tract) {
          message(paste("Tract ", tract, " of ", length(tract_list), ": ", tract_list[tract], sep = ""))
          
          region_block <- paste("for=block:*&in=state:", state.fips, "+county:", county_list[county], "+tract:", tract_list[tract], sep = "")
          census.temp <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", key = key, vars = vars, region = region_block, retry)
          return(census.temp)
        })
      }
    )
    
    census <- rbind(census, census_blocks)
    rm(census_blocks)
  }
  
  census$state <- state
  
  if (age == F & sex == F) {
    
    ## Calculate Pr(Geolocation | Race)
    census$r_whi <- census$P005003 / sum(census$P005003) #Pr(Tract|White)
    census$r_bla <- census$P005004 / sum(census$P005004) #Pr(Tract|Black)
    census$r_his <- census$P005010 / sum(census$P005010) #Pr(Tract|Latino)
    census$r_asi <- (census$P005006 + census$P005007) / (sum(census$P005006) + sum(census$P005007)) #Pr(Tract | Asian or NH/PI)
    census$r_oth <- (census$P005005 + census$P005008 + census$P005009) / (sum(census$P005005) + sum(census$P005008) + sum(census$P005009)) #Pr(Tract | AI/AN, Other, or Mixed)
    
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

check_temp_save <- function(county_list, save_temp, census) {
  if (!is.null(save_temp)) {
    if (file.exists(save_temp)) {
      message("Temporary save file will be used as requested.")
      load(save_temp)
      ## Expecting a dataframe named census with the same format
      county_list <- setdiff(county_list, unique(census$county))
      message(paste0(
        length(unique(census$county)), " counties in the temporary file."
      ))
      message(paste0(length(county_list), " counties to be processed."))
    } else {
      message("Results will be saved in the specified temporary file.")
    }
  }
  return(list(county_list = county_list, census = census))
}
