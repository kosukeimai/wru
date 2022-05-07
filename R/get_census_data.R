#' Multilevel Census data download function.
#'
#' \code{get_census_data} returns county-, tract-, and block-level Census data 
#' for specified state(s). Using this function to download Census data in advance 
#' can save considerable time when running \code{predict_race} and \code{census_helper}.
#' 
#' @param key A required character object containing a valid Census API key, 
#' which can be requested \href{https://api.census.gov/data/key_signup.html}{here}.
#' @param states which states to extract Census data for, e.g., \code{c("NJ", "NY")}.
#' @param age A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  age or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Age | Race). 
#'  If \code{\var{sex}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param sex A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  sex or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race). 
#'  If \code{TRUE}, function will return Pr(Geolocation, Sex | Race). 
#'  If \code{\var{age}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param year A character object specifying the year of U.S. Census data to be downloaded.
#'  Use \code{"2010"}, or \code{"2020"}. Default is \code{"2010"}.
#'  Warning: 2020 U.S. Census data is downloaded only when \code{\var{age}} and 
#'  \code{\var{sex}} are both \code{FALSE}.
#' @param census.geo An optional character vector specifying what level of 
#' geography to use to merge in U.S. Census 2010 geographic data. Currently
#' \code{"county"}, \code{"tract"}, \code{"block"}, and \code{"place"} are supported.
#' @param retry The number of retries at the census website if network interruption occurs.
#' @param counties A character vector of counties present in your \var{voter.file} 
#' @return Output will be an object of class \code{list} indexed by state. 
#' Output will contain a subset of the following elements: 
#' \code{state}, \code{age}, \code{sex}, 
#' \code{county}, \code{tract}, \code{block}, and \code{place}.
#' 
#' @export
#'
#' @examples 
#' \dontrun{get_census_data(key = "...", states = c("NJ", "NY"), age = TRUE, sex = FALSE)}
#' \dontrun{get_census_data(key = "...", states = "MN", age = FALSE, sex = FALSE, year = "2020")}
get_census_data <- function(key = NULL, states, age = FALSE, sex = FALSE, year = "2010", census.geo = "block", retry = 3, counties = NULL) {
  
  if (is.null(key)) {
    # Matches tidycensus name for env var
    key <- Sys.getenv("CENSUS_API_KEY")
  }
  
  if (missing(key) | key == "") {
    stop('Must enter valid Census API key, which can be requested at https://api.census.gov/data/key_signup.html.')
  }
  
  states <- toupper(states)
  
  CensusObj <- NULL
  for (s in states) {
    CensusObj[[s]] <- list(state = s, age = age, sex = sex, year = year)
    if (census.geo == "place") {
      place <- census_geo_api(key, s, geo = "place", age, sex, year, retry)
      CensusObj[[s]]$place <- place
    }
    if (census.geo == "block") {
      block <- census_geo_api(key, s, geo = "block", age, sex, year, retry, counties = counties)
      CensusObj[[s]]$block <- block
    }
    if ((census.geo == "block") || (census.geo == "tract")) {
      tract <- census_geo_api(key, s, geo = "tract", age, sex, year, retry, counties = counties)
      CensusObj[[s]]$tract <- tract
    }
    if ((census.geo == "block") || (census.geo == "tract") || (census.geo == "county")) {
      county <- census_geo_api(key, s, geo = "county", age, sex, year, retry)
      CensusObj[[s]]$county <- county
    }
  }
  return(CensusObj)
}