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
#' @param census.geo An optional character vector specifying what level of 
#' geography to use to merge in U.S. Census 2010 geographic data. Currently
#' \code{"county"}, \code{"tract"}, \code{"block"}, and \code{"place"} are supported.
#' @param retry The number of retries at the census website if network interruption occurs.
#' @return Output will be an object of class \code{list} indexed by state. 
#' Output will contain a subset of the following elements: 
#' \code{state}, \code{age}, \code{sex}, 
#' \code{county}, \code{tract}, \code{block}, and \code{place}.
#' 
#' @export
#'
#' @examples \dontrun{get_census_data(key = "...", states = c("NJ", "NY"), age = TRUE, sex = FALSE)}
get_census_data <- function(key, states, age = FALSE, sex = FALSE, census.geo = "block", retry = 0) {
  
  if (missing(key)) {
    stop('Must enter valid Census API key, which can be requested at https://api.census.gov/data/key_signup.html.')
  }
  
  states <- toupper(states)
  
  CensusObj <- NULL
  for (s in states) {
    CensusObj[[s]] <- list(state = s, age = age, sex = sex)
    if (census.geo == "place") {
      place <- census_geo_api(key, s, geo = "place", age, sex, retry)
      CensusObj[[s]]$place <- place
    }
    if (census.geo == "block") {
      block <- census_geo_api(key, s, geo = "block", age, sex, retry)
      CensusObj[[s]]$block <- block
    }
    if ((census.geo == "block") || (census.geo == "tract")) {
      tract <- census_geo_api(key, s, geo = "tract", age, sex, retry)
      CensusObj[[s]]$tract <- tract
    }
    if ((census.geo == "block") || (census.geo == "tract") || (census.geo == "county")) {
      county <- census_geo_api(key, s, geo = "county", age, sex, retry)
      CensusObj[[s]]$county <- county
    }
  }
  return(CensusObj)
}