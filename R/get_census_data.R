#' Multilevel Census data download function.
#'
#' \code{get_census_data} returns county-, tract-, and block-level Census data 
#' for specified state(s). Using this function to download Census data in advance 
#' can save considerable time when running \code{predict_race} and \code{census_helper}.
#' 
#' @param key A required character object containing a valid Census API key, 
#' which can be requested \href{http://api.census.gov/data/key_signup.html}{here}.
#' @param states which states to extract Census data for, e.g., \code{c("NJ", "NY")}.
#' @param age A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  age or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Age | Race). 
#'  If \code{\var{sex}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param sex A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  sex or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race). 
#'  If \code{TRUE}, function will return Pr(Geolocation, Sex | Race). 
#'  If \code{\var{age}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @return Output will be an object of class \code{list} indexed by state. 
#' Output will contain the following elements: \code{state}, \code{age}, \code{sex}, 
#' \code{county}, \code{tract} and \code{block}. 
#' 
#' @export
#'
#' @examples \dontrun{get_census_data(key = "...", states = c("NJ", "NY"), age = TRUE, sex = FALSE)}
get_census_data <- function(key, states, age = FALSE, sex = FALSE) {
  
  if (missing(key)) {
    stop('Must enter valid Census API key, which can be requested at http://api.census.gov/data/key_signup.html.')
  }
  
  states <- toupper(states)
  
  CensusObj <- NULL
  for (s in states) {
      county = census_geo_api(key, s, geo = "county", age, sex)
      tract = census_geo_api(key, s, geo = "tract", age, sex)
      block = census_geo_api(key, s, geo = "block", age, sex)
      CensusObj[[s]] <- list(state = s, age = age, sex = sex, county = county, tract = tract, block = block)
  }
  return(CensusObj)
}
