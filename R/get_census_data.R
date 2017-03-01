#' Multilevel Census Data download function.
#'
#' \code{get_census_data} returns a Census data object for specified state(s).
#' Using this function to download Census data in advance can save considerable 
#' time when running \code{predict_race} and \code{census_helper} functions.
#' 
#' @param key A required character object containing a valid Census API key, 
#' which can be requested \href{http://api.census.gov/data/key_signup.html}{here}.
#' @param states which states to extract Census data for, e.g., \code{c("NJ", "NY")}.
#' @param demo A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  demographics (i.e., age and sex) or not. If \code{TRUE}, function will return 
#'  Pr(Geolocation, Age, Sex | Race). If \code{FALSE}, function will return 
#'  Pr(Geolocation | Race). Default is \code{FALSE}.
#'
#' @return Output will be an object of class \code{list} indexed by state. 
#' Output will contain the following elements: \code{state}, \code{demo}, 
#' \code{county}, \code{tract} and \code{block}. 
#' 
#' @export
#'
#' @examples \dontrun{get_census_data(key = "...", states = c("NJ", "NY"), demo = TRUE)}
get_census_data <- function(key, states, demo = FALSE) {
  
  if (missing(key)) {
    stop('Must enter valid Census API key, which can be requested at http://api.census.gov/data/key_signup.html.')
  }
  
  states <- toupper(states)
  
  CensusObj <- NULL
  for (s in states) {
      county = census_geo_api(key, s, geo = "county", demo)
      tract = census_geo_api(key, s, geo = "tract", demo)
      block = census_geo_api(key, s, geo = "block", demo)
      CensusObj[[s]] <- list(state = s, demo = demo, county = county, tract = tract, block = block)
  }
  return(CensusObj)
}
