#' Census helper function.
#'
#' \code{census_helper_v2} links user-input dataset with Census geographic data.
#'
#' This function allows users to link their geocoded dataset (e.g., voter file) 
#' with U.S. Census 2010 data. The function extracts Census Summary File data 
#' at the county, tract, or block level using the 'UScensus2010' package. Census data 
#' calculated are Pr(Geolocation | Race) where geolocation is county, tract, or block.
#'
#' @param key A required character object. Must contain user's Census API
#'  key, which can be requested \href{https://api.census.gov/data/key_signup.html}{here}.
#' @param voter.file An object of class \code{data.frame}. Must contain field(s) named 
#'  \code{\var{county}}, \code{\var{tract}}, \code{\var{block}}, and/or \code{\var{place}}
#'  specifying geolocation. These should be character variables that match up with 
#'  U.S. Census categories. County should be three characters (e.g., "031" not "31"), 
#'  tract should be six characters, and block should be four characters. 
#'  Place should be five characters if it is included.
#' @param states A character vector specifying which states to extract 
#'  Census data for, e.g. \code{c("NJ", "NY")}. Default is \code{"all"}, which extracts 
#'  Census data for all states contained in user-input data.
#' @param geo A character object specifying what aggregation level to use. 
#'  Use \code{"county"}, \code{"tract"}, or \code{"block"}. Default is \code{"tract"}. 
#'  Warning: extracting block-level data takes very long.
#' @param age A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  age or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Age | Race). 
#'  If \code{\var{sex}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param sex A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  sex or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race). 
#'  If \code{TRUE}, function will return Pr(Geolocation, Sex | Race). 
#'  If \code{\var{age}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param census.data A optional census object of class \code{list} containing 
#' pre-saved Census geographic data. Can be created using \code{get_census_data} function.
#' If \code{\var{census.data}} is provided, the \code{\var{age}} element must have the same value
#' as the \code{\var{age}} option specified in this function (i.e., \code{TRUE} in both or 
#' \code{FALSE} in both). Similarly, the \code{\var{sex}} element in the object provided in 
#' \code{\var{census.data}} must have the same value as the \code{\var{sex}} option here.
#' If \code{\var{census.data}} is missing, Census geographic data will be obtained via Census API. 
#' @param retry The number of retries at the census website if network interruption occurs.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns of 
#'  Census data.
#'
#' @examples
#' \dontshow{data(voters)}
#' \dontrun{census_helper(key = "...", voter.file = voters, states = "nj", geo = "block")}
#' \dontrun{census_helper(key = "...", voter.file = voters, states = "all", geo = "tract", 
#' age = TRUE, sex = TRUE)}
#'
#' @export
census_helper_new <- function(key, voter.file, states = "all", geo = "tract", age = FALSE, sex = FALSE, census.data = NA, retry = 0) {
  
  if (is.na(census.data) || (typeof(census.data) != "list")) {
    toDownload = TRUE
  } else {
    toDownload = FALSE
  }
  
  if (toDownload) {
    if (missing(key)) {
      stop('Must enter U.S. Census API key, which can be requested at https://api.census.gov/data/key_signup.html.')
    }
  } 
  
  states <- toupper(states)
  if (states == "ALL") {
    states <- toupper(as.character(unique(voter.file$state)))
  }
  
  df.out <- NULL
  
  for (s in 1:length(states)) {
    
    print(paste("State ", s, " of ", length(states), ": ", states[s], sep  = ""))
    state <- toupper(states[s])
    
    if (geo == "place") {
      geo.merge <- c("place")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        census <- census_geo_api(key, state, geo = "place", age, sex, retry)
      } else {
        census <- census.data[[toupper(state)]]$place
      }
    }
    
    if (geo == "county") {
      geo.merge <- c("county")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        census <- census_geo_api(key, state, geo = "county", age, sex, retry)
      } else {
        census <- census.data[[toupper(state)]]$county
      }
    }
    
    if (geo == "tract") {
      geo.merge <- c("county", "tract")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        census <- census_geo_api(key, state, geo = "tract", age, sex, retry)
      } else {
        census <- census.data[[toupper(state)]]$tract
      }
    }
    
    if (geo == "block") {
      geo.merge <- c("county", "tract", "block")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        census <- census_geo_api(key, state, geo = "block", age, sex, retry)
      } else {
        census <- census.data[[toupper(state)]]$block
      }
    }
    
    census$state <- state
    
    if (age == F & sex == F) {
      
      ## Calculate Pr(Geolocation | Race)
      geoPopulations <- rowSums(census[,grepl("P00", names(census))])
      census$r_whi <- census$P005003 / geoPopulations #Pr(White | Geo)
      census$r_bla <- census$P005004 / geoPopulations #Pr(Black | Geo)
      census$r_his <- census$P005010 / geoPopulations #Pr(Latino | Geo)
      census$r_asi <- (census$P005006 + census$P005007) / geoPopulations #Pr(Asian or NH/PI | Geo)
      census$r_oth <- (census$P005005 + census$P005008 + census$P005009) / geoPopulations #Pr(AI/AN, Other, or Mixed | Geo)
      
      drop <- c(grep("state", names(census)), grep("P005", names(census)))
      voters.census <- merge(voter.file[toupper(voter.file$state) == toupper(states[s]), ], census[, -drop], by = geo.merge, all.x  = T)
      
    }
    
    keep.vars <- c(names(voter.file)[names(voter.file) != "agecat"], 
                   paste("r", c("whi", "bla", "his", "asi", "oth"), sep = "_"))
    df.out <- as.data.frame(rbind(df.out, voters.census[keep.vars]))
    
  }
  
  return(df.out)
}
