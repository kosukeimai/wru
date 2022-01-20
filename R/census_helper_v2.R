#' Census helper function.
#'
#' \code{census_helper_new} links user-input dataset with Census geographic data.
#'
#' This function allows users to link their geocoded dataset (e.g., voter file) 
#' with U.S. Census data (2010 or 2020). The function extracts Census Summary File data 
#' at the county, tract, block, or place level using the 'UScensus2010' package. Census data 
#' calculated are Pr(Geolocation | Race) where geolocation is county, tract, block, or place.
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
#'  Use \code{"county"}, \code{"tract"}, \code{"block"}, or \code{"place"}. 
#'  Default is \code{"tract"}. Warning: extracting block-level data takes very long.
#' @param year A character object specifying the year of U.S. Census data to be downloaded.
#'  Use \code{"2010"}, or \code{"2020"}. Default is \code{"2010"}.
#' @param census.data A optional census object of class \code{list} containing 
#' pre-saved Census geographic data. Can be created using \code{get_census_data} function.
#' If \code{\var{census.data}} is provided, the \code{\var{year}} element must 
#' have the same value as the \code{\var{year}} option specified in this function 
#' (i.e., \code{"2010"} in both or \code{"2020"} in both). 
#' If \code{\var{census.data}} is provided, the \code{\var{age}} and the \code{\var{sex}} 
#' elements must be \code{FALSE}. This corresponds to the defaults of \code{census_geo_api}.
#' If \code{\var{census.data}} is missing, Census geographic data will be obtained via Census API. 
#' @param retry The number of retries at the census website if network interruption occurs.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns of 
#'  Census data.
#'
#' @examples
#' \dontshow{data(voters)}
#' \dontrun{census_helper_new(key = "...", voter.file = voters, states = "nj", geo = "block")}
#' \dontrun{census_helper_new(key = "...", voter.file = voters, states = "all", geo = "tract")}
#' \dontrun{census_helper_new(key = "...", voter.file = voters, states = "all", geo = "place", year = "2020")}
#'
#' @export
census_helper_new <- function(key, voter.file, states = "all", geo = "tract", year = "2010", census.data = NA, retry = 0) {
  
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
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$year != year) || (census.data[[state]]$age != FALSE) || (census.data[[state]]$sex != FALSE)) {#} || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        census <- census_geo_api(key, state, geo = "place", age = FALSE, sex = FALSE, year, retry)
      } else {
        census <- census.data[[toupper(state)]]$place
      }
    }
    
    if (geo == "county") {
      geo.merge <- c("county")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$year != year) || (census.data[[state]]$age != FALSE) || (census.data[[state]]$sex != FALSE)) {#} || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        census <- census_geo_api(key, state, geo = "county", age = FALSE, sex = FALSE, year, retry)
      } else {
        census <- census.data[[toupper(state)]]$county
      }
    }
    
    if (geo == "tract") {
      geo.merge <- c("county", "tract")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$year != year) || (census.data[[state]]$age != FALSE) || (census.data[[state]]$sex != FALSE)) {#} || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        census <- census_geo_api(key, state, geo = "tract", age = FALSE, sex = FALSE, year, retry)
      } else {
        census <- census.data[[toupper(state)]]$tract
      }
    }
    
    if (geo == "block") {
      geo.merge <- c("county", "tract", "block")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$year != year) || (census.data[[state]]$age != FALSE) || (census.data[[state]]$sex != FALSE)) {#} || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        census <- census_geo_api(key, state, geo = "block", age = FALSE, sex = FALSE, year, retry)
      } else {
        census <- census.data[[toupper(state)]]$block
      }
    }
    
    census$state <- state
    
    # if (age == F & sex == F) {
    #   
    #   ## Calculate Pr(Geolocation | Race)
    #   geoPopulations <- rowSums(census[,grepl("P00", names(census))])
    #   census$r_whi <- (0.5 + census$P005003) / (geoPopulations + 2.5)#Pr(White | Geo)
    #   census$r_bla <- (0.5 + census$P005004) / (geoPopulations + 2.5)#Pr(Black | Geo)
    #   census$r_his <- (0.5 + census$P005010) / (geoPopulations + 2.5)#Pr(Latino | Geo)
    #   census$r_asi <- (0.5 + census$P005006 + census$P005007) / (geoPopulations + 2.5) #Pr(Asian or NH/PI | Geo)
    #   census$r_oth <- (0.5 + census$P005005 + census$P005008 + census$P005009) / (geoPopulations + 2.5) #Pr(AI/AN, Other, or Mixed | Geo)
    #   
    #   drop <- c(grep("state", names(census)), grep("P005", names(census)))
    #   voters.census <- merge(voter.file[toupper(voter.file$state) == toupper(states[s]), ], census[, -drop], by = geo.merge, all.x  = T)
    #   
    # }
    
    # if (age == F & sex == F) {
      
    ## Calculate Pr(Geolocation | Race)
    if (year == "2010") {
      geoPopulations <- rowSums(census[,grepl("P00", names(census))])
      vars <- c(
        pop_white = 'P005003', pop_black = 'P005004',
        pop_aian = 'P005005', pop_asian = 'P005006',
        pop_nhpi = 'P005007', pop_other = 'P005008', 
        pop_two = 'P005009', pop_hisp = 'P005010'
      )
      drop <- c(grep("state", names(census)), grep("P005", names(census)))
    }
    else if (year == "2020") {
      geoPopulations <- rowSums(census[,grepl("P2_", names(census))])
      vars <- c(
        pop_white = 'P2_005N', pop_black = 'P2_006N',
        pop_aian = 'P2_007N', pop_asian = 'P2_008N', 
        pop_nhpi = 'P2_009N', pop_other = 'P2_010N', 
        pop_two = 'P2_011N', pop_hisp = 'P2_002N'
      )
      drop <- c(grep("state", names(census)), grep("P2_", names(census)))
    }
    
    census$r_whi <- (0.5 + census[, vars["pop_white"]]) / (geoPopulations + 2.5) #Pr(White | Geo)
    census$r_bla <- (0.5 + census[, vars["pop_black"]]) / (geoPopulations + 2.5) #Pr(Black | Geo)
    census$r_his <- (0.5 + census[, vars["pop_hisp"]]) / (geoPopulations + 2.5) #Pr(Latino | Geo)
    census$r_asi <- (0.5 + census[, vars["pop_asian"]] + census[, vars["pop_nhpi"]]) / (geoPopulations + 2.5) #Pr(Asian or NH/PI | Geo)
    census$r_oth <- (0.5 + census[, vars["pop_aian"]] + census[, vars["pop_other"]] + census[, vars["pop_two"]]) / (geoPopulations + 2.5) #Pr(AI/AN, Other, or Mixed | Geo)
    
    voters.census <- merge(voter.file[toupper(voter.file$state) == toupper(states[s]), ], census[, -drop], by = geo.merge, all.x  = T)
      
    # }
    
    keep.vars <- c(names(voter.file)[names(voter.file) != "agecat"], 
                   paste("r", c("whi", "bla", "his", "asi", "oth"), sep = "_"))
    df.out <- as.data.frame(rbind(df.out, voters.census[keep.vars]))
    
  }
  
  return(df.out)
}