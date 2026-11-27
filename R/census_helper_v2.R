
#' Census helper function.
#'
#' \code{census_helper_new} links user-input dataset with Census geographic data.
#'
#' This function allows users to link their geocoded dataset (e.g., voter file) 
#' with U.S. Census data (2010 or 2020). The function extracts Census Summary File data 
#' at the county, tract, block, or place level. Census data calculated are 
#' Pr(Geolocation | Race) where geolocation is county, tract, block, or place.
#'
#' @inheritParams get_census_data
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
#' @param age A \code{TRUE}/\code{FALSE} object indicating whether to condition on
#'  age or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Age | Race).
#'  If \code{\var{sex}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param sex A \code{TRUE}/\code{FALSE} object indicating whether to condition on
#'  sex or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Sex | Race).
#'  If \code{\var{age}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param year A character object specifying the year of U.S. Census data to be downloaded.
#'  Use \code{"2010"}, or \code{"2020"}. Default is \code{"2020"}.
#' @param census.data A optional census object of class \code{list} containing 
#' pre-saved Census geographic data. Can be created using \code{get_census_data} function.
#' If \code{\var{census.data}} is provided, the \code{\var{year}} element must 
#' have the same value as the \code{\var{year}} option specified in this function 
#' (i.e., \code{"2010"} in both or \code{"2020"} in both). 
#' If \code{\var{census.data}} is provided, the \code{\var{age}} and the \code{\var{sex}} 
#' elements must be \code{FALSE}. This corresponds to the defaults of \code{census_geo_api}.
#' If \code{\var{census.data}} is missing, Census geographic data will be obtained via Census API. 
#' @param retry The number of retries at the census website if network interruption occurs.
#' @param use.counties A logical, defaulting to FALSE. Should census data be filtered by counties 
#' available in \var{census.data}?
#' @param skip_bad_geos Logical. Option to have the function skip any geolocations that are not present 
#' in the census data, returning a partial data set. Default is set to \code{FALSE}, which case it will 
#' break and provide error message with a list of offending geolocations.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns of 
#'  Census data.
#'
#' @examples
#' \dontshow{data(voters)}
#' \dontrun{census_helper_new(voter.file = voters, states = "nj", geo = "block")}
#' \dontrun{census_helper_new(voter.file = voters, states = "all", geo = "tract")}
#' \dontrun{census_helper_new(voter.file = voters, states = "all", geo = "place",
#'  year = "2020")}
#'
#' @keywords internal

census_helper_new <- function(
    key = Sys.getenv("CENSUS_API_KEY"),
    voter.file,
    states = "all",
    geo = c("tract", "block", "block_group", "county", "place", "zcta"),
    age = FALSE,
    sex = FALSE,
    year = "2020",
    census.data = NULL,
    retry = 3,
    use.counties = FALSE,
    skip_bad_geos = FALSE
) {
  
  if ("precinct" %in% geo) {
    stop("Error: census_helper_new function does not currently support precinct-level data.")
  }
  
  geo <- tolower(geo)
  geo <- rlang::arg_match(geo)
  
  if(!(year %in% c("2000","2010","2020"))){
    stop("Interface only implemented for census years '2000', '2010', or '2020'.")
  }
  
  if (is.null(census.data) || (typeof(census.data) != "list")) {
    toDownload = TRUE
  } else {
    toDownload = FALSE
  }
  
  if (toDownload) {
    key <- validate_key(key)
  } 
  
  if (identical(toupper(states), "ALL")) {
    states <- toupper(as.character(unique(voter.file$state)))
  }
  states <- as_state_abbreviation(states)
  
  df.out <- NULL
  
  for (s in seq_along(states)) {
    
    message(paste("State ", s, " of ", length(states), ": ", states[s], sep  = ""))
    state <- toupper(states[s])
    
    if (geo == "tract") {
      geo.merge <- c("county", "tract")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$year != year) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        if(use.counties) {
          census <- census_geo_api(key, state, geo = "tract", age, sex, year, retry,
                                   # Only those counties within the target state
                                   counties = unique(voter.file$county[toupper(voter.file$state) == state]))
        } else {
          census <- census_geo_api(key, state, geo = "tract", age, sex, year, retry)
        }
      } else {
        census <- census.data[[toupper(state)]]$tract
      }
    } else if (geo == "block_group") {
      geo.merge <- c("county", "tract", "block_group")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$year != year) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        if(use.counties) {
          census <- census_geo_api(key, state, geo = "block_group", age, sex, year, retry,
                                   # Only those counties within the target state
                                   counties = unique(voter.file$county[toupper(voter.file$state) == state]))
        } else {
          census <- census_geo_api(key, state, geo = "block_group", age, sex, year, retry)
        }
        
      } else {
        census <- census.data[[toupper(state)]]$block_group
      }
    } else if (geo == "block") {
      if(any(names(census.data) == "block_group")) {
        geo.merge <- c("county", "tract", "block_group", "block")
      } else {
        geo.merge <- c("county", "tract", "block")
      }

      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$year != year) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex)) {
        if(use.counties) {
          census <- census_geo_api(key, state, geo = "block", age, sex, year, retry,
                                   # Only those counties within the target state
                                   counties = unique(voter.file$county[toupper(voter.file$state) == state]))
        } else {
          census <- census_geo_api(key, state, geo = "block", age, sex, year, retry)
        }
        
      } else {
        census <- census.data[[toupper(state)]]$block
      }
    } else {
      geo.merge <- geo
      
      state_must_be_downloaded <- toDownload ||
        is.null(census.data[[state]]) ||
        census.data[[state]]$year != year ||
        census.data[[state]]$age != age ||
        census.data[[state]]$sex != sex
      
      if (state_must_be_downloaded) {
        census <- census_geo_api(key, state, geo = geo, age, sex, year, retry)
      } else {
        census <- census.data[[state]][[geo]]
      }
    }
    
    census$state <- state
    
    ## Calculate Pr(Geolocation | Race)
    if (any(c("P2_005N", "P005003") %in% names(census))) {
      message(sprintf("NOTE: Legacy column names detected, loading Race values from Census Redistricting table for %s. Age, Sex, and ZCTA predictions will be unavailable.", year))
      # TODO: Add test that we get the same ratios with legacy and new tables for 2020
      # Old table: Redistricting (Pl-some numbers) (does not have age, sex, or ZCTAs)
      # New table: DHC (does have age, sex, and ZCTA)
      vars_ <- census_geo_api_names_legacy(year = year, nms = names(census))
    } else {
      vars_ <- census_geo_api_names(year)
    }
    drop <- match(c("state", unlist(vars_)), names(census))
    
    geoPopulations <- rowSums(census[,names(census) %in% unlist(vars_)])
    
    census$r_whi <- rowSums(census[, vars_[["r_whi"]], drop = FALSE]) / (geoPopulations) #Pr(White | Geo)
    census$r_bla <- rowSums(census[, vars_[["r_bla"]], drop = FALSE]) / (geoPopulations) #Pr(Black | Geo)
    census$r_his <- rowSums(census[, vars_[["r_his"]], drop = FALSE]) / (geoPopulations) #Pr(Latino | Geo)
    census$r_asi <- rowSums(census[, vars_[["r_asi"]], drop = FALSE]) / (geoPopulations) #Pr(Asian or NH/PI | Geo)
    census$r_oth <- rowSums(census[, vars_[["r_oth"]], drop = FALSE]) / (geoPopulations) #Pr(AI/AN, Other, or Mixed | Geo)
    
    # check locations with zero people
    # get average without places with zero people, and assign that to zero locs.
    zero_ind <- which((geoPopulations - 0.0) < .Machine$double.eps)
    if (length(zero_ind)) {
      for (rcat in c("r_whi","r_bla","r_his","r_asi","r_oth")) {
        census[[rcat]][zero_ind] <- mean(census[[rcat]], na.rm = TRUE)
      }
    }
    
    voters.census <- merge(
      voter.file[toupper(voter.file$state) == toupper(states[s]), ],
      census[, -drop], by = geo.merge, all.x  = TRUE)
    
    #Check if geolocation missing from census object
    if(any(is.na(voters.census$r_whi))){
      miss_ind <- which(is.na(voters.census$r_whi))
      message("The following locations in the voter.file are not available in the census data.",
           paste0("(listed as ", paste0(c("state",geo.merge), collapse="-"),"):\n"),
           paste(do.call(paste, c(unique(voters.census[miss_ind, c("state",geo.merge)]),
                                  sep="-")),
                 collapse = ", "))
      if(skip_bad_geos == TRUE) {
        message("NOTE: Skipping unavailable geolocations. Returning partial data set.")
        voters.census <- voters.census[!is.na(voters.census$r_whi),]
      }
      else {
        stop("Stopping predictions. Please revise census data and/or verify the correct year is being supplied. To skip these rows use 'skip_bad_geos = TRUE'")
      }
    }
      
    # }
    
    if (age || sex) {
      voters.census <- condition_on_age_sex(voters.census, year, age, sex)
    }
    
    keep.vars <- c(names(voter.file)[names(voter.file) != "agecat"], 
                   paste("r", c("whi", "bla", "his", "asi", "oth"), sep = "_"))
    df.out <- as.data.frame(rbind(df.out, voters.census[keep.vars]))
    
  }
  
  return(df.out)
}

# Replace Pr(Race | Geo) with Pr(Race | Geo, Age, Sex): for each voter,
# recompute r_whi, ..., r_oth from the Census counts of the voter's age
# category and/or sex within their geography. Voters whose age or sex does not
# match a Census cell, or whose cell has no population, keep Pr(Race | Geo).
condition_on_age_sex <- function(voters.census, year, age, sex) {
  races <- c("whi", "bla", "his", "asi", "oth")
  vars_ <- census_geo_api_names(year = year, age = age, sex = sex)
  
  cell <- rep("", nrow(voters.census))
  if (sex) {
    cell <- paste0(cell, "_", c("mal", "fem")[match(voters.census$sex, c(0, 1))])
  }
  if (age) {
    # Lower bounds of the 23 Census age categories (0-4, 5-9, ..., 85 and over)
    lower <- c(0, 5, 10, 15, 18, 20, 21, 22, 25, 30, 35, 40, 45, 50, 55, 60,
               62, 65, 67, 70, 75, 80, 85)
    cell <- paste0(cell, "_", findInterval(voters.census$age, lower))
  }
  
  unmatched <- 0
  for (k in unique(cell)) {
    rows <- which(cell == k)
    vars_k <- vars_[paste0("r", k, "_", races)]
    if (any(vapply(vars_k, is.null, logical(1)))) {
      unmatched <- unmatched + length(rows)
      next
    }
    pops <- matrix(
      vapply(vars_k, function(v) rowSums(voters.census[rows, v, drop = FALSE]), numeric(length(rows))),
      ncol = length(races)
    )
    cellPop <- rowSums(pops)
    ok <- cellPop > 0
    voters.census[rows[ok], paste0("r_", races)] <- pops[ok, , drop = FALSE] / cellPop[ok]
  }
  if (unmatched > 0) {
    message(unmatched, " record(s) have a missing or invalid age/sex; using Pr(Race | Geo) for them.")
  }
  
  voters.census
}
