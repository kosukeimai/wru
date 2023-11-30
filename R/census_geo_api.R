#' Census Data download function.
#'
#' \code{census_geo_api} retrieves U.S. Census geographic data for a given state.
#'
#' This function allows users to download U.S. Census geographic data (2010 or 2020), 
#' at either the county, tract, block, or place level, for a particular state. 
#'
#' @inheritParams get_census_data
#' @param state A required character object specifying which state to extract Census data for, 
#'  e.g., \code{"NJ"}.
#' @param geo A character object specifying what aggregation level to use. 
#'  Use `"block"`, `"block_group"`, `"county"`, `"place"`, `"tract"`, or `"zcta"`. 
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
#'  Warning: 2020 U.S. Census data is downloaded only when \code{\var{age}} and 
#'  \code{\var{sex}} are both \code{FALSE}.
#' @param retry The number of retries at the census website if network interruption occurs.
#' @param save_temp File indicating where to save the temporary outputs. 
#'  Defaults to NULL. If specified, the function will look for an .RData file
#'  with the same format as the expected output. 
#' @param counties A vector of counties contained in your data. If \code{NULL}, all counties are pulled.
#' Useful for smaller predictions where only a few counties are considered. Must be zero padded.
#' @return Output will be an object of class \code{list}, indexed by state names. It will 
#'  consist of the original user-input data with additional columns of Census geographic data.
#'
#' @examples
#' \dontshow{data(voters)}
#' \dontrun{census_geo_api(states = c("NJ", "DE"), geo = "block")}
#' \dontrun{census_geo_api(states = "FL", geo = "tract", age = TRUE, sex = TRUE)}
#' \dontrun{census_geo_api(states = "MA", geo = "place", age = FALSE, sex = FALSE,
#'  year = "2020")}
#'
#' @references
#' Relies on `get_census_api()`, `get_census_api_2()`, and `vec_to_chunk()` functions authored by Nicholas Nagle, 
#' available [here](https://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html).
#' 
#' @importFrom furrr future_map_dfr
#' @importFrom purrr map_dfr
#' @keywords internal

census_geo_api <- function(
    key = Sys.getenv("CENSUS_API_KEY"),
    state,
    geo = c("tract", "block", "block_group", "county", "place", "zcta"),
    age = FALSE,
    sex = FALSE,
    year = c("2020", "2010"),
    retry = 3,
    save_temp = NULL,
    counties = NULL
) {
  validate_key(key)
  
  geo <- tolower(geo)
  geo <- rlang::arg_match(geo)
  
  if (geo == "zcta") {
    return(
      census_geo_api_zcta(
        state = state,
        age = age,
        sex = sex,
        year = year,
        retry = retry,
        key = key
      )
    )
  }
  
  year <- as.character(year)
  year <- rlang::arg_match(year)
  
  census <- NULL
  state <- as_state_abbreviation(state)
  
  df.out <- NULL
  state.fips <- as_fips_code(state)
  
  vars <- census_geo_api_names(year = year, age = age, sex = sex)
  census_data_url <- census_geo_api_url(year = year)
  
  if (geo == "place") {
    geo.merge <- c("state", "place")
    region <- paste("for=place:*&in=state:", state.fips, sep = "")
    census <- get_census_api(census_data_url, key = key, var.names = unlist(vars), region = region, retry)
  }
  
  if (geo == "county") {
    geo.merge <- c("state", "county")
    
    if (is.null(counties)) {
      region <- paste("for=county:*&in=state:", state.fips, sep = "")
    } else {
      counties_paste <- paste0(counties, collapse = ",")
      region <- paste("for=county:",counties_paste,"&in=state:", state.fips, sep = "")
    }

    census <- get_census_api(census_data_url, key = key, var.names = unlist(vars), region = region, retry)
  }
  
  if (geo == "tract") {
    
    geo.merge <- c("state", "county", "tract")
    
    if (is.null(counties)) {
      region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
    } else {
      counties_paste <- paste0(counties, collapse = ",")
      region_county <- paste("for=county:",counties_paste,"&in=state:", state.fips, sep = "")
    }
    
    county_df <- get_census_api(census_data_url, key = key, var.names = unlist(vars), region = region_county, retry)
    
    if(is.null(counties)) {
      county_list <- county_df$county
    } else {
      county_list <- intersect(counties, county_df$county)
    }
    
    if(length(county_list) > 0) {
      census_tracts <- furrr::future_map_dfr(seq_along(county_list), function(county) {
        message(paste("County ", county, " of ", length(county_list), ": ", county_list[county], sep = ""))
        region_county <- paste("for=tract:*&in=state:", state.fips, "+county:", county_list[county], sep = "")
        get_census_api(data_url = census_data_url, key = key, var.names = unlist(vars), region = region_county, retry)
      })
      
      census <- rbind(census, census_tracts)
      rm(census_tracts)
    } else {
      message('There were no intersecting counties in your voter.file data (tract)')
    } 
  }
  
  if (geo == "block_group") {
    geo.merge <- c("state", "county", "tract", "block_group")
    
    if (is.null(counties)) {
      region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
    } else {
      counties_paste <- paste0(counties, collapse = ",")
      region_county <- paste("for=county:",counties_paste,"&in=state:", state.fips, sep = "")
    }
    
    county_df <- get_census_api(census_data_url, key = key, var.names = unlist(vars), region = region_county, retry)
    
    if(is.null(counties)) {
      county_list <- county_df$county
    } else {
      county_list <- intersect(counties, county_df$county)
    }
    
    if(length(county_list) > 0) {
      message('Running block_group by county...')
      
      census_blockgroup <- purrr::map_dfr(
        1:length(county_list), 
        function(county) {
          # too verbose, commenting out
          message(paste("County ", county, " of ", length(county_list), ": ", county_list[county], sep = ""))
          
          blockgroup <- paste("for=block+group:*&in=state:", state.fips, "+county:", county_list[county], sep = "")
  
          # message(region_tract)
          blockgroup_df <- get_census_api(census_data_url, key = key, var.names = unlist(vars), region = blockgroup, retry)
          names(blockgroup_df)[4] <- "block_group" # Fix name, it comes in with a space from api. 
          blockgroup_df
        }
      )
      message("\n") # new line for progress bar
      
      census <- rbind(census, census_blockgroup)
      rm(census_blockgroup)
    } else {
      message('There were no intersecting counties in your voter.file data (block)')
    }
  }
  
  if (geo == "block") {
    
    geo.merge <- c("state", "county", "tract", "block")
    
    if (is.null(counties)) {
      region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
    } else {
      counties_paste <- paste0(counties, collapse = ",")
      region_county <- paste("for=county:",counties_paste,"&in=state:", state.fips, sep = "")
    }
    
    county_df <- get_census_api(census_data_url, key = key, var.names = unlist(vars), region = region_county, retry)
    
    if(is.null(counties)) {
      county_list <- county_df$county
    } else {
      county_list <- intersect(counties, county_df$county)
    }
    
    if(length(county_list) > 0) {
      message('Running block by county...')
      
      census_blocks <- purrr::map_dfr(
        1:length(county_list), 
        function(county) {
          # too verbose, commenting out
          message(paste("County ", county, " of ", length(county_list), ": ", county_list[county], sep = ""))
          
          region_tract <- paste("for=tract:*&in=state:", state.fips, "+county:", county_list[county], sep = "")
          # message(region_tract)
          tract_df <- get_census_api(census_data_url, key = key, var.names = unlist(vars), region = region_tract, retry)
          tract_list <- tract_df$tract
          
          furrr::future_map_dfr(1:length(tract_list), function(tract) {
            message(paste("Tract ", tract, " of ", length(tract_list), ": ", tract_list[tract], sep = ""))
            
            region_block <- paste("for=block:*&in=state:", state.fips, "+county:", county_list[county], "+tract:", tract_list[tract], sep = "")
            get_census_api(census_data_url, key = key, var.names = unlist(vars), region = region_block, retry)
          })
        }
      )
      message("\n") # new line for progress bar
      
      census <- rbind(census, census_blocks)
      rm(census_blocks)
    } else {
      message('There were no intersecting counties in your voter.file data (block)')
    } 
  }
  
  census <- dplyr::mutate(census, state = as_state_abbreviation(state))
  
  r_columns <- purrr::map(vars, function(vars) rowSums(census[vars]))
  
  census <- dplyr::bind_cols(census, r_columns)
  census <- dplyr::group_by(census, dplyr::across(dplyr::any_of("state")))
  census <- dplyr::mutate(
    census,
    dplyr::across(
      # Divide all r_columns by the total population of the corresponding race
      dplyr::all_of(names(r_columns)),
      function(x) {
        x / sum(
          dplyr::pick(
            sub("^.+_(.{3})$", "r_\\1", dplyr::cur_column(), perl = TRUE)
          )
        )
      }
    )
  )
  census <- dplyr::ungroup(census)
  
  census
}
