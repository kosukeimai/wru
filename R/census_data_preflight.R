#' Preflight census data
#' 
#' @param census.data See documentation in \code{race_predict}.
#' @param census.geo See documentation in \code{race_predict}.
#' @param year See documentation in \code{race_predict}.
census_data_preflight <- function(census.data, census.geo, year) {
  
  if (year != "2020"){
    vars_ <- c(
      pop_white = 'P005003', pop_black = 'P005004',
      pop_aian = 'P005005', pop_asian = 'P005006',
      pop_nhpi = 'P005007', pop_other = 'P005008', 
      pop_two = 'P005009', pop_hisp = 'P005010'
    )
  } else {
    vars_ <- c(
      pop_white = 'P2_005N', pop_black = 'P2_006N',
      pop_aian = 'P2_007N', pop_asian = 'P2_008N', 
      pop_nhpi = 'P2_009N', pop_other = 'P2_010N', 
      pop_two = 'P2_011N', pop_hisp = 'P2_002N'
    )
  }
  
  test <- lapply(census.data, function(x) {
    nms_to_test <- names(x[[census.geo]])
    all(vars_ %in% nms_to_test)
  }) 
  missings <- names(test)[!unlist(test)]
  
  if(any(!unlist(test))) {
    stop(
      paste0(
        "Missing ", 
        paste0(vars_, collapse = ", "), 
        " from census.data object. Please run `get_census_data` again"
      )
    )
  }
}