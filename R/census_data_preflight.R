#' Preflight census data
#' 
#' @inheritParams predict_race
#' @keywords internal

census_data_preflight <- function(census.data, census.geo, year) {
  vars_ <- unlist(census_geo_api_names(year = year))
  legacy_vars <- unlist(census_geo_api_names_legacy(year = year))
  
  test <- lapply(census.data, function(x) {
    nms_to_test <- names(x[[census.geo]])
    all(vars_ %in% nms_to_test) || all(legacy_vars %in% nms_to_test)
  }) 
  missings <- names(test)[!unlist(test)]
  
  if(any(!unlist(test))) {
    stop(
      paste0(
        "Missing ", 
        paste0(vars_, collapse = ", "), 
        " from census.data object. Please update your census.data by",
        " running `get_census_data` again."
      )
    )
  }
}