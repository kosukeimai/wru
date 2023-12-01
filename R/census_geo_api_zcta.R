census_geo_api_zcta <- function(
    census_data_url,
    key,
    vars,
    state,
    counties,
    retry
) {
  if (!is.null(counties)) {
    cli::cli_abort(
      '{.arg counties} must be {.code NULL} when {.code geo = "zcta"},
        because ZCTA-level census data split by county is not available.'
    )
  }
  
  region <- paste0(
    "for=zip%20code%20tabulation%20area%20(or%20part):*&in=state:",
    paste(as_fips_code(state), collapse = ",")
  )
  
  census <- get_census_api(
    census_data_url,
    key = key,
    var.names = unlist(vars),
    region = region,
    retry
  )
  
  names(census)[[2]] <- "zcta"
  
  census
}
