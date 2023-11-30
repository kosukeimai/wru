#' Census download function for state-ZCTA-level data
#'
#' @inheritParams census_geo_api
#' @param ... These dots are for future extensions and must be empty.
#'
#' @return A [data.frame] with columns
#'   `state`, `zcta`, `r_whi`, `r_bla`, `r_his`, `r_asi`, and `r_oth`.
#'
#' @examplesIf nzchar(Sys.getenv("CENSUS_API_KEY"))
#' \dontrun{census_geo_api_zcta(state = c("DE", "NJ")}
#' \dontrun{census_geo_api_zcta(state = "FL", age = TRUE, sex = TRUE)}
#' \dontrun{census_geo_api_zcta(state = "MA", age = FALSE, sex = FALSE, year = "2020")}
#' 
#' @keywords internal
census_geo_api_zcta <- function(
    state,
    ...,
    age = FALSE,
    sex = FALSE,
    year = c("2020", "2010", "2000"),
    retry = 3,
    key = Sys.getenv("CENSUS_API_KEY")
) {
  # Validate arguments
  rlang::check_dots_empty()
  validate_key(key)
  assert_boolean(age)
  assert_boolean(sex)
  year <- as.character(year)
  year <- rlang::arg_match(year)
  
  census_data_url <- census_geo_api_url(year = year)
  vars <- census_geo_api_names(year = year, age = age, sex = sex)
  
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
  
  census <- dplyr::mutate(census, state = as_state_abbreviation(state))
  names(census)[[2]] <- "zcta"
  
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
