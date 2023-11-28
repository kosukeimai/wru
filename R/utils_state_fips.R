#' Dataset with FIPS codes for US states
#'
#' Dataset including FIPS codes and postal abbreviations for each U.S. state,
#' district, and territory.
#'
#' @format
#' A tibble with 57 rows and 3 columns:
#' \describe{
#'   \item{`state`}{Two-letter postal abbreviation}
#'   \item{`state_code`}{Two-digit FIPS code}
#'   \item{`state_name`}{English name}
#' }
#' @source Derived from [tidycensus::fips_codes()]
"state_fips"
