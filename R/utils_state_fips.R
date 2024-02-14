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

#' Convert between state names, postal abbreviations, and FIPS codes
#'
#' @param x A [numeric] or [character] vector of state names,
#'   postal abbreviations, or FIPS codes.
#'   Matches for state names and abbreviations are not case sensitive.
#'   FIPS codes may be matched from numeric or character vectors,
#'   with or without leading zeroes.
#'
#' @return
#' \describe{
#'   \item{`as_state_fips_code()`}{
#'     A [character] vector of two-digit FIPS codes.
#'     One-digit FIPS codes are prefixed with a leading zero,
#'     e.g., `"06"` for California.
#'   }
#'   \item{`as_state_abbreviation()`}{
#'     A [character] vector of two-letter postal abbreviations,
#'     e.g., `"CA"` for California.
#'   }
#' }
#'
#' @examples
#' as_fips_code("california")
#' as_state_abbreviation("california")
#' 
#' # Character vector matches ignore case
#' as_fips_code(c("DC", "Md", "va"))
#' as_state_abbreviation(c("district of columbia", "Maryland", "VIRGINIA"))
#' 
#' # Note that `3` and `7` are standardized to `NA`,
#' # because no state is assigned those FIPS codes
#' as_fips_code(1:10)
#' as_state_abbreviation(1:10)
#' 
#' # You can even mix methods in the same vector
#' as_fips_code(c("utah", "NM", 8, "04"))
#' as_state_abbreviation(c("utah", "NM", 8, "04"))
#'
#' @keywords internal
#' @export
as_fips_code <- function(x) {
  state_fips <- wru::state_fips
  state_fips$state_code[
    dplyr::coalesce(
      match(toupper(x), state_fips$state),
      match(tolower(x), tolower(state_fips$state_name)),
      match(suppressWarnings(as.numeric(x)), as.numeric(state_fips$state_code))
    )
  ]
}

#' @rdname as_fips_code
#' @export
as_state_abbreviation <- function(x) {
  state_fips <- wru::state_fips
  state_fips$state[
    dplyr::coalesce(
      match(toupper(x), state_fips$state),
      match(tolower(x), tolower(state_fips$state_name)),
      match(suppressWarnings(as.numeric(x)), as.numeric(state_fips$state_code))
    )
  ]
}