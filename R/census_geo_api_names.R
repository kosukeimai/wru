# @staticimports pkg:stringstatic
#   str_pad

#' Census geo API helper functions
#'
#' @inheritParams census_geo_api
#'
#' @return
#' \describe{
#'   \item{`census_geo_api_names()`}{
#'     A named list of [character] vectors whose values correspond to columns
#'     of a Census API table and whose names represent the new columns they are
#'     used to calculate in [census_geo_api()].
#'   }
#'   \item{`census_geo_api_url()`}{
#'     A [character] string containing the base of the URL to a
#'     Census API table.
#'   }
#' }
#' @keywords internal
census_geo_api_names <- function(
    year = c("2020", "2010", "2000"),
    age = FALSE,
    sex = FALSE
) {
  year <- as.character(year)
  year <- rlang::arg_match(year)
  
  assert_boolean(age)
  assert_boolean(sex)
  
  if (year == "2020") {
    prefix <- "P12"
    separator <- "_"
    suffix <- "N"
  } else if (year %in% c("2010", "2000")) {
    prefix <- "P012"
    separator <- ""
    suffix <- ""
  }
  
  race_codes <- list(
    "_whi" = "I",
    "_bla" = "B",
    "_his" = "H",
    "_asi" = c("D", "E"),
    "_oth" = c("C", "F", "G")
  )
  
  sex_codes <- c("_mal" = 2, "_fem" = 26)
  
  age_codes <- 1:23
  names(age_codes) <- paste0("_", age_codes)
  
  numeric_codes <- if (age && sex) {
    age_sex_codes <- unlist(
      purrr::map(sex_codes, function(x) x + age_codes)
    )
    names(age_sex_codes) <- sub(".", "", names(age_sex_codes), fixed = TRUE)
    age_sex_codes[] <- str_pad(age_sex_codes, 3, "left", pad = "0")
    as.list(age_sex_codes)
  } else if (age) {
    purrr::map(
      age_codes,
      function(x) str_pad(x + sex_codes, 3, "left", pad = "0")
    )
  } else if (sex) {
    sex_codes[] <- str_pad(sex_codes, 3, "left", pad = "0")
    as.list(sex_codes)
  }
  
  numeric_codes <- c("001", numeric_codes)
  
  combinations <- expand.grid(
    prefix = prefix,
    race_codes = race_codes,
    separator = separator,
    numeric_codes = numeric_codes,
    suffix = suffix,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  vars <- purrr::pmap(
    combinations,
    function(prefix, race_codes, separator, numeric_codes, suffix) {
      inner_combinations <- expand.grid(
        prefix = prefix,
        race_codes = race_codes,
        separator = separator,
        numeric_codes = numeric_codes,
        suffix = suffix,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
      
      apply(inner_combinations, 1, paste, collapse = "")
    }
  )
  
  names(vars) <- paste0(
    "r",
    names(combinations$numeric_codes),
    names(combinations$race_codes)
  )
  
  vars
}

#' @rdname census_geo_api_names
census_geo_api_url <- function(year = c("2020", "2010", "2000")) {
  year <- as.character(year)
  year <- rlang::arg_match(year)
  
  if (year == "2020") return("https://api.census.gov/data/2020/dec/dhc?")
  paste0("https://api.census.gov/data/", year, "/dec/sf1?")
}