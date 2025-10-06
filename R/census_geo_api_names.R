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
    sex_codes <- c("_mal" = 2, "_fem" = 26)
    age_codes <- 1:23
    names(age_codes) <- paste0("_", age_codes)
  } else if (year %in% c("2010", "2000")) {
    prefix <- "PCT012"
    separator <- ""
    suffix <- ""
    sex_codes <- c("_mal" = 2, "_fem" = 106)
    age_codes <- list(
      "_1" = 1:5,
      "_2" = 6:10,
      "_3" = 11:15,
      "_4" = 16:18,
      "_5" = 19:20,
      "_6" = 21,
      "_7" = 22,
      "_8" = 23:25,
      "_9" = 26:30,
      "_10" = 31:35,
      "_11" = 36:40,
      "_12" = 41:45,
      "_13" = 46:50,
      "_14" = 51:55,
      "_15" = 56:60,
      "_16" = 61:62,
      "_17" = 63:65,
      "_18" = 66:67,
      "_19" = 68:70,
      "_20" = 71:75,
      "_21" = 76:80,
      "_22" = 81:85,
      "_23" = 86:103
    )
  }
  
  race_codes <- list(
    "_whi" = "I",
    "_bla" = "J",
    "_his" = "H",
    "_asi" = c("L", "M"),
    "_oth" = c("K", "N", "O")
  )
  
  numeric_codes <- if (age && sex) {
    age_sex_codes <- purrr::imap(
      sex_codes,
      function(sex_code, name) {
        codes <- purrr::map(
          age_codes,
          function(age_code) {
            str_pad(age_code + sex_code, 3, "left", pad = "0")
          }
        )
        names(codes) <- paste0(name, names(codes))
        codes
      }
    )
    
    do.call(c, unname(age_sex_codes))
  } else if (age) {
    purrr::map(
      age_codes,
      function(age_code) {
        unlist(
          purrr::map(
            sex_codes,
            function(sex_code) {
              str_pad(age_code + sex_code, 3, "left", pad = "0")
            }
          )
        )
      }
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

census_geo_api_names_legacy <- function(year) {
  if (year == 2020) {
    return(
      list(
        r_whi = 'P2_005N',
        r_bla = 'P2_006N',
        r_his = 'P2_002N',
        r_asi = c('P2_008N', 'P2_009N'),
        r_oth = c('P2_007N', 'P2_010N', 'P2_011N')
      )
    )
  }
      
  list(
    r_whi = 'P005003',
    r_bla = 'P005004',
    r_his = 'P005010',
    r_asi = c('P005006', 'P005007'),
    r_oth = c('P005005', 'P005008', 'P005009')
  )
}

#' @rdname census_geo_api_names
census_geo_api_url <- function(year = c("2020", "2010", "2000")) {
  year <- as.character(year)
  year <- rlang::arg_match(year)
  
  if (year == "2020") return("https://api.census.gov/data/2020/dec/dhc?")
  paste0("https://api.census.gov/data/", year, "/dec/sf1?")
}