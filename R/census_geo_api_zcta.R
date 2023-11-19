# @staticimports pkg:stringstatic
#   str_pad

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
  
  if (year == "2020") {
    census_data_url <- "https://api.census.gov/data/2020/dec/dhc?"
    prefix <- "P12"
    separator <- "_"
    suffix <- "N"
  } else if (year %in% c("2010", "2000")) {
    census_data_url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?")
    prefix <- "P012"
    separator <- ""
    suffix <- ""
  }
  
  race_codes <- list(
    "whi" = "I",
    "bla" = "B",
    "his" = "H",
    "asi" = c("D", "E"),
    "oth" = c("C", "F", "G")
  )
  
  sex_codes <- c("mal" = 2, "fem" = 26)
  
  age_codes <- 1:23
  
  numeric_codes <- if (age) {
    unlist(purrr::map(sex_codes, function(x) x + age_codes))
  } else if (sex) {
    sex_codes
  } else {
    1
  }
  numeric_codes <- str_pad(numeric_codes, width = 3, side = "left", pad = "0")
  
  vars <- expand.grid(
    prefix,
    unlist(race_codes),
    separator,
    numeric_codes,
    suffix,
    stringsAsFactors = FALSE
  )
  vars <- apply(vars, 1, paste, collapse = "")
  
  region <- paste0(
    "for=zip%20code%20tabulation%20area%20(or%20part):*&in=state:",
    paste(as_fips_code(state), collapse = ",")
  )
  
  census <- get_census_api(
    census_data_url,
    key = key,
    var.names = vars,
    region = region,
    retry
  )
  
  if (!age && !sex) {
    ## Calculate Pr(Geolocation | Race)
    
    for (i in seq_along(race_codes)) {
      var_name <- paste("r", names(race_codes)[[i]], sep = "_")
      
      code <- paste0(prefix, race_codes[[i]], separator, "001", suffix)
      
      census[var_name] <- rowSums(census[code])
    }
  } else if (!age && sex) {
    ## Calculate Pr(Geolocation, Sex | Race)
    
    for (race in seq_along(race_codes)) {
      for (sex in seq_along(sex_codes)) {
        var_name <- paste(
          "r",
          names(sex_codes)[[sex]],
          names(race_codes)[[race]],
          sep = "_"
        )
        
        code <- paste0(
          prefix,
          race_codes[[race]],
          separator,
          str_pad(sex_codes[[sex]], width = 3, pad = "0"),
          suffix
        )
        
        census[var_name] <- rowSums(census[code])
      }
    }
  } else if (age && !sex) {
    ## Calculate Pr(Geolocation, Age Category | Race)
    
    for (race in seq_along(race_codes)) {
      for (age_category in age_codes) {
        var_name <- paste(
          "r",
          age_category,
          names(race_codes)[[race]],
          sep = "_"
        )
        
        code <- paste0(
          prefix,
          race_codes[[race]],
          separator,
          str_pad(sex_codes + age_category, width = 3, pad = "0"),
          suffix
        )
        
        census[var_name] <- rowSums(census[code])
      }
    }
  } else if (age && sex) {
    ## Calculate Pr(Geolocation, Sex, Age Category | Race)
    
    for (race in seq_along(race_codes)) {
      for (age_category in age_codes) {
        for (sex in seq_along(sex_codes)) {
          var_name <- paste(
            "r",
            names(sex_codes)[[sex]],
            age_category,
            names(race_codes)[[race]],
            sep = "_"
          )
          
          code <- paste0(
            prefix,
            race_codes[[race]],
            separator,
            str_pad(sex_codes[[sex]] + age_category, width = 3, pad = "0"),
            suffix
          )
          
          census[var_name] <- rowSums(census[code])
        }
      }
    }
  }
  
  census <- dplyr::group_by(census, dplyr::across(dplyr::any_of("state")))
  census <- dplyr::mutate(
    census,
    state = as_state_abbreviation(state),
    dplyr::across(dplyr::starts_with("r_"), function(x) x / sum(x))
  )
  census <- dplyr::ungroup(census)
  census <- dplyr::select(census, -dplyr::starts_with(prefix))
  
  names(census)[[2]] <- "zcta"
  
  census
}
