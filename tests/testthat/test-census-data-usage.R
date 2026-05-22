## Regression tests for #161: census.data handling in census_helper_new().

## A stand-in census_geo_api that records the arguments it is called with and
## then aborts, so we can inspect how census_helper_new dispatched to it without
## hitting the Census API. Formals mirror the real function so positional
## argument matching behaves identically.
capturing_api <- function(captured) {
  function(key = Sys.getenv("CENSUS_API_KEY"), state, geo, age = FALSE, sex = FALSE,
           year = c("2020", "2010"), retry = 3, save_temp = NULL, counties = NULL) {
    captured$called <- TRUE
    captured$year <- year
    stop("captured")
  }
}

test_that("use.counties downloads pass year, not retry, to census_geo_api (#161)", {
  skip_on_cran()
  captured <- new.env()
  captured$called <- FALSE
  testthat::local_mocked_bindings(census_geo_api = capturing_api(captured), .package = "wru")

  vf <- data.frame(surname = "Smith", state = "NJ", county = "021",
                   tract = "004000", stringsAsFactors = FALSE)
  ## Non-null census.data with a year mismatch routes into the download branch
  ## without needing a Census key.
  cd <- list(NJ = list(year = "2010", age = FALSE, sex = FALSE, tract = data.frame()))

  tryCatch(
    suppressMessages(census_helper_new(
      key = "", voter.file = vf, states = "NJ", geo = "tract",
      year = "2020", census.data = cd, use.counties = TRUE
    )),
    error = function(e) NULL
  )

  expect_true(captured$called)
  expect_equal(captured$year, "2020")
})

test_that("provided age/sex census.data is used, not re-downloaded (#161)", {
  skip_on_cran()
  captured <- new.env()
  captured$called <- FALSE
  testthat::local_mocked_bindings(census_geo_api = capturing_api(captured), .package = "wru")

  vf <- data.frame(surname = "Smith", state = "NJ", county = "021",
                   tract = "004000", stringsAsFactors = FALSE)
  ## Cached data built with age/sex TRUE that matches the requested age/sex and
  ## year must be used as-is; previously this was always rejected (#161).
  tract_tbl <- data.frame(
    county = "021", tract = "004000",
    r_whi = 0.5, r_bla = 0.2, r_his = 0.2, r_asi = 0.05, r_oth = 0.05,
    stringsAsFactors = FALSE
  )
  cd <- list(NJ = list(year = "2020", age = TRUE, sex = TRUE, tract = tract_tbl))

  out <- suppressMessages(census_helper_new(
    key = "", voter.file = vf, states = "NJ", geo = "tract",
    age = TRUE, sex = TRUE, year = "2020", census.data = cd
  ))

  expect_false(captured$called)          # no Census API call: cache was used
  expect_true("r_whi" %in% names(out))
})
