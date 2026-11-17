## #150: options(wru_progress = FALSE) suppresses the furrr progress bar during
## Census downloads without silencing the other messages.

## Drive census_geo_api() to its tract-level furrr call with the network mocked
## out, and capture the .progress value handed to furrr::future_map_dfr().
run_capturing_progress <- function() {
  captured <- new.env()
  testthat::local_mocked_bindings(validate_key = function(key, ...) "fake-key", .package = "wru")
  testthat::local_mocked_bindings(
    get_census_api = function(...) data.frame(county = "001", stringsAsFactors = FALSE),
    .package = "wru"
  )
  testthat::local_mocked_bindings(
    future_map_dfr = function(.x, .f, ..., .progress = FALSE) {
      captured$progress <- .progress
      data.frame()
    },
    .package = "furrr"
  )
  tryCatch(
    suppressMessages(census_geo_api(key = "fake-key", state = "NJ", geo = "tract", year = "2020")),
    error = function(e) NULL
  )
  captured$progress
}

test_that("wru_progress default shows the progress bar (#150)", {
  old <- options(wru_progress = NULL); on.exit(options(old))
  expect_true(run_capturing_progress())
})

test_that("options(wru_progress = FALSE) suppresses the progress bar (#150)", {
  old <- options(wru_progress = FALSE); on.exit(options(old))
  expect_false(run_capturing_progress())
})
