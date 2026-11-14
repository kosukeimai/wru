## Regression tests for census_helper_new():
## 1) `if (toupper(states) == "ALL")` errors on R >= 4.2 when `states` has
##    length > 1 (e.g., `states = c("NJ", "NY")`).
## 2) `use.counties = TRUE` previously filtered `voter.file$state == state`
##    case-sensitively, passing `counties = character(0)` when `voter.file$state`
##    was lowercase (e.g., `"nj"`).
## 3) `predict_race()` checked US territories (`"PR"`, etc.) case-sensitively
##    before uppercasing `voter.file$state`.

test_that("census_helper_new accepts a multi-element `states` vector without condition-length error", {
  census <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  census$NY <- census$NJ
  census$NY$state <- "NY"
  census$NY$county$state <- "NY"

  vf <- data.frame(
    surname = c("Smith", "Garcia"),
    state   = c("NJ", "NY"),
    county  = c("021", "021"),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    out <- suppressMessages(census_helper_new(
      voter.file  = vf,
      states      = c("NJ", "NY"),
      geo         = "county",
      year        = "2020",
      census.data = census
    ))
  )
  expect_equal(nrow(out), 2L)
})

test_that("census_helper_new with use.counties = TRUE matches lowercase voter.file$state", {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    census_geo_api = function(key = Sys.getenv("CENSUS_API_KEY"), state, geo,
                              age = FALSE, sex = FALSE, year = c("2020", "2010"),
                              retry = 3, save_temp = NULL, counties = NULL) {
      captured$counties <- counties
      stop("captured")
    },
    .package = "wru"
  )

  vf <- data.frame(
    surname = "Smith",
    state   = "nj",
    county  = "021",
    tract   = "004000",
    stringsAsFactors = FALSE
  )
  cd <- list(NJ = list(year = "2010", age = FALSE, sex = FALSE, tract = data.frame()))

  tryCatch(
    suppressMessages(census_helper_new(
      key = "", voter.file = vf, states = "all", geo = "tract",
      year = "2020", census.data = cd, use.counties = TRUE
    )),
    error = function(e) NULL
  )

  expect_equal(captured$counties, "021")
})

test_that("predict_race rejects lowercase US territory codes", {
  vf <- data.frame(surname = "Smith", state = "pr", stringsAsFactors = FALSE)
  expect_error(
    suppressMessages(predict_race(voter.file = vf, surname.only = TRUE)),
    "The wru package does not support US territories"
  )
})
