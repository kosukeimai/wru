# Note: must provide a valid U.S. Census API key for test cases that use U.S. Census statistics
# > usethis::edit_r_profile
# Sys.setenv("CENSUS_API_KEY" = "yourkey")
# For testing package coverage use: Sys.setenv("NOT_CRAN" = "TRUE")
options("piggyback.verbose" = FALSE)
options("wru_data_wd" = TRUE)

skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")))
test_that("old predict_race fxn returns sensible predictions for test names", {
  data("voters")
  x <- .predict_race_old(
    voter.file = voters,
    census.surname = TRUE,
    surname.only = TRUE,
    surname.year = 2010,
    census.geo = "tract",
    census.key = Sys.getenv("CENSUS_API_KEY"),
    age = FALSE,
    sex = FALSE,
    year = "2010",
    retry = 3,
    impute.missing = TRUE,
    use.counties = FALSE
  )
  expect_equal(x[x$surname == "Lopez", "pred.whi"], 0.0486000, tolerance = .000001)
  expect_equal(x[x$surname == "Khanna", "pred.whi"], 0.0676000, tolerance = .000001)
  expect_equal(x[x$surname == "Lopez", "pred.bla"], 0.00570000, tolerance = .000001)
  expect_equal(x[x$surname == "Khanna", "pred.bla"], 0.00430000, tolerance = .000001)
  expect_equal(x[x$surname == "Lopez", "pred.his"], 0.92920000, tolerance = .000001) #assumed to be high Hispanic score
  expect_equal(x[x$surname == "Zhou", "pred.asi"], 0.98200000, tolerance = .000001) #assumed to be high Asian score
  
})
