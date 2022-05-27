# Note: must provide a valid U.S. Census API key for test cases that use U.S. Census statistics
# > usethis::edit_r_profile
# Sys.setenv("CENSUS_API_KEY" = "yourkey")


test_that("Tests surname only predictions", {
  set.seed(42)
  data(voters)

  # Prediction using surname only
  x <- suppressMessages(predict_race(voter.file = voters, surname.only = TRUE, census.surname = TRUE))
  # Test and confirm prediction output is as expected
  expect_equal(dim(x), c(10, 20))
  expect_equal(sum(is.na(x)), 0)
  expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.00, tolerance = 0.01)
  expect_equal(round(x[x$surname == "Johnson", "pred.his"], 4), 0.026, tolerance = 0.01)
})

test_that("Test BISG NJ at county level", {
  set.seed(42)
  data(voters)
  
  census <- readRDS(test_path("data/census_test_nj_block_2010.rds"))

  x <- suppressMessages(predict_race(
      voter.file = voters[voters$state == "NJ",],
      census.geo = "county",
      census.data = census
    ))

  expect_equal(as.character(x$VoterID), as.character(c(1, 2, 4, 5, 6, 8, 9)))
  expect_equal(dim(x), c(7, 20))
  expect_equal(sum(is.na(x)), 0L)
  expect_equal(sum(x$surname == "Johnson"), 0)
  expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0314, tolerance = 0.01)
  expect_equal(round(x[x$surname == "Khanna", "pred.asi"], 4), 0.9367, tolerance = 0.01)
  expect_equal(round(x[x$surname == "Fifield", "pred.whi"], 4), 0.9230, tolerance = 0.01)
  expect_equal(round(x[x$surname == "Lopez", "pred.his"], 4), 0.9178, tolerance = 0.01)
})

test_that("Test fBISG NJ at tract level", {
  set.seed(42)
  data(voters)
  
  census <- readRDS(test_path("data/census_test_nj_block_2010.rds"))

  x <- suppressMessages(predict_race(
    voter.file = voters[voters$state == "NJ",],
    census.geo = "tract",
    census.data = census,
    model = "fBISG"
  ))
  
  expect_equal(as.character(x$VoterID), as.character(c(1, 2, 4, 5, 6, 8, 9)))
  expect_equal(dim(x), c(7, 20))
  expect_equal(sum(is.na(x)), 0L)
  expect_equal(sum(x$surname == "Johnson"), 0)
  expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.063, tolerance = 0.01) # 0.0644
  expect_equal(round(x[x$surname == "Lopez", "pred.his"], 4), 0.78, tolerance = 0.01) # 0.0644
})

test_that("BISG NJ at block level", {
  set.seed(42)
  data(voters)
  census <- readRDS(test_path("data/census_test_nj_block_2010.rds"))

  x <- suppressMessages(predict_race(
    voter.file = voters[voters$state == "NJ", ], 
    census.geo = "block", 
    census.key = NULL, 
    census.data = census, 
    use_counties = TRUE)
  )
  
  expect_equal(dim(x), c(7, 20))
  expect_equal(sum(is.na(x$pred.asi)), 1L)
  expect_true(!any(duplicated(x$surname)))
  expect_equal(x[x$surname == "Khanna", "pred.asi"], 0.7640, tolerance = 0.01)
  expect_equal(x[x$surname == "Zhou", "pred.asi"], 1.0, tolerance = 0.1)
  expect_equal(x[x$surname == "Lopez", "pred.his"], 0.7, tolerance = 0.1)
})