library(wru)

# Note: must provide a valid U.S. Census API key for test cases that use U.S. Census statistics
k <- NULL

# Load data
data(voters)

test_that("Tests surname only predictions", {
  set.seed(12345)
  
  # Prediction using surname only
  x <- suppressWarnings(predict_race(voter.file = voters, surname.only = TRUE))
  # Test and confirm prediction output is as expected
  expect_equal(dim(x), c(10, 18))
  expect_equal(sum(is.na(x)), 0)
  expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0676)
  expect_equal(round(x[x$surname == "Johnson", "pred.his"], 4), 0.0236)
})

test_that("Tests predictions using the Census object", {
  # set random seed
  set.seed(12345)
  
  if (!is.null(k)) {
    # Remove two NY cases from dataset to reduce the amount of the computation in the following test
    voters.dc.nj <- voters[c(-3, -7), ]  
    
    # Create Census data object covering DC and NJ
    census.dc.nj <- get_census_data(key = k, state = c("DC", "NJ"), census.geo = "tract", age = TRUE, sex = FALSE)  
    
    # Prediction using the Census object created in the previous step; tract-level statistics used in prediction
    x = predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = census.dc.nj, age = TRUE, sex = FALSE, party = "PID")
    # test and comfirm the prediction output as expected
    expect_equal(dim(x), c(8,18))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0644)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0042)
    
    # Build a Census object by parts; both county-level and tract-level statistics needed for tract-level predictions
    censusObj2  <- list()
    county.dc <- census_geo_api(key = k, state = "DC", geo = "county", age = TRUE, sex = FALSE, save_temp = NULL)
    tract.dc <- census_geo_api(key = k, state = "DC", geo = "tract", age = TRUE, sex = FALSE, save_temp = NULL)
    censusObj2[["DC"]] <- list(state = "DC", county = county.dc, tract = tract.dc, age = TRUE, sex = FALSE)
    tract.nj <- census_geo_api(key = k, state = "NJ", geo = "tract", age = TRUE, sex = FALSE, save_temp = NULL)
    county.nj <- census_geo_api(key = k, state = "NJ", geo = "county", age = TRUE, sex = FALSE, save_temp = NULL)
    censusObj2[["NJ"]] <- list(state = "NJ", county = county.nj, tract = tract.nj, age = TRUE, sex = FALSE)
    
    # Prediction using the Census object built in the previous step; county-level statistics used in prediction
    x = predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = censusObj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, County)
    # test and comfirm the prediction output as expected
    expect_equal(dim(x), c(8,18))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0441)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0163)
    
    # Prediction using the Census object built in the previous step; tract-level statistics used in prediction
    x = predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract, Party)
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(8,18))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0644)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0042)
  }
})

test_that("Tests predictions using Census API key", {
  set.seed(12345)
  
  if (!is.null(k)) {
    # Prediction using a valid Census API key; tract-level statistics used
    x = predict_race(voter.file = voters, census.geo = "tract", census.key = k, party = "PID")
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(10,18))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 1)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0819)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0034)
    
    # Prediction using a valid Census API key; place-level statistics used
    x = predict_race(voter.file = voters, census.geo = "place", census.key = k, sex = TRUE)
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(10,18))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 1)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0566)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0197)
    
    # Remove two NY cases from dataset to reduce the amount of the computation in the following test
    voters.dc.nj <- voters[c(-3, -7), ]  
    
    # Prediction using a valid Census API key; block-level statistics used
    x = predict_race(voter.file = voters.dc.nj, census.geo = "block", census.key = k, sex = TRUE)
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(8,18))
    expect_equal(sum(is.na(x)), 5)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(sum(is.na(x[x$surname != "Ratkovic",])), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.2978)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0123)
  }
}) 

test_that("Tests predictions using Census data from a different year", {
  set.seed(12345)
  
  if (!is.null(k)) {
    # Prediction using Census statistics from the year 2000, which is different from the default year (2010)
    x = predict_race(voter.file = voters, census.geo = "tract", census.key = k, party = "PID", surname.year = 2000)
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(10,18))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 1)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0982)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0023)
  }
})
