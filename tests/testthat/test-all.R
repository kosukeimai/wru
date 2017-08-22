rm(list=ls())
library(wru)
context("tests wru")

#
# Note: must provide a valid census key for test cases that use census statistics
# 
k <- NULL



# load the data
data(voters)

test_that("tests surname only predictions", {
  # set random seed
  set.seed(12345)

  # prediction using surname only
  x <- predict_race(voter.file = voters, surname.only = T)
  # test and comfirm the prediction output as expected
  expect_that(dim(x), is_equivalent_to(c(10,18)))
  expect_that(sum(is.na(x)), is_equivalent_to(0))
  expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0676))
  expect_that(round(x[x$surname == "Johnson", "pred.his"], 4), is_equivalent_to(0.0236))
})

test_that("tests predictions using the census object", {
  # set random seed
  set.seed(12345)
  
  if (!is.null(k)) {
    # remove two NY cases from dataset to reduce the amount of the computation in the following test
    voters.dc.nj <- voters[c(-3, -7), ]  
  
    # create Census data object covering DC and NJ
    census.dc.nj <- get_census_data(key = k, state = c("DC", "NJ"), census.geo = "tract", age = TRUE, sex = FALSE)  
  
    # prediction using the census object created in the previous step; tract level statistics is used in prediction
    x = predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = census.dc.nj, age = TRUE, sex = FALSE, party = "PID")
    # test and comfirm the prediction output as expected
    expect_that(dim(x), is_equivalent_to(c(8,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(0))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0644))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0042))
    
    # build a census object by parts; both county and tract statistics are needed to predict at the tract level
    censusObj2  <- list()
    county.dc <- census_geo_api(key = k, state = "DC", geo = "county", age = TRUE, sex = FALSE)
    tract.dc <- census_geo_api(key = k, state = "DC", geo = "tract", age = TRUE, sex = FALSE)
    censusObj2[["DC"]] <- list(state = "DC", county = county.dc, tract = tract.dc, age = TRUE, sex = FALSE)
    tract.nj <- census_geo_api(key = k, state = "NJ", geo = "tract", age = TRUE, sex = FALSE)
    county.nj <- census_geo_api(key = k, state = "NJ", geo = "county", age = TRUE, sex = FALSE)
    censusObj2[["NJ"]] <- list(state = "NJ", county = county.nj, tract = tract.nj, age = TRUE, sex = FALSE)
    
    # prediction using the census object built in the previous step; county level statistics is used in prediction
    x = predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = censusObj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, County)
    # test and comfirm the prediction output as expected
    expect_that(dim(x), is_equivalent_to(c(8,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(0))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0441))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0163))
    
    # prediction using the census object built in the previous step; tract level statistics is used in prediction
    x = predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract, Party)
    # test and comfirm the prediction output as expected
    expect_that(dim(x), is_equivalent_to(c(8,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(0))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0644))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0042))
  }
})

test_that("tests predictions using the census key", {
  # set random seed
  set.seed(12345)
  
  if (!is.null(k)) {
    # prediction using a valid census key; tract level census statistics is used
    x = predict_race(voter.file = voters, census.geo = "tract", census.key = k, party = "PID")
    # test and comfirm the prediction output as expected
    expect_that(dim(x), is_equivalent_to(c(10,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(1))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0819))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0034))

    # prediction using a valid census key; place level census statistics is used
    x = predict_race(voter.file = voters, census.geo = "place", census.key = k, sex = T)
    # test and comfirm the prediction output as expected
    expect_that(dim(x), is_equivalent_to(c(10,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(1))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0566))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0197))
    
    # remove two NY cases from dataset to reduce the amount of the computation in the following test
    voters.dc.nj <- voters[c(-3, -7), ]  
    
    # prediction using a valid census key; block level census statistics is used
    x = predict_race(voter.file = voters.dc.nj, census.geo = "block", census.key = k, sex = T)
    # test and comfirm the prediction output as expected
    expect_that(dim(x), is_equivalent_to(c(8,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(5))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(0))
    expect_that(sum(is.na(x[x$surname != "Ratkovic",])), is_equivalent_to(0))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.2978))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0123))
  }
}) 
  
test_that("tests predictions using census from a defferent year", {
  # set random seed
  set.seed(12345)
  
  if (!is.null(k)) {
    # prediction using census statistics from the year 2000 which is different from the default (2010)
    x = predict_race(voter.file = voters, census.geo = "tract", census.key = k, party = "PID", surname.year = 2000)
    # test and comfirm the prediction output as expected
    expect_that(dim(x), is_equivalent_to(c(10,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(1))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0982))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0023))
  }
})

