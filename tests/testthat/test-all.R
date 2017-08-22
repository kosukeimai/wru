rm(list=ls())
library(wru)
context("tests wru")


# need a valid census key
k <- NULL


# load the data
data(voters)

test_that("tests surname only predictions", {
  # set random seed
  set.seed(12345)

  x <- predict_race(voter.file = voters, surname.only = T)
  expect_that(dim(x), is_equivalent_to(c(10,18)))
  expect_that(sum(is.na(x)), is_equivalent_to(0))
  expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0676))
  expect_that(round(x[x$surname == "Johnson", "pred.his"], 4), is_equivalent_to(0.0236))
})

test_that("tests predictions using the census object", {
  # set random seed
  set.seed(12345)
  
  if (!is.null(k)) {
    voters.dc.nj <- voters[c(-3, -7), ]  # remove two NY cases from dataset
    census.dc.nj <- get_census_data(key = k, state = c("DC", "NJ"), census.geo = "tract", age = TRUE, sex = FALSE)  # create Census data object covering DC and NJ 
    x = predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = census.dc.nj, age = TRUE, sex = FALSE, party = "PID")
    expect_that(dim(x), is_equivalent_to(c(8,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(0))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0644))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0042))
    
    # Or build the census object by parts
    censusObj2  <- list()
    county.dc <- census_geo_api(key = k, state = "DC", geo = "county", age = TRUE, sex = FALSE)
    tract.dc <- census_geo_api(key = k, state = "DC", geo = "tract", age = TRUE, sex = FALSE)
    censusObj2[["DC"]] <- list(state = "DC", county = county.dc, tract = tract.dc, age = TRUE, sex = FALSE)
    tract.nj <- census_geo_api(key = k, state = "NJ", geo = "tract", age = TRUE, sex = FALSE)
    county.nj <- census_geo_api(key = k, state = "NJ", geo = "county", age = TRUE, sex = FALSE)
    censusObj2[["NJ"]] <- list(state = "NJ", county = county.nj, tract = tract.nj, age = TRUE, sex = FALSE)
    
    x = predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = censusObj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, County)
    expect_that(dim(x), is_equivalent_to(c(8,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(0))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0441))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0163))
    
    x = predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract, Party)
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
    x = predict_race(voter.file = voters, census.geo = "tract", census.key = k, party = "PID")
    expect_that(dim(x), is_equivalent_to(c(10,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(1))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0819))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0034))
    
    x = predict_race(voter.file = voters, census.geo = "place", census.key = k, sex = T)
    expect_that(dim(x), is_equivalent_to(c(10,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(1))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0566))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0197))
    
    voters.dc.nj <- voters[c(-3, -7), ]  # remove two NY cases from dataset
    x = predict_race(voter.file = voters.dc.nj, census.geo = "block", census.key = k, sex = T)
    expect_that(dim(x), is_equivalent_to(c(8,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(5))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(0))
    expect_that(sum(is.na(x[x$surname != "Ratkovic",])), is_equivalent_to(0))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.2978))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0123))
  }
})

