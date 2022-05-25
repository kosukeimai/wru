# Note: must provide a valid U.S. Census API key for test cases that use U.S. Census statistics
# > usethis::edit_r_profile
# Sys.setenv("CENSUS_API_KEY" = "yourkey")

if (Sys.getenv("CENSUS_API_KEY") == "") {
  message("Census key not available")
}

future::plan(future::multisession)
# Load data
data(voters)

test_that("Tests surname only predictions", {
  set.seed(12345)

  # Prediction using surname only
  x <- suppressMessages(predict_race(voter.file = voters, surname.only = TRUE, census.surname = TRUE))
  # Test and confirm prediction output is as expected
  expect_equal(dim(x), c(10, 20))
  expect_equal(sum(is.na(x)), 0)
  expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.068, tolerance = 0.01)
  expect_equal(round(x[x$surname == "Johnson", "pred.his"], 4), 0.0236, tolerance = 0.01)
})

test_that("Test BISG NJ and DC at tract level, manually build census object", {
  set.seed(12345)

  if (Sys.getenv("CENSUS_API_KEY") != "") {
    # Remove two NY cases from dataset to reduce the amount of the computation in the following test
    voters.dc.nj <- voters[c(-3, -7), ]

    # Create Census data object covering DC and NJ
    census.dc <- suppressMessages(
      get_census_data(key = NULL, state = c("DC"), census.geo = "tract", counties = "001")
    )

    census.nj <- suppressMessages(
      get_census_data(key = NULL, state = c("NJ"), census.geo = "tract", counties = "021")
    )

    census <- list()
    census$NJ <- census.nj$NJ
    census$DC <- census.dc$DC
    
    message(length(census))

    # Prediction using the Census object created in the previous step; tract-level statistics used in prediction
    x <- suppressMessages(predict_race(
      voter.file = voters.dc.nj,
      census.geo = "county",
      census.data = census
    ))
    # test and comfirm the prediction output as expected
    # test order
    expect_equal(as.character(x$VoterID), as.character(c(1, 2, 4, 5, 6, 8, 9, 10)))
    expect_equal(dim(x), c(8, 20))
    expect_equal(sum(is.na(x)), 0L)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.3142, tolerance = 0.01) # 0.0644
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.005, tolerance = 0.01) # 0.005
  }
})

test_that("Test fBISG NJ and DC at tract level, manually build census object", {
  set.seed(12345)
  
  if (Sys.getenv("CENSUS_API_KEY") != "") {
    # Remove two NY cases from dataset to reduce the amount of the computation in the following test
    voters.dc.nj <- voters[c(-3, -7), ]
    
    # Create Census data object covering DC and NJ
    census.dc <- suppressMessages(
      get_census_data(key = NULL, state = c("DC"), census.geo = "tract", counties = "001")
    )
    
    census.nj <- suppressMessages(
      get_census_data(key = NULL, state = c("NJ"), census.geo = "tract", counties = "021")
    )
    
    census <- list()
    census$NJ <- census.nj$NJ
    census$DC <- census.dc$DC
    
    # Prediction using the Census object created in the previous step; tract-level statistics used in prediction
    x <- suppressMessages(predict_race(
      voter.file = voters.dc.nj,
      census.geo = "county",
      census.data = census,
      model = "fBISG"
    ))
    # test and comfirm the prediction output as expected
    # test order
    expect_equal(as.character(x$VoterID), as.character(c(1, 2, 4, 5, 6, 8, 9, 10)))
    expect_equal(dim(x), c(8, 20))
    expect_equal(sum(is.na(x)), 0L)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.324, tolerance = 0.01) # 0.0644
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.005, tolerance = 0.01) # 0.005
  }
})

test_that("Tests predictions at tract and county level using the manually generated census object", {
  # set random seed
  set.seed(12345)

  if (Sys.getenv("CENSUS_API_KEY") != "") {

    # Build a Census object by parts; both county-level and tract-level statistics needed for tract-level predictions
    censusObj2 <- list()
    county.dc <- suppressMessages(
      census_geo_api(key = NULL, state = "DC", geo = "county", save_temp = NULL, counties = "001")
    )
    tract.dc <- suppressMessages(
      census_geo_api(key = NULL, state = "DC", geo = "tract", save_temp = NULL, counties = "001")
    )
    censusObj2[["DC"]] <- list(state = "DC", age = FALSE, sex = FALSE, county = county.dc, tract = tract.dc, year = "2010")
    tract.nj <- suppressMessages(
      census_geo_api(key = NULL, state = "NJ", geo = "tract", save_temp = NULL, counties = "021")
    )
    county.nj <- suppressMessages(
      census_geo_api(key = NULL, state = "NJ", geo = "county", save_temp = NULL, counties = "021")
    )
    censusObj2[["NJ"]] <- list(state = "NJ", age = FALSE, sex = FALSE, county = county.nj, tract = tract.nj, year = "2010")

    # Prediction using the Census object built in the previous step; county-level statistics used in prediction
    x <- suppressMessages(
      predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = censusObj2)
    )
    
    x <- predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, census.surname=T)
    # test and comfirm the prediction output as expected
    expect_equal(dim(x), c(8, 20))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.3142)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0055)

    # Prediction using the Census object built in the previous step; tract-level statistics used in prediction
    x <- suppressMessages(
      predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, party = "PID")) # Pr(Race | Surname, Tract, Party)
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(8, 20))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.4532) # TODO: Too high for an obviously Asian surname
    expect_equal(round(x[x$surname == "Khanna", "pred.asi"], 4), 0.5193) # TODO: Too low!
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0023)
    
    x <- suppressMessages(
      predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, census.surname = FALSE))
  }
})

test_that("Predict at block level", {
  if(Sys.getenv("CENSUS_API_KEY" != "")) {
    set.seed(42)
    vf <- voters[voters$state %in% c("NJ", "DC"),]
    x <- suppressMessages(predict_race(voter.file = voters, census.geo = "block", census.key = NULL, use_counties = TRUE))
    expect_equal(dim(x, c(10, 20)))
    expect_equal(is.na(x), 0)
    expect_true(!any(duplicated(x$surname)))
    expect_equal(round(x[x$surname == "Khanna", "pred.asi"]), 0.52)
    expect_equal(round(x[x$surname == "Zhou", "pred.asi"]), 0.52)
    expect_equal(round(x[x$surname == "Lopez", "pred.his"]), 0.52)
  }
})



test_that("Tests predictions using Census API key", {
  set.seed(12345)

  if (Sys.getenv("CENSUS_API_KEY") != "") {
    # Prediction using a valid Census API key; tract-level statistics used
    x <- suppressMessages(predict_race(voter.file = voters, census.geo = "tract", census.key = NULL))
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(10, 20))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 1)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0819)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0034)

    # Prediction using a valid Census API key; place-level statistics used
    x <- suppressMessages(predict_race(voter.file = voters, census.geo = "place", census.key = NULL, sex = T))
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(10, 20))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 1)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0566)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0197)

    # Remove two NY cases from dataset to reduce the amount of the computation in the following test
    voters.dc.nj <- voters[c(-3, -7), ]

    # Prediction using a valid Census API key; block-level statistics used
    x <- suppressMessages(predict_race(voter.file = voters.dc.nj, census.geo = "block", census.key = NULL, sex = T))
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(8, 20))
    expect_equal(sum(is.na(x)), 5)
    expect_equal(sum(x$surname == "Johnson"), 0)
    expect_equal(sum(is.na(x[x$surname != "Ratkovic", ])), 0)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.2978)
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0123)
  }
})

test_that("Tests predictions using Census data from a different year", {
  set.seed(12345)

  if (Sys.getenv("CENSUS_API_KEY") != "") {
    # Prediction using Census statistics from the year 2000, which is different from the default year (2010)
    x <- suppressMessages(predict_race(voter.file = voters, census.geo = "tract", census.key = NULL, surname.year = 2000, use_counties = TRUE))
    # Test and confirm prediction output is as expected
    expect_equal(dim(x), c(10, 20))
    expect_equal(sum(is.na(x)), 0)
    expect_equal(sum(x$surname == "Johnson"), 1)
    expect_equal(round(x[x$surname == "Khanna", "pred.whi"], 4), 0.0982) # 0.104
    expect_equal(round(x[x$surname == "Morse", "pred.his"], 4), 0.0023)
  }
})
