if(Sys.getenv("CENSUS_API_KEY") != "") {
  test_that("Census block download works", {
    # takes a long time!
    future::plan(future::multisession)
    r <- suppressMessages(get_census_data(
      key = NULL, 
      state = c("DC"), 
      census.geo = "block", 
    ))
    
    expect_named(r$DC, c("state", "age", "sex", "year", "block", "tract", "county"))
    expect_true(all(r$DC$block$state == "DC"))
  })
  
  test_that("Census block_group download works", {
    r <- suppressMessages(get_census_data(
      key = NULL,
      state = "RI",
      census.geo = "block_group"
    ))
    expect_named(r$RI, c("state", "age", "sex", "year", "block_group", "tract", "county"))
    expect_true(all(r$RI$place$state == "RI"))
  })
  
  test_that("Census tract download works", {
    r <- suppressMessages(get_census_data(
      key = NULL,
      state = c("NY"),
      census.geo = "tract",
      county.list =  list(NY = "061")
    ))
    expect_named(r$NY, c("state", "age", "sex", "year", "tract", "county"))
    expect_true(all(r$NY$tract$state == "NY"))
    expect_true(all(r$NY$county$state == "NY"))
  })
  
  test_that("Census county download works", {
    r <- suppressMessages(get_census_data(
      key = NULL, 
      state = "NJ",
      census.geo = "county",
      county.list = list(NJ = "021")
    ))
    expect_named(r$NJ, c("state", "age", "sex", "year", "county"))
    expect_true(all(r$NJ$county$state == "NJ"))
  })
  
  test_that("Census place download works", {
    r <- suppressMessages(get_census_data(
      key = NULL,
      state = "RI",
      census.geo = "place"
    ))
    expect_named(r$RI, c("state", "age", "sex", "year", "place"))
    expect_true(all(r$RI$place$state == "RI"))
  })
  
  test_that("Census ZCTA download works", {
    r <- suppressMessages(get_census_data(
      key = NULL,
      state = "DC",
      census.geo = "zcta"
    ))
    expect_named(r$DC, c("state", "age", "sex", "year", "zcta"))
    expect_true(all(r$DC$zcta$state == "DC"))
  })
}
