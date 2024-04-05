# Note: must provide a valid U.S. Census API key for test cases that use U.S. Census statistics
# > usethis::edit_r_profile
# Sys.setenv("CENSUS_API_KEY" = "yourkey")
# For testing package coverage use: Sys.setenv("NOT_CRAN" = "TRUE")
options("piggyback.verbose" = FALSE)
options("wru_data_wd" = TRUE)

skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")))
test_that("Fails if 'precinct' is set as the geo var",{
  skip_on_cran()
  set.seed(42)
  data(voters)
  future::plan(future::multisession)
  census <- readRDS(test_path("data/new_census_table_NJ_2020.rds"))
  expect_error(
  census_helper_new(
    voter.file = voters,
    states = "all",
    geo = "precinct",
    age = FALSE,
    sex = FALSE,
    year = "2020",
    census.data = census,
    retry = 3,
    use.counties = FALSE,
    skip_bad_geos = FALSE
  ),
  "Error: census_helper_new function does not currently support precinct-level data.")
})

skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")))
test_that("helper returns verified census tract data",{
  skip_on_cran()
  set.seed(42)
  data(voters)
  future::plan(future::multisession)
  census <- readRDS(test_path("data/new_census_table_NJ_2020.rds"))
  x <- census_helper_new(
    voter.file = voters,
    states = "NJ",
    geo = "tract",
    age = FALSE,
    sex = FALSE,
    year = "2020",
    census.data = census,
    retry = 3,
    use.counties = FALSE,
    skip_bad_geos = FALSE
    )
  expect_equal(x[x$surname == "Lopez", "r_whi"], 0.767197, tolerance = 0.000001)
  expect_equal(x[x$surname == "Khanna", "r_whi"], 0.708026, tolerance = 0.000001)
  expect_equal(x[x$surname == "Lopez", "r_bla"], 0.09522743, tolerance = 0.000001)
  expect_equal(x[x$surname == "Khanna", "r_bla"], 0.09544469, tolerance = 0.000001)
})

skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")))
test_that("New tables and legacy tables return equal race predictions",{
  skip_on_cran()
  set.seed(42)
  data(voters)
  future::plan(future::multisession)
  # legacy redistricting table
  census <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  x <- census_helper_new(
    voter.file = voters,
    states = "NJ",
    geo = "tract",
    age = FALSE,
    sex = FALSE,
    year = "2020",
    census.data = census,
    use.counties = FALSE
  )
  # use new table source
  new_census <- readRDS(test_path("data/new_census_table_NJ_2020.rds"))
  y <- census_helper_new(
    voter.file = voters,
    states = "NJ",
    geo = "tract",
    age = FALSE,
    sex = FALSE,
    year = "2020",
    census.data = new_census,
    use.counties = FALSE
  )
  expect_equal(x$r_whi, y$r_whi, tolerance = .01)
  # expect_equal(x$r_bla, y$r_bla, tolerance = .01)
  expect_equal(x$r_his, y$r_his, tolerance = .01)
  expect_equal(x$r_asi, y$r_asi, tolerance = .01)
  # expect_equal(x$r_oth, y$r_oth, tolerance = .01)
})
