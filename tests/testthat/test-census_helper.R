# Note: must provide a valid U.S. Census API key for test cases that use U.S. Census statistics
# > usethis::edit_r_profile
# Sys.setenv("CENSUS_API_KEY" = "yourkey")
# For testing package coverage use: Sys.setenv("NOT_CRAN" = "TRUE")
options("piggyback.verbose" = FALSE)
options("wru_data_wd" = TRUE)

skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")))
test_that("census_helper old still returns predictions", {
  data(voters)
  census <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  x <- census_helper(
    voter.file = voters, 
    states = "NJ",
    year = "2020",
    census.data = census
    )
  expect_named(x, c('VoterID', 'surname', 'state', 'CD', 'county',
                    'tract', 'block', 'precinct', 'age', 'sex', 'party', 
                    'PID', 'place', 'last', 'first', 'r_whi', 'r_bla', 'r_his', 
                    'r_asi', 'r_oth'))
})
