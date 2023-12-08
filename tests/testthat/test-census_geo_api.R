skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")))

test_that("snapshot", {
  # These snapshots were generated using the calculations in v2.0.0
  # and verified that the calculations resulted in the same numbers for PR #120.
  expect_snapshot_value(
    census_geo_api(state = "DE", geo = "county", year = "2020"),
    style = "deparse"
  )
  expect_snapshot_value(
    census_geo_api(state = "DE", geo = "county", year = "2010"),
    style = "deparse"
  )
  expect_snapshot_value(
    census_geo_api(state = "DE", geo = "county", year = "2010", sex = TRUE),
    style = "deparse"
  )
  expect_snapshot_value(
    census_geo_api(state = "DE", geo = "county", year = "2010", age = TRUE),
    style = "deparse"
  )
})
