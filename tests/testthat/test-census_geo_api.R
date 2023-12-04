skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")))

test_that("snapshot", {
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
