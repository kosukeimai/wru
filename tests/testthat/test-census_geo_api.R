skip_if_not(nzchar(Sys.getenv("CENSUS_API_KEY")))

test_that("snapshot", {
  # TODO: Test that sub-geographies sum to match pooled geographies (e.g. blocks sum to block groups, sum to tracts, sum to counties)
  
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

expect_subset_sums_equal_overall_total <- function(data) {
  `%>%` <- dplyr::`%>%`
  
  sums <- data %>%
    dplyr::select(-dplyr::starts_with("r_")) %>%
    tidyr::pivot_longer(dplyr::starts_with("P")) %>%
    dplyr::mutate(
      subset = dplyr::case_when(
        grepl("001", name) ~ "overall",
        .default = "subset"
      ),
      name = sub("_.+", "", name)
    ) %>%
    dplyr::summarise(
      value = sum(value),
      .by = -value
    ) %>%
    dplyr::summarize(
      are_equal = length(unique(value)) <= 1,
      .by = c(-subset, -value)
    )
  
  expect_true(all(sums$are_equal))
}

test_that("sums", {
  expect_subset_sums_equal_overall_total(
    census_geo_api(state = "DE", geo = "county", year = "2020", sex = TRUE)
  )
  expect_subset_sums_equal_overall_total(
    census_geo_api(state = "DE", geo = "county", year = "2020", age = TRUE)
  )
  expect_subset_sums_equal_overall_total(
    census_geo_api(state = "DE", geo = "county", year = "2020", age = TRUE, sex = TRUE)
  )
  expect_subset_sums_equal_overall_total(
    census_geo_api(state = "DE", geo = "county", year = "2010", sex = TRUE)
  )
  expect_subset_sums_equal_overall_total(
    census_geo_api(state = "DE", geo = "county", year = "2010", age = TRUE)
  )
  expect_subset_sums_equal_overall_total(
    census_geo_api(state = "DE", geo = "county", year = "2010", age = TRUE, sex = TRUE)
  )
})

# TODO: Test that all variables sum to total population of geography
