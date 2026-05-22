## The wru_progress option toggles the furrr progress bar in census downloads
## (#150) without silencing the per-county/tract messages.

test_that(".census_progress() defaults to TRUE", {
  old <- options(wru_progress = NULL)
  on.exit(options(old))
  expect_true(.census_progress())
})

test_that(".census_progress() is FALSE when wru_progress = FALSE", {
  old <- options(wru_progress = FALSE)
  on.exit(options(old))
  expect_false(.census_progress())
})

test_that(".census_progress() is TRUE when wru_progress = TRUE", {
  old <- options(wru_progress = TRUE)
  on.exit(options(old))
  expect_true(.census_progress())
})

test_that(".census_progress() tolerates a non-logical option value", {
  old <- options(wru_progress = "yes")
  on.exit(options(old))
  expect_false(.census_progress())
})
