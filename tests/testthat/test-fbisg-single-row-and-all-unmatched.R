## Regression tests for predict_race_me() (model = "fBISG"):
## 1) Single-row voter.file: `race_samples[order(race_samples[, 1]), -1]`
##    previously dropped the matrix dimensions on a 1-row matrix, causing
##    `proportions(race_samples, 1)` to fail with
##    `Error in array(STATS, dims[perm]) : 'dim' cannot be of length 0`.
## 2) When no names in voter.file match the dictionary (`sum(kw_in_vf) == 0`),
##    `colMeans(pi_, na.rm = TRUE)` is `NaN`, causing `random_discrete(probs)`
##    in `sample_me.cpp` to return `J` (5) and crash with
##    `Error: Mat::operator(): index out of bounds`.

test_that("fBISG succeeds on a single-row voter.file", {
  fixture_last <- data.frame(
    last_name  = "SMITH",
    c_whi_last = 0.60, c_bla_last = 0.10, c_his_last = 0.10,
    c_asi_last = 0.10, c_oth_last = 0.10,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    wru_data_preflight = function(...) invisible(tempdir()),
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last)
    },
    .package = "wru"
  )

  census <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  vf <- data.frame(
    surname = "Smith",
    state   = "NJ",
    county  = "021",
    tract   = "004000",
    stringsAsFactors = FALSE
  )

  out <- suppressMessages(predict_race(
    voter.file  = vf,
    census.geo  = "tract",
    census.data = census,
    model       = "fBISG",
    race.init   = 1L,
    control     = list(iter = 10, burnin = 5, verbose = FALSE, seed = 42)
  ))

  pred_cols <- paste0("pred.", c("whi", "bla", "his", "asi", "oth"))
  expect_equal(nrow(out), 1L)
  expect_false(anyNA(out[, pred_cols]))
  expect_equal(unname(rowSums(out[, pred_cols])), 1, tolerance = 1e-6)
})

test_that("fBISG does not crash with index-out-of-bounds when no names match dictionary", {
  fixture_last <- data.frame(
    last_name  = "SMITH",
    c_whi_last = 0.60, c_bla_last = 0.10, c_his_last = 0.10,
    c_asi_last = 0.10, c_oth_last = 0.10,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    wru_data_preflight = function(...) invisible(tempdir()),
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last)
    },
    .package = "wru"
  )

  census <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  vf <- data.frame(
    surname = c("Zzqqunmatched1", "Zzqqunmatched2"),
    state   = "NJ",
    county  = "021",
    tract   = "004000",
    stringsAsFactors = FALSE
  )

  out <- suppressMessages(predict_race(
    voter.file  = vf,
    census.geo  = "tract",
    census.data = census,
    model       = "fBISG",
    race.init   = c(1L, 2L),
    control     = list(iter = 10, burnin = 5, verbose = FALSE, seed = 42)
  ))

  pred_cols <- paste0("pred.", c("whi", "bla", "his", "asi", "oth"))
  expect_equal(nrow(out), 2L)
  expect_false(anyNA(out[, pred_cols]))
  expect_equal(unname(rowSums(out[, pred_cols])), c(1, 1), tolerance = 1e-6)
})
