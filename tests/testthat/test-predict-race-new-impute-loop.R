## In predict_race_new(), the fallback imputation loop previously ran
## `for (i in ncol(preds))` instead of `for (i in seq_len(ncol(preds)))`,
## only coalescing column 5 (pred.oth) and leaving columns 1:4 as NA/NaN,
## which then caused `preds / rowSums(preds)` to evaluate to NA across all 5
## columns.

test_that("predict_race_new fallback imputation coalesces all 5 race columns, not just column 5", {
  fixture_last <- data.frame(
    last_name  = "SMITH",
    c_whi_last = 0.60,
    c_bla_last = 0.10,
    c_his_last = 0.10,
    c_asi_last = 0.10,
    c_oth_last = 0.10,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    wru_data_preflight = function(...) invisible(tempdir()),
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last)
    },
    .package = "wru"
  )

  ## A voter file where NO surnames match the dictionary: merge_names'
  ## column-mean imputation yields NaN across all 5 c_*_last columns, so
  ## predict_race_new relies on its `if (impute.missing)` fallback to
  ## `race.margin` for all 5 columns.
  vf <- data.frame(surname = "Zzqqunmatched", stringsAsFactors = FALSE)
  out <- suppressMessages(predict_race(
    voter.file = vf,
    surname.only = TRUE,
    impute.missing = TRUE
  ))

  pred_cols <- paste0("pred.", c("whi", "bla", "his", "asi", "oth"))
  expect_false(anyNA(out[, pred_cols]))
  expect_equal(unname(rowSums(out[, pred_cols])), 1, tolerance = 1e-6)
  expect_equal(
    as.numeric(out[1, pred_cols]),
    c(0.5783619, 0.1205021, 0.1872988, 0.06106737, 0.05276981),
    tolerance = 1e-6
  )
})
