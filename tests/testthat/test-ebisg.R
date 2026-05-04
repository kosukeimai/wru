test_that("predict_race rejects unknown model strings", {
  data(voters)
  # Confirms the validation block in predict_race() actually fires; the
  # eBISG-accepting path is exercised by the end-to-end test below (when
  # Python is available).
  expect_error(
    predict_race(voter.file = voters, surname.only = TRUE, model = "not_a_model"),
    "model"
  )
})

test_that("map_6class_to_5class produces correct output", {
  probs_6 <- matrix(c(0.5, 0.1, 0.2, 0.1, 0.05, 0.05), nrow = 1,
                    dimnames = list(NULL, c("whi", "bla", "his", "asi", "aian", "oth")))
  result <- map_6class_to_5class(probs_6)
  expect_equal(ncol(result), 5)
  expect_equal(as.numeric(rowSums(result)), 1, tolerance = 1e-6)
  expect_equal(as.numeric(result[1, "c_whi"]), 0.5)
  expect_equal(as.numeric(result[1, "c_oth"]), 0.10)  # aian + oth = 0.05 + 0.05
})

test_that("map_6class_to_5class handles multiple rows", {
  probs_6 <- matrix(1 / 6, nrow = 5, ncol = 6,
                    dimnames = list(NULL, c("whi", "bla", "his", "asi", "aian", "oth")))
  result <- map_6class_to_5class(probs_6)
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 5)
  expect_equal(as.numeric(rowSums(result)), rep(1, 5), tolerance = 1e-6)
})

test_that("ensure_ebisg_python gives clear error without reticulate", {
  skip_on_cran()
  # If reticulate is not installed, we expect a clear error message
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    expect_error(ensure_ebisg_python(), "reticulate")
  }
})

test_that("eBISG predictions work end-to-end", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("sentence_transformers"),
              "sentence-transformers not available")
  skip_if_not(reticulate::py_module_available("torch"),
              "torch not available")

  data(voters)
  result <- predict_race(voter.file = voters, surname.only = TRUE, model = "eBISG")

  pred_cols <- grep("^pred\\.", names(result), value = TRUE)
  expect_equal(length(pred_cols), 5)

  # Probabilities should be valid
  for (col in pred_cols) {
    expect_true(all(result[[col]] >= 0 & result[[col]] <= 1))
  }

  # Row sums should equal 1
  row_sums <- rowSums(result[, pred_cols])
  expect_equal(row_sums, rep(1, nrow(result)), tolerance = 1e-6)
})
