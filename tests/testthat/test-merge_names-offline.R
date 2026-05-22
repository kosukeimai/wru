## Offline tests for merge_names() merge logic, with dictionary loading mocked.
## Guards the merge/clean/impute behavior without network downloads.

fixture_last <- function() {
  data.frame(
    last_name  = c("SMITH", "GARCIA"),
    c_whi_last = c(0.60, 0.05),
    c_bla_last = c(0.10, 0.02),
    c_his_last = c(0.10, 0.88),
    c_asi_last = c(0.10, 0.03),
    c_oth_last = c(0.10, 0.02),
    stringsAsFactors = FALSE
  )
}

test_that("merge_names merges an injected last-name dictionary without network", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last())
    },
    .package = "wru"
  )

  vf <- data.frame(surname = c("Smith", "Garcia"), stringsAsFactors = FALSE)
  out <- merge_names(vf, namesToUse = "surname", name_source = "mixed",
                     impute.missing = FALSE)

  expect_equal(out$c_whi_last[out$surname == "Smith"], 0.60)
  expect_equal(out$c_his_last[out$surname == "Garcia"], 0.88)
})

test_that("merge_names does not sweep user columns containing 'c_' into imputation (#156)", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last())
    },
    .package = "wru"
  )
  ## "abc_def" contains the substring "c_"; an unanchored grep("c_") would treat
  ## it as a dictionary probability column and coalesce its NA away.
  vf <- data.frame(
    surname = c("Smith", "Zzqqunmatched"),
    abc_def = c(10, NA_real_),
    stringsAsFactors = FALSE
  )
  out <- merge_names(vf, namesToUse = "surname", name_source = "mixed",
                     impute.missing = FALSE)

  expect_true(is.na(out$abc_def[out$surname == "Zzqqunmatched"]))
  expect_equal(out$abc_def[out$surname == "Smith"], 10)
})

test_that("impute.missing = FALSE leaves unmatched names as NA, not populated (#162)", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last())
    },
    .package = "wru"
  )
  vf <- data.frame(surname = c("Smith", "Zzqqunmatched"), stringsAsFactors = FALSE)
  out <- merge_names(vf, namesToUse = "surname", name_source = "mixed",
                     impute.missing = FALSE)

  ## Unmatched name keeps NA (not coalesced to 1 or a column mean).
  expect_true(is.na(out$c_whi_last[out$surname == "Zzqqunmatched"]))
  ## Matched name is unaffected.
  expect_equal(out$c_whi_last[out$surname == "Smith"], 0.60)
})
