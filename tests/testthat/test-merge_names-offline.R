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

## The Census surname list contains the genuine surname "NA". merge_names
## coerces unmatched non-hyphenated names to the literal string "NA" during the
## cleaning cascade, so they spuriously merge against this entry. The match flag
## must not be fooled by that collision (#105).
fixture_last_with_na <- function() {
  data.frame(
    last_name  = c("SMITH", "NA"),
    c_whi_last = c(0.60, 0.0001),
    c_bla_last = c(0.10, 0.0001),
    c_his_last = c(0.10, 0.0001),
    c_asi_last = c(0.10, 0.0001),
    c_oth_last = c(0.10, 0.0001),
    stringsAsFactors = FALSE
  )
}

fixture_first <- function() {
  data.frame(
    first_name  = c("JOHN", "MARIA"),
    c_whi_first = c(0.70, 0.10),
    c_bla_first = c(0.10, 0.05),
    c_his_first = c(0.05, 0.75),
    c_asi_first = c(0.10, 0.05),
    c_oth_first = c(0.05, 0.05),
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

test_that("return.unmatched = FALSE (default) adds no *_matched columns", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last())
    },
    .package = "wru"
  )
  vf <- data.frame(surname = c("Smith", "Zzqqunmatched"), stringsAsFactors = FALSE)
  out <- merge_names(vf, namesToUse = "surname", name_source = "mixed",
                     impute.missing = FALSE)

  expect_false("last_matched" %in% names(out))
})

test_that("return.unmatched = TRUE flags matched vs unmatched last names (#105)", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last())
    },
    .package = "wru"
  )
  vf <- data.frame(surname = c("Smith", "Zzqqunmatched"), stringsAsFactors = FALSE)
  out <- merge_names(vf, namesToUse = "surname", name_source = "mixed",
                     impute.missing = FALSE, return.unmatched = TRUE)

  expect_true("last_matched" %in% names(out))
  expect_true(out$last_matched[out$surname == "Smith"])
  expect_false(out$last_matched[out$surname == "Zzqqunmatched"])
})

test_that("return.unmatched flag is computed before imputation (#105)", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last())
    },
    .package = "wru"
  )
  vf <- data.frame(surname = c("Smith", "Zzqqunmatched"), stringsAsFactors = FALSE)
  out <- merge_names(vf, namesToUse = "surname", name_source = "mixed",
                     impute.missing = TRUE, return.unmatched = TRUE)

  ## Imputation fills the probability, but the flag must still report unmatched.
  expect_false(is.na(out$c_whi_last[out$surname == "Zzqqunmatched"]))
  expect_false(out$last_matched[out$surname == "Zzqqunmatched"])
  expect_true(out$last_matched[out$surname == "Smith"])
})

test_that("return.unmatched is not fooled by the 'NA' surname collision (#105)", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last_with_na())
    },
    .package = "wru"
  )
  vf <- data.frame(
    surname = c("Smith", "Zzqqunmatched", "NA"),
    stringsAsFactors = FALSE
  )
  out <- merge_names(vf, namesToUse = "surname", name_source = "mixed",
                     impute.missing = FALSE, return.unmatched = TRUE)

  ## A real match.
  expect_true(out$last_matched[out$surname == "Smith"])
  ## Unmatched name that collided with the dictionary's "NA" surname: still FALSE.
  expect_false(out$last_matched[out$surname == "Zzqqunmatched"])
  ## The genuine surname "NA" really is a match.
  expect_true(out$last_matched[out$surname == "NA"])
})

test_that("unmatched names coerced to 'NA' do not inherit the 'NA' surname probabilities (#162)", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = NULL, middle = NULL, last = fixture_last_with_na())
    },
    .package = "wru"
  )
  vf <- data.frame(
    surname = c("Smith", "Zzqqunmatched", "NA"),
    stringsAsFactors = FALSE
  )
  out <- merge_names(vf, namesToUse = "surname", name_source = "mixed",
                     impute.missing = FALSE)

  ## The cascade coerces the unmatched name's key to the string "NA", which
  ## merges against the genuine "NA" surname. It must be blanked back to NA.
  expect_true(is.na(out$c_whi_last[out$surname == "Zzqqunmatched"]))
  ## A real match is untouched.
  expect_equal(out$c_whi_last[out$surname == "Smith"], 0.60)
  ## The genuine surname "NA" keeps its probability.
  expect_equal(out$c_whi_last[out$surname == "NA"], 0.0001)
})

test_that("return.unmatched flags last and first names independently (#105)", {
  testthat::local_mocked_bindings(
    load_name_dictionaries = function(...) {
      list(first = fixture_first(), middle = NULL, last = fixture_last())
    },
    .package = "wru"
  )
  vf <- data.frame(
    surname = c("Smith", "Garcia"),
    first   = c("John", "Zzqqunmatched"),
    stringsAsFactors = FALSE
  )
  out <- merge_names(vf, namesToUse = "surname, first", name_source = "mixed",
                     impute.missing = FALSE, return.unmatched = TRUE)

  expect_true(all(c("last_matched", "first_matched") %in% names(out)))
  expect_true(out$last_matched[out$surname == "Smith"])
  expect_true(out$first_matched[out$first == "John"])
  expect_false(out$first_matched[out$first == "Zzqqunmatched"])
})
