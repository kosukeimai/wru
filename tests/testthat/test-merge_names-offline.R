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
