## Tests for name_source dictionary selection / stacking (v4.0.0).
## Pure, offline: exercises stack_name_dictionary() with tiny fixtures.

make_dict <- function(names, key = "last_name", suffix = "last", whi = NULL) {
  n <- length(names)
  d <- data.frame(names, stringsAsFactors = FALSE)
  names(d) <- key
  eth <- c("whi", "bla", "his", "asi", "oth")
  for (i in seq_along(eth)) {
    col <- paste0("c_", eth[i], "_", suffix)
    d[[col]] <- if (i == 1 && !is.null(whi)) whi else rep(0.1, n)
  }
  d
}

test_that("mixed name_source unions Census over voter-file, Census winning on overlap", {
  census <- make_dict(c("AAA", "BBB"), whi = c(0.20, 0.20))
  vf     <- make_dict(c("BBB", "CCC"), whi = c(0.90, 0.40))

  out <- stack_name_dictionary(census = census, vf = vf,
                               name_source = "mixed", key = "last_name")

  ## Union of names, no duplicates
  expect_setequal(out$last_name, c("AAA", "BBB", "CCC"))
  ## Census wins for the overlapping name BBB
  expect_equal(out$c_whi_last[out$last_name == "BBB"], 0.20)
  ## Provenance tagged: AAA/BBB from census, CCC filled from voter-file
  expect_equal(out$source[out$last_name == "AAA"], "census")
  expect_equal(out$source[out$last_name == "BBB"], "census")
  expect_equal(out$source[out$last_name == "CCC"], "vf")
})

test_that("census_only returns only the Census dictionary, ignoring voter-file", {
  census <- make_dict(c("AAA", "BBB"), whi = c(0.20, 0.20))
  vf     <- make_dict(c("BBB", "CCC"), whi = c(0.90, 0.40))

  out <- stack_name_dictionary(census = census, vf = vf,
                               name_source = "census_only", key = "last_name")

  expect_setequal(out$last_name, c("AAA", "BBB"))
  expect_true(all(out$source == "census"))
  expect_equal(out$c_whi_last[out$last_name == "BBB"], 0.20)
})

test_that("vf_only returns only the voter-file dictionary, ignoring Census", {
  census <- make_dict(c("AAA", "BBB"), whi = c(0.20, 0.20))
  vf     <- make_dict(c("BBB", "CCC"), whi = c(0.90, 0.40))

  out <- stack_name_dictionary(census = census, vf = vf,
                               name_source = "vf_only", key = "last_name")

  expect_setequal(out$last_name, c("BBB", "CCC"))
  expect_true(all(out$source == "vf"))
  expect_equal(out$c_whi_last[out$last_name == "BBB"], 0.90)
})
