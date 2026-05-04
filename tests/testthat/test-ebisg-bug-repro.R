## Regression tests for eBISG handling of unmatched surnames.
##
## These guard against re-introducing the bug where merge_names'
## cleaning cascade overwrites lastname.match for non-hyphenated
## unmatched names (it leaves the literal string "NA" in that column),
## which previously caused predict_race_embedding to embed the token
## "NA" instead of the actual surname text — collapsing every distinct
## non-hyphenated unmatched surname to the same garbage prediction.
##
## We mock the embedding/MLP calls so the tests exercise the real
## predict_race_embedding code path without needing Python deps or a
## downloaded MLP checkpoint.

with_mocked_ebisg <- function(code) {
  embed_calls <- character(0)
  testthat::local_mocked_bindings(
    ensure_ebisg_python  = function(...) invisible(),
    ebisg_data_preflight = function(...) tempdir(),
    ebisg_embed_names    = function(names, ...) {
      embed_calls <<- c(embed_calls, names)
      ## Return a deterministic per-name vector so distinct inputs
      ## produce distinct (downstream) outputs.
      matrix(seq_along(names), nrow = length(names), ncol = 4L)
    },
    ebisg_predict_mlp = function(embeddings, ...) {
      n <- nrow(embeddings)
      ## Distinct softmax distributions per input row, normalized.
      raw <- matrix(seq_len(n * 6L), nrow = n, ncol = 6L)
      raw / rowSums(raw)
    },
    .package = "wru"
  )
  result <- code()
  list(result = result, embed_calls = embed_calls)
}

test_that("eBISG embeds the original surname text, not merge_names' scratchpad", {
  vf <- data.frame(
    surname = c("SMITH", "ZQXWVPLM", "QQXWPLZZ"),
    state   = "NJ",
    stringsAsFactors = FALSE
  )
  out <- with_mocked_ebisg(function() {
    predict_race(voter.file = vf, surname.only = TRUE, model = "eBISG")
  })

  ## The fix: original surnames flow into the embedding step. The bug
  ## would have sent the literal token "NA" instead.
  expect_true(all(c("ZQXWVPLM", "QQXWPLZZ") %in% out$embed_calls))
  expect_false("NA" %in% out$embed_calls)
})

test_that("eBISG keeps unique unmatched surnames distinct in the embedding step", {
  ## Pre-fix, two different non-hyphenated unmatched surnames both
  ## collapsed to lastname.match = "NA" and so produced one embedding
  ## input shared across all such rows.
  vf <- data.frame(
    surname = c("ZQXWVPLM", "QQXWPLZZ"),
    state   = "NJ",
    stringsAsFactors = FALSE
  )
  out <- with_mocked_ebisg(function() {
    predict_race(voter.file = vf, surname.only = TRUE, model = "eBISG")
  })
  expect_setequal(out$embed_calls, c("ZQXWVPLM", "QQXWPLZZ"))
})

test_that("eBISG produces finite, normalized predictions for unmatched non-hyphenated surnames", {
  vf <- data.frame(
    surname = c("SMITH", "FAKE-NAME", "ZQXWVPLM"),
    state   = "NJ",
    stringsAsFactors = FALSE
  )
  out <- with_mocked_ebisg(function() {
    predict_race(voter.file = vf, surname.only = TRUE, model = "eBISG")
  })
  pred_cols <- grep("^pred\\.", names(out$result), value = TRUE)
  expect_equal(length(pred_cols), 5L)
  expect_false(any(is.na(out$result[, pred_cols])))
  expect_false(any(is.nan(as.matrix(out$result[, pred_cols]))))
  expect_equal(
    unname(rowSums(out$result[, pred_cols])),
    rep(1, nrow(vf)),
    tolerance = 1e-6
  )
})

test_that("eBISG skips rows with NA / empty surname rather than overriding with NA", {
  vf <- data.frame(
    surname = c("SMITH", NA_character_, ""),
    state   = "NJ",
    stringsAsFactors = FALSE
  )
  ## merge_names rejects NA surnames at validation, so we restrict to the
  ## empty-string case here, which it accepts.
  vf <- vf[vf$surname %in% c("SMITH", "") | is.na(vf$surname), ]
  vf <- vf[!is.na(vf$surname), ]
  out <- with_mocked_ebisg(function() {
    predict_race(voter.file = vf, surname.only = TRUE, model = "eBISG")
  })
  expect_false("" %in% out$embed_calls)
  pred_cols <- grep("^pred\\.", names(out$result), value = TRUE)
  expect_false(any(is.na(out$result[, pred_cols])))
})
