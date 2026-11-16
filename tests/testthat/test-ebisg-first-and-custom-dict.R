## Regression tests for eBISG first-name embedding and custom dictionary matching:
## 1) Unmatched non-hyphenated first names coerced to "NA" in firstname.match
##    must still be embedded (not treated as matching the genuine first name "NA").
## 2) First-name matching must respect load_name_dictionaries() / name_source /
##    custom name.dictionaries rather than hard-coding readRDS("wru-data-first_c.rds").
## 3) Custom name.dictionaries[["surname"]] / [["first"]] must not be overwritten
##    by embedding predictions for names that ARE present in the custom dictionary.

with_mocked_ebisg_dicts <- function(last_dict, first_dict, code) {
  embed_calls <- character(0)
  testthat::local_mocked_bindings(
    wru_data_preflight   = function(...) invisible(tempdir()),
    read_name_dictionaries = function(year = "2020", namesToUse = "surname, first, middle") {
      list(
        census_last  = last_dict,
        last         = last_dict,
        first        = first_dict,
        mid          = NULL,
        census_first = first_dict
      )
    },
    ensure_ebisg_python  = function(...) invisible(),
    ebisg_data_preflight = function(...) tempdir(),
    ebisg_embed_names    = function(names, ...) {
      embed_calls <<- c(embed_calls, names)
      matrix(seq_along(names), nrow = length(names), ncol = 4L)
    },
    ebisg_predict_mlp = function(embeddings, ...) {
      n <- nrow(embeddings)
      raw <- matrix(seq_len(n * 6L), nrow = n, ncol = 6L)
      probs <- raw / rowSums(raw)
      colnames(probs) <- c("whi", "bla", "his", "asi", "aian", "oth")
      probs
    },
    .package = "wru"
  )
  result <- code()
  list(result = result, embed_calls = embed_calls)
}

test_that("eBISG embeds unmatched non-hyphenated first names despite 'NA' dictionary collision", {
  last_dict <- data.frame(
    last_name  = c("SMITH", "NA"),
    c_whi_last = c(0.60, 0.01),
    c_bla_last = c(0.10, 0.01),
    c_his_last = c(0.10, 0.01),
    c_asi_last = c(0.10, 0.96),
    c_oth_last = c(0.10, 0.01),
    stringsAsFactors = FALSE
  )
  ## First-name dictionary includes the genuine first name "NA"
  first_dict <- data.frame(
    first_name  = c("JOHN", "NA"),
    c_whi_first = c(0.70, 0.01),
    c_bla_first = c(0.10, 0.01),
    c_his_first = c(0.05, 0.01),
    c_asi_first = c(0.10, 0.96),
    c_oth_first = c(0.05, 0.01),
    stringsAsFactors = FALSE
  )

  vf <- data.frame(
    surname = c("SMITH", "SMITH"),
    first   = c("JOHN", "ZXQWVUT"),
    state   = "NJ",
    stringsAsFactors = FALSE
  )

  out <- with_mocked_ebisg_dicts(last_dict, first_dict, function() {
    suppressWarnings(suppressMessages(predict_race(
      voter.file = vf,
      names.to.use = "surname, first",
      surname.only = TRUE,
      model = "eBISG"
    )))
  })

  expect_equal(out$embed_calls, "ZXQWVUT")
})

test_that("eBISG respects custom name.dictionaries when deciding which names are unmatched", {
  last_dict <- data.frame(
    last_name  = "SMITH",
    c_whi_last = 0.60,
    c_bla_last = 0.10,
    c_his_last = 0.10,
    c_asi_last = 0.10,
    c_oth_last = 0.10,
    stringsAsFactors = FALSE
  )
  custom_last <- data.frame(
    last_name  = c("SMITH", "CUSTOMNAME"),
    c_whi_last = c(0.60, 0.90),
    c_bla_last = c(0.10, 0.025),
    c_his_last = c(0.10, 0.025),
    c_asi_last = c(0.10, 0.025),
    c_oth_last = c(0.10, 0.025),
    stringsAsFactors = FALSE
  )

  vf <- data.frame(
    surname = c("SMITH", "CUSTOMNAME", "UNMATCHEDNAME"),
    state   = "NJ",
    stringsAsFactors = FALSE
  )

  out <- with_mocked_ebisg_dicts(last_dict, NULL, function() {
    suppressWarnings(suppressMessages(predict_race(
      voter.file = vf,
      surname.only = TRUE,
      name.dictionaries = list(surname = custom_last),
      model = "eBISG"
    )))
  })

  ## CUSTOMNAME is in custom_last, so only UNMATCHEDNAME should be embedded
  expect_equal(out$embed_calls, "UNMATCHEDNAME")
})
