## Regression test: the eBISG MLPs return Pr(Race | Name), but the name columns
## that enter the Bayesian combination hold Pr(Name | Race). The MLP output must
## be divided by the race mix of its training table before it is used, or the
## race marginal is counted twice for every embedded name.
##
## Check: an embedded name whose predicted Pr(Race | Name) equals the training
## mix carries no information, so its surname-only posterior must equal the
## race marginal (and its first name must leave the posterior unchanged).

margin_mocks <- function(mlp_out) {
  last_dict <- data.frame(
    last_name  = "SMITH",
    c_whi_last = 0.010, c_bla_last = 0.017, c_his_last = 0.001,
    c_asi_last = 0.001, c_oth_last = 0.012,
    stringsAsFactors = FALSE
  )
  first_dict <- data.frame(
    first_name  = "JOHN",
    c_whi_first = 0.020, c_bla_first = 0.015, c_his_first = 0.004,
    c_asi_first = 0.003, c_oth_first = 0.010,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    wru_data_preflight     = function(...) invisible(tempdir()),
    read_name_dictionaries = function(year = "2020", namesToUse = "surname, first, middle") {
      list(census_last = last_dict, last = last_dict, first = first_dict,
           mid = NULL, census_first = first_dict)
    },
    ensure_ebisg_python  = function(...) invisible(),
    ebisg_data_preflight = function(...) tempdir(),
    ebisg_embed_names    = function(names, ...) matrix(0, nrow = length(names), ncol = 4L),
    ebisg_predict_mlp    = function(embeddings, ...) {
      probs <- matrix(mlp_out, nrow = nrow(embeddings), ncol = 6L, byrow = TRUE)
      colnames(probs) <- c("whi", "bla", "his", "asi", "aian", "oth")
      probs
    },
    .package = "wru",
    .env = parent.frame()
  )
}

race_margin <- c(0.5783619, 0.1205021, 0.1872988, 0.06106737, 0.05276981)
## Race mix of the 2020 Census surname and first-name tables (whi, bla, his,
## asi, aian + oth), split here into the MLP's six classes.
to6 <- function(m) c(m[1:4], m[5] / 2, m[5] / 2)
sn_mix6 <- to6(c(0.60022790, 0.11765559, 0.18012701, 0.06208315, 0.03990636))
fn_mix6 <- to6(c(0.59839914, 0.11777467, 0.18207780, 0.06197576, 0.03977263))

test_that("an uninformative embedded surname returns the race marginal", {
  margin_mocks(sn_mix6)
  vf <- data.frame(surname = c("SMITH", "ZXQWVUT"), state = "NJ", stringsAsFactors = FALSE)
  out <- suppressWarnings(suppressMessages(predict_race(
    voter.file = vf, surname.only = TRUE, model = "eBISG"
  )))
  pred <- unlist(out[out$surname == "ZXQWVUT", grep("^pred\\.", names(out))])
  expect_equal(unname(pred), race_margin / sum(race_margin), tolerance = 1e-6)
})

test_that("an uninformative embedded first name leaves the surname-and-geography posterior unchanged", {
  margin_mocks(fn_mix6)
  ## predict_race_embedding() reads the first-name dictionary from disk to
  ## decide which first names are unmatched; give it one with JOHN.
  fpath <- file.path(tempdir(), "wru-data-first_c.rds")
  made_fpath <- !file.exists(fpath)
  if (made_fpath) {
    saveRDS(data.frame(first_name = "JOHN", c_whi_first = 0.020, c_bla_first = 0.015,
                       c_his_first = 0.004, c_asi_first = 0.003, c_oth_first = 0.010), fpath)
  }
  ## Pr(Race | Geography) for a single tract
  r_geo <- c(0.30, 0.50, 0.10, 0.05, 0.05)
  testthat::local_mocked_bindings(
    validate_key      = function(key, ...) "dummy",
    census_helper_new = function(voter.file, ...) {
      voter.file$r_whi <- r_geo[1]; voter.file$r_bla <- r_geo[2]; voter.file$r_his <- r_geo[3]
      voter.file$r_asi <- r_geo[4]; voter.file$r_oth <- r_geo[5]
      voter.file
    },
    .package = "wru"
  )
  vf <- data.frame(surname = "SMITH", first = c("JOHN", "ZXQWVUT"), state = "NJ",
                   county = "001", tract = "000100", stringsAsFactors = FALSE)
  ## Call the eBISG step directly: predict_race() downloads Census data first.
  out <- suppressWarnings(suppressMessages(predict_race_embedding(
    voter.file = vf, names.to.use = "surname, first", census.geo = "tract",
    name.dictionaries = NULL
  )))
  sn <- c(0.010, 0.017, 0.001, 0.001, 0.012)  # Pr(SMITH | Race) from the mocked dictionary
  expected <- sn * r_geo
  expect_equal(unname(unlist(out[out$first == "ZXQWVUT", grep("^pred\\.", names(out))])),
               expected / sum(expected), tolerance = 1e-6)
  if (made_fpath) unlink(fpath)
})
