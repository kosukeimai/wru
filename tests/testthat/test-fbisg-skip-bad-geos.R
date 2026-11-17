## #163: model = "fBISG" should respect skip_bad_geos. Previously the fBISG path
## hard-stopped on any voter geography missing from census.data, ignoring the
## flag (and the BISG-derived race.init could misalign with voter.file).

test_that("fBISG drops unmatched geographies when skip_bad_geos = TRUE (#163)", {
  skip_on_cran()
  skip_if_not(file.exists(test_path("data/census_test_nj_block_2020.rds")))
  census <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  data(voters)
  nj <- voters[voters$state == "NJ", ]
  nj$tract[1] <- "999999"   # a tract that is not in census.data

  set.seed(42)
  out <- suppressMessages(predict_race(
    voter.file = nj, census.geo = "tract", census.data = census,
    model = "fBISG", skip_bad_geos = TRUE,
    control = list(iter = 20, burnin = 10, verbose = FALSE)
  ))

  ## The bad-geo row is dropped; the rest are predicted without error.
  expect_equal(nrow(out), nrow(nj) - 1L)
  expect_false("999999" %in% out$tract)
  expect_true(all(!is.na(out$pred.whi)))
})

test_that("fBISG still errors on unmatched geographies when skip_bad_geos = FALSE (#163)", {
  skip_on_cran()
  skip_if_not(file.exists(test_path("data/census_test_nj_block_2020.rds")))
  census <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  data(voters)
  nj <- voters[voters$state == "NJ", ]
  nj$tract[1] <- "999999"

  set.seed(42)
  expect_error(
    suppressMessages(predict_race(
      voter.file = nj, census.geo = "tract", census.data = census,
      model = "fBISG", skip_bad_geos = FALSE,
      control = list(iter = 20, burnin = 10, verbose = FALSE)
    ))
  )
})
