options("piggyback.verbose" = FALSE)
options("wru_data_wd" = TRUE)

# read the file once for every test in this file
legacy_de <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- format_legacy_data(PL94171::pl_url("DE", 2020), state = "DE")
    }
    cached
  }
})

race_vars <- unlist(census_geo_api_names_legacy("2020"), use.names = FALSE)

test_that("legacy data returns an object predict_race can consume", {
  skip_on_cran()
  skip_if_offline()
  de <- legacy_de()

  # same shape as get_census_data(): keyed by state, then by geography
  expect_named(de, "DE")
  expect_named(
    de$DE,
    c("state", "age", "sex", "year", "county", "tract", "block_group", "block")
  )
  expect_equal(de$DE$year, "2020")
  expect_equal(de$DE$state, "DE")

  # 2020 redistricting column names, not the 2010 SF1 ones
  expect_named(de$DE$county, c("state", "county", race_vars), ignore.order = TRUE)
  expect_named(de$DE$tract, c("state", "county", "tract", race_vars), ignore.order = TRUE)
  expect_named(
    de$DE$block_group,
    c("state", "county", "tract", "block_group", race_vars),
    ignore.order = TRUE
  )
  expect_named(
    de$DE$block,
    c("state", "county", "tract", "block", race_vars),
    ignore.order = TRUE
  )

  expect_no_error(census_data_preflight(de, "block", "2020"))
})

test_that("predict_race runs on legacy data at block level (#175)", {
  skip_on_cran()
  skip_if_offline()
  de <- legacy_de()

  blocks <- head(de$DE$block[de$DE$block$P2_005N > 50, ], 5)
  voter.file <- data.frame(
    surname = c("Smith", "Nguyen", "Garcia", "Washington", "Kim"),
    state = "DE",
    county = blocks$county,
    tract = blocks$tract,
    block = blocks$block
  )

  out <- predict_race(
    voter.file = voter.file,
    census.geo = "block",
    census.data = de,
    year = "2020",
    names.to.use = "surname"
  )

  pred <- out[, grep("^pred[._]", names(out))]
  expect_equal(ncol(pred), 5)
  expect_false(anyNA(pred))
  expect_equal(unname(rowSums(pred)), rep(1, nrow(voter.file)), tolerance = 1e-6)
})

test_that("legacy data geo ids line up across geography levels", {
  skip_on_cran()
  skip_if_offline()
  de <- legacy_de()

  expect_true(all(nchar(de$DE$county$county) == 3))
  expect_true(all(nchar(de$DE$tract$tract) == 6))
  expect_true(all(nchar(de$DE$block_group$block_group) == 1))
  expect_true(all(nchar(de$DE$block$block) == 4))

  # a finer level must name a real coarser unit, or the merge in
  # census_helper_new() silently matches nothing
  expect_true(all(de$DE$block$county %in% de$DE$county$county))
  expect_true(all(de$DE$block$tract %in% de$DE$tract$tract))
  expect_true(all(de$DE$block_group$tract %in% de$DE$tract$tract))

  # the block group is the first digit of the block code
  bg <- paste(de$DE$block$county, de$DE$block$tract, substr(de$DE$block$block, 1, 1))
  expect_true(
    all(bg %in% paste(
      de$DE$block_group$county, de$DE$block_group$tract, de$DE$block_group$block_group
    ))
  )
})
