## predict_race_me() (model = "fBISG") previously called load_name_dictionaries()
## with namesToUse = "surname" only, and then read "wru-data-first_c.rds"
## directly from disk when first names were requested. As a result:
## 1) fBISG ignored the 2020 Census first-name dictionary (census_first_c) and
##    `name_source` for first names;
## 2) `name_source = "census_only"` with middle names was not rejected by
##    predict_race_me() when race.init was supplied.

test_that("fBISG uses load_name_dictionaries() for first names and respects name_source", {
  census_last <- data.frame(
    last_name  = "SMITH",
    c_whi_last = 0.60, c_bla_last = 0.10, c_his_last = 0.10,
    c_asi_last = 0.10, c_oth_last = 0.10,
    stringsAsFactors = FALSE
  )
  ## Census first-name dictionary says "ALEX" is 90% Asian; voter-file says 90% White
  census_first <- data.frame(
    first_name  = "ALEX",
    c_whi_first = 0.025, c_bla_first = 0.025, c_his_first = 0.025,
    c_asi_first = 0.900, c_oth_first = 0.025,
    stringsAsFactors = FALSE
  )
  vf_first <- data.frame(
    first_name  = "ALEX",
    c_whi_first = 0.900, c_bla_first = 0.025, c_his_first = 0.025,
    c_asi_first = 0.025, c_oth_first = 0.025,
    stringsAsFactors = FALSE
  )

  census <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  dir <- file.path(tempdir(), "wru-fbisg-first-test")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(vf_first, file.path(dir, "wru-data-first_c.rds"))
  old_wd  <- setwd(dir)
  old_opt <- options(wru_data_wd = TRUE)
  on.exit({
    setwd(old_wd)
    options(old_opt)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)

  captured_pi_f <- NULL
  testthat::local_mocked_bindings(
    wru_data_preflight = function(...) invisible(dir),
    read_name_dictionaries = function(year = "2020", namesToUse = "surname, first, middle") {
      list(
        census_last  = census_last,
        last         = census_last,
        first        = vf_first,
        mid          = NULL,
        census_first = census_first
      )
    },
    sample_me = function(last_name, first_name, mid_name, geo, N_rg,
                         pi_s, pi_f, pi_m, pi_nr, which_names,
                         samples, burnin, race_init, verbose) {
      captured_pi_f <<- pi_f
      matrix(2L, nrow = length(last_name), ncol = 5L)
    },
    .package = "wru"
  )
  vf <- data.frame(
    surname = c("Smith", "Smith"),
    first   = c("Alex", "Alex"),
    state   = "NJ",
    county  = "021",
    tract   = "004000",
    stringsAsFactors = FALSE
  )

  suppressMessages(predict_race(
    voter.file   = vf,
    names.to.use = "surname, first",
    name_source  = "census_only",
    census.geo   = "tract",
    census.data  = census,
    model        = "fBISG",
    race.init    = c(1L, 1L),
    control      = list(iter = 10, burnin = 5, verbose = FALSE)
  ))

  expect_false(is.null(captured_pi_f))
  ## Row 4 corresponds to c_asi_first, which is 0.90 in census_first (vs 0.025 in vf_first)
  expect_equal(as.numeric(captured_pi_f[4, 1]), 0.90)
})
