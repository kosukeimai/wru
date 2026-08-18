## surname.only predictions should not require the first/middle name
## dictionaries to be present (#160).

test_that("surname.only does not require first/middle name data (#160)", {
  skip_on_cran()

  ## Isolated data dir holding ONLY the surname dictionaries.
  src_last    <- test_path("wru-data-last_c.rds")
  src_clast   <- test_path("wru-data-census_last_c.rds")
  skip_if_not(file.exists(src_last) && file.exists(src_clast),
              "cached surname dictionaries not available")

  dir <- file.path(tempdir(), "wru160")
  dir.create(dir, showWarnings = FALSE)
  file.copy(src_last,  file.path(dir, "wru-data-last_c.rds"),        overwrite = TRUE)
  file.copy(src_clast, file.path(dir, "wru-data-census_last_c.rds"), overwrite = TRUE)
  ## Make sure first/middle data is genuinely absent.
  unlink(file.path(dir, c("wru-data-first_c.rds", "wru-data-mid_c.rds")))

  old_wd <- setwd(dir)
  old_opt <- options(wru_data_wd = TRUE)
  on.exit({ setwd(old_wd); options(old_opt) }, add = TRUE)

  ## Don't let preflight try to (re)download the missing files.
  testthat::local_mocked_bindings(
    wru_data_preflight = function(...) invisible(NULL),
    .package = "wru"
  )

  data(voters)
  expect_no_error(
    suppressMessages(predict_race(voter.file = voters, surname.only = TRUE))
  )
})
