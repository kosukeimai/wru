## wru_data_preflight() must fail loudly when the release data cannot be obtained.
## Previously a failed pb_download() only emitted a message and execution carried
## on into readRDS(), so the user saw `cannot open the connection` from gzfile()
## deep in the stack rather than a statement of what failed.

CORE <- c("wru-data-census_last_c.rds", "wru-data-last_c.rds")

## Run `code` with the package pointed at an isolated, disposable data directory.
## `code` is called with that directory as its only argument.
with_data_dir <- function(name, files = character(), code) {
  dir <- file.path(tempdir(), name)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (f in files) saveRDS(data.frame(), file.path(dir, f))

  old_wd  <- setwd(dir)
  old_opt <- options(wru_data_wd = TRUE)
  on.exit({
    setwd(old_wd)
    options(old_opt)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)

  code(dir)
}

test_that("a failed download with no data on disk raises a clear error", {
  with_data_dir("wru-preflight-empty", code = function(dir) {
    local_mocked_bindings(
      pb_download = function(...) stop("argument is of length zero"),
      .package = "wru"
    )
    ## names the file that is missing, rather than failing later in gzfile()
    expect_error(wru_data_preflight(), "wru-data-census_last_c\\.rds")
    ## carries the underlying download failure through
    expect_error(wru_data_preflight(), "argument is of length zero")
    ## and names the release it tried to read from
    expect_error(wru_data_preflight(), "v4\\.0\\.0")
  })
})

test_that("a download that reports success but writes nothing still errors", {
  with_data_dir("wru-preflight-silent", code = function(dir) {
    local_mocked_bindings(pb_download = function(...) invisible(NULL), .package = "wru")
    expect_error(wru_data_preflight(), "wru-data-census_last_c\\.rds")
  })
})

test_that("a failed download falls back to data already on disk", {
  with_data_dir("wru-preflight-cached", files = CORE, code = function(dir) {
    local_mocked_bindings(
      pb_download = function(...) stop("HTTP 403 rate limit exceeded"),
      .package = "wru"
    )
    ## Reported, but not fatal: the cached copies are usable offline.
    expect_message(wru_data_preflight(), "rate limit exceeded")
  })
})

test_that("a successful download is silent", {
  with_data_dir("wru-preflight-ok", code = function(dir) {
    local_mocked_bindings(
      pb_download = function(...) {
        for (f in CORE) saveRDS(data.frame(), file.path(dir, f))
        invisible(NULL)
      },
      .package = "wru"
    )
    expect_no_error(wru_data_preflight())
    expect_no_message(wru_data_preflight())
  })
})
