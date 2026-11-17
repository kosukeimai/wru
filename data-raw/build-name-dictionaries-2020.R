## Build 2020 Census name dictionaries for wru.
##
## Derives `census_last_c` and `census_first_c` -- P(name | race) tables whose
## race columns each sum to 1 -- from the *standard* 2020 Census names-by-race-
## and-Hispanic-origin files. The standard files have differential-privacy noise
## already handled losslessly: negative cells are zeroed and each row is
## rebalanced so it still sums to the name's true total count.
##
## Source page:
##   https://www.census.gov/topics/population/genealogy/data/2020_names.html
##
## Output schema (matches the existing piggyback dictionaries so the runtime
## `stopifnot(identical(names(...)))` checks in predict_race_new() pass):
##   census_last_c : last_name,  c_whi_last,  c_bla_last,  c_his_last,  c_asi_last,  c_oth_last
##   census_first_c: first_name, c_whi_first, c_bla_first, c_his_first, c_asi_first, c_oth_first
##
## Usage:
##   Rscript data-raw/build-name-dictionaries-2020.R [output_dir]

suppressMessages(library(readxl))

BASE <- "https://www2.census.gov/topics/genealogy/2020surnames"
FILES <- c(
  last  = "Names2020_LastNames_RaceHispanic.xlsx",
  first = "Names2020_FirstNames_RaceHispanic.xlsx"
)

## The six race count columns are the trailing six columns of each by-race file,
## in this fixed order:
##   1 Non-Hispanic White alone
##   2 Non-Hispanic Black alone
##   3 Non-Hispanic American Indian / Alaska Native alone
##   4 Non-Hispanic Asian + Native Hawaiian / Pacific Islander alone
##   5 Non-Hispanic Two or More Races
##   6 Hispanic or Latino origin
## wru collapses these six into five: oth = AIAN + Two-or-More.
CENSUS_CATS <- c("white", "black", "aian", "asian", "tomr", "hisp")
WRU_ETH     <- c("whi", "bla", "his", "asi", "oth")

args      <- commandArgs(trailingOnly = TRUE)
out_dir   <- if (length(args) >= 1) args[[1]] else file.path("data-raw", "name-dict-2020")
cache_dir <- file.path(tempdir(), "wru-2020-names")
dir.create(out_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

download_if_needed <- function(fname) {
  dest <- file.path(cache_dir, fname)
  if (!file.exists(dest)) {
    message("Downloading ", fname, " ...")
    utils::download.file(file.path(BASE, fname), dest, mode = "wb", quiet = TRUE)
  }
  dest
}

build_dictionary <- function(xlsx_path, name_type) {
  stopifnot(name_type %in% c("last", "first"))
  raw <- as.data.frame(readxl::read_excel(xlsx_path, skip = 2))

  nm        <- toupper(trimws(as.character(raw[[1]])))
  race_cols <- (ncol(raw) - 5L):ncol(raw)
  counts    <- vapply(raw[race_cols], as.numeric, numeric(nrow(raw)))
  colnames(counts) <- CENSUS_CATS

  ## Drop only the aggregate footer; keep genuine surnames such as "NA".
  keep   <- !is.na(nm) & nzchar(nm) & nm != "ALL OTHER NAMES"
  nm     <- nm[keep]
  counts <- counts[keep, , drop = FALSE]

  if (anyDuplicated(nm)) {
    stop("Unexpected duplicate names in ", basename(xlsx_path))
  }

  ## Collapse six Census categories into the wru five.
  wru <- cbind(
    whi = counts[, "white"],
    bla = counts[, "black"],
    his = counts[, "hisp"],
    asi = counts[, "asian"],
    oth = counts[, "aian"] + counts[, "tomr"]
  )
  if (anyNA(wru) || any(wru < 0)) {
    stop("NA or negative counts after collapse in ", basename(xlsx_path),
         " -- standard files should be non-negative.")
  }

  ## Column-normalize -> P(name | race); each race column sums to 1.
  probs <- sweep(wru, 2, colSums(wru), "/")

  out <- data.frame(nm, probs, stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- c(
    paste(name_type, "name", sep = "_"),
    paste0("c_", WRU_ETH, "_", name_type)
  )
  out
}

verify_dictionary <- function(d, name_type) {
  prob_cols <- grep("^c_", names(d), value = TRUE)
  sums      <- colSums(d[prob_cols])
  cat(sprintf("  %s: %d names\n", name_type, nrow(d)))
  cat("  column sums:", paste(sprintf("%.6f", sums), collapse = " "), "\n")
  stopifnot(
    length(prob_cols) == 5L,
    all(abs(sums - 1) < 1e-8),
    !anyNA(d[prob_cols]),
    all(d[prob_cols] >= 0)
  )
}

census_last_c  <- build_dictionary(download_if_needed(FILES[["last"]]),  "last")
census_first_c <- build_dictionary(download_if_needed(FILES[["first"]]), "first")

cat("Verifying derived dictionaries:\n")
verify_dictionary(census_last_c,  "census_last_c")
verify_dictionary(census_first_c, "census_first_c")

saveRDS(census_last_c,  file.path(out_dir, "wru-data-census_last_c.rds"))
saveRDS(census_first_c, file.path(out_dir, "wru-data-census_first_c.rds"))
cat("Wrote dictionaries to ", normalizePath(out_dir), "\n")
