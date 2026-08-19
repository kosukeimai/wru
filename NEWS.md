# wru (development version)

* Surname priors now come from the 2020 Census names files. The
  `wru-data-census_last_c.rds` dictionary on the v4.0.0 release replaces the
  2010-vintage table used since v2.0.0, and a Census first-name dictionary
  (`wru-data-census_first_c.rds`) is published for the first time. Predictions
  move for every name. The 2020 table covers 156,621 surnames against the 2010
  table's 162,253, and shifts probability mass toward the Hispanic and
  multi-race categories. `year` selects Census geography only: the name
  dictionaries are always the newest vintage, so `year = "2010"` combines 2010
  geography with 2020 name priors.
* `format_legacy_data()` now returns an object `predict_race()` can consume, and
  its output changes shape as a result (#175). It is keyed by state and carries
  `state`/`age`/`sex`/`year` alongside the `county`, `tract`, `block_group` and
  `block` tables, matching `get_census_data()`. Three defects are fixed: the
  population counts were labelled with 2010 SF1 names (`P005003` and friends)
  rather than the 2020 redistricting names the 2020 code path reads; the
  block-group table used a `blockGroup` id column that no merge looks for; and
  the tract, block-group and block ids were cut from the wrong end of the GEOID,
  so no block-level row ever matched a voter file. Wrapping the result in
  `list(ST = ...)` is no longer needed.
* A `census.data` object that carries legacy column names is now recognised by
  those names rather than by `year`, so an object built for the 2020
  redistricting tables or by an older `format_legacy_data()` works whichever
  `year` is requested (#175).
* `model = "fBISG"` now respects `skip_bad_geos = TRUE`: voter rows whose
  geography is absent from the census data are dropped (with a message) so the
  model can initialize and sample on a consistent set of rows, instead of
  erroring with "Some initial race values are NA" (#163).
* Fixed `use.counties = TRUE` raising `` `year` must be one of "2020" or "2010", not "3" ``:
  the county-filtered Census download passed arguments positionally with `year`
  omitted, so `retry` was read as `year` (#161).
* Provided `census.data` built with `age = TRUE` and/or `sex = TRUE` is now used
  instead of being silently re-downloaded; the cache check compared the stored
  flags against `FALSE` rather than the requested `age`/`sex` (#161).
* Surname-only predictions (`surname.only = TRUE` or `names.to.use = "surname"`)
  no longer require the first- and middle-name dictionaries to be present; those
  files are read only when first/middle names are actually used (#160).
* Added a `wru_progress` option to suppress the Census-download progress bar.
  Set `options(wru_progress = FALSE)` to silence the `furrr` progress bar (which
  bloats batch/non-interactive log files) while keeping the other progress
  messages (#150).
* `predict_race()` gains a `return.unmatched` argument (default `FALSE`). When
  `TRUE`, it appends boolean `last_matched` / `first_matched` / `middle_matched`
  columns reporting whether each name was found in the dictionary. The flags use
  a single dictionary match for every `model` (BISG, fBISG, eBISG), so they are
  comparable across models and name selections, and are computed before
  imputation (#105). Re-implements the feature proposed by @mdblocker in #165,
  keeping the original argument and column names.
* Fixed unmatched non-hyphenated names being silently matched to the genuine
  surname "Na" after the name-cleaning cascade coerced their match key to the
  string `"NA"`. Such names are now correctly treated as unmatched, so
  `impute.missing = FALSE` leaves them `NA` and `impute.missing = TRUE` imputes
  them rather than assigning the unrelated "Na" probabilities (#162).

# wru 3.1.1

* Updating release for piggyback fix.

# wru 3.1.0

* New feature release adding the eBISG model.

# wru 3.0.4

* Fixes a bug related to a list of variables not being unnested properly (#151, #153).

# wru 3.0.3

* Fixes a bug that pushed NaN into small population tracts (#151).

# wru 3.0.2

* Fixes a bug that led to overestimation of black and hispanic populations (#145).

# wru 3.0.1

* GitHub has changed their policy on binary formats in releases, need to refer to older version.

# wru 3.0.0

* Adding back age and sex functionality. Other improvements.

# wru 2.0.0

* Updated defaults to 2020 data, specified as next major version 2.0.

# wru 1.0.1

* Bug fixes for census URL and census year.

# wru 1.0.0

* Updates to BISG, inclusion of fBISG and other package improvements.

# wru 0.1.12

* Some Census API improvements (thanks to Silvia Kim).

# wru 0.1.11

* Minor fix requested by CRAN.

# wru 0.1.10

* Fixed minor warning message.

# wru 0.1.9

* Fixed `census_helper.R` so that state field can be lower case in user data.

# wru 0.1.8

* Updated to be compatible with U.S. Census API updates.

# wru 0.1.7

* Added testthat functionality.

# wru 0.1.6

* Removed extraneous documentation, renamed 2010 Surname List object, and added place as geography.

# wru 0.1.5

* Updated http to https to access U.S. Census API.

# wru 0.1.4

* Fixed error in merge_surnames.R and updated relevant documentation.

# wru 0.1.3

* Allows Census data download at level user prefers (block, tract, or county).

# wru 0.1.2

* Updated surname handling, enhanced demographics option, and improved error handling and documentation.

# wru 0.1.1

* New function to pre-download Census data and other minor improvements.

# wru 0.0.2

* Minor improvements.

# wru 0.0.1

* First version on CRAN.
