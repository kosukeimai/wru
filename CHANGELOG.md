# wru (development version)

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
