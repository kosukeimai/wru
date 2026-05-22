# wru (development version)

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
