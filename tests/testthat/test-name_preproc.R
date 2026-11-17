## .name_preproc() (used by fBISG) previously stripped hyphens and spaces
## in-place in steps 2-3 and then attempted to split double-barreled names on
## `[-, ]` in steps 5-6 using `(!match_tmp) & grep(...)`. Because hyphens and
## spaces had already been removed from `post_names`, steps 5-6 were dead code
## and double-barreled names like "SMITH-JONES" or "FAKE-GARCIA" never matched
## their constituent parts.

test_that(".name_preproc splits double-barreled names on first or second part", {
  target <- c("SMITH", "GARCIA", "LOPEZ")

  ## Matches first part of hyphenated or space-separated double-barreled name
  expect_equal(
    .name_preproc(c("Smith-Unknown", "Garcia Unknown"), target),
    c("SMITH", "GARCIA")
  )

  ## Matches second part when first part is not in target_names
  expect_equal(
    .name_preproc(c("Unknown-Smith", "Unknown Lopez"), target),
    c("SMITH", "LOPEZ")
  )

  ## Preserves step 2/3 punctuation and space stripping when concatenated form matches
  expect_equal(
    .name_preproc(c("O'Brien", "De La Cruz"), c("OBRIEN", "DELACRUZ")),
    c("OBRIEN", "DELACRUZ")
  )
})
