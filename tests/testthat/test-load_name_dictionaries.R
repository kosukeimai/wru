## Offline tests for load_name_dictionaries(): name_source selection + year.
## The raw-read I/O seam (read_name_dictionaries) is mocked so no network is hit.

dl <- function(names, whi) {
  data.frame(last_name = names, c_whi_last = whi, c_bla_last = 0.1, c_his_last = 0.1,
             c_asi_last = 0.1, c_oth_last = 0.1, stringsAsFactors = FALSE)
}
df_ <- function(names, whi) {
  data.frame(first_name = names, c_whi_first = whi, c_bla_first = 0.1, c_his_first = 0.1,
             c_asi_first = 0.1, c_oth_first = 0.1, stringsAsFactors = FALSE)
}
dm <- function(names) {
  data.frame(middle_name = names, c_whi_middle = 0.2, c_bla_middle = 0.2, c_his_middle = 0.2,
             c_asi_middle = 0.2, c_oth_middle = 0.2, stringsAsFactors = FALSE)
}
mock_raw <- function(census_first = NULL) {
  function(year) list(
    census_last  = dl(c("SMITH", "GARCIA"), c(0.60, 0.05)),
    last         = dl(c("GARCIA", "OCONNELL"), c(0.90, 0.95)),
    census_first = census_first,
    first        = df_(c("MARIA", "JOHN"), c(0.10, 0.70)),
    mid          = dm("LEE")
  )
}

test_that("census_only with middle names errors", {
  local_mocked_bindings(read_name_dictionaries = mock_raw(), .package = "wru")
  expect_error(
    load_name_dictionaries(namesToUse = "surname, first, middle",
                           name_source = "census_only", year = "2020"),
    "middle"
  )
})

test_that("mixed unions Census over voter-file for last names", {
  local_mocked_bindings(read_name_dictionaries = mock_raw(), .package = "wru")
  d <- load_name_dictionaries(namesToUse = "surname", name_source = "mixed", year = "2020")
  expect_setequal(d$last$last_name, c("SMITH", "GARCIA", "OCONNELL"))
  expect_equal(d$last$c_whi_last[d$last$last_name == "GARCIA"], 0.05) # Census wins
  expect_false("source" %in% names(d$last))
})

test_that("census_only selects only the Census last dictionary", {
  local_mocked_bindings(read_name_dictionaries = mock_raw(), .package = "wru")
  d <- load_name_dictionaries(namesToUse = "surname", name_source = "census_only", year = "2020")
  expect_setequal(d$last$last_name, c("SMITH", "GARCIA"))
})

