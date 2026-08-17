options("piggyback.verbose" = FALSE)
options("wru_data_wd" = TRUE)

# Relabel a 2020 redistricting census object with the SF1 column names that
# format_legacy_data() emitted before wru 4.0.0. Same counts, older labels.
as_sf1_names <- function(obj) {
  map <- c(
    P2_005N = "P005003", P2_006N = "P005004", P2_002N = "P005010",
    P2_008N = "P005006", P2_009N = "P005007", P2_007N = "P005005",
    P2_010N = "P005008", P2_011N = "P005009"
  )
  for (state in names(obj)) {
    for (geo in c("county", "tract", "block_group", "block")) {
      nms <- names(obj[[state]][[geo]])
      hit <- nms %in% names(map)
      nms[hit] <- map[nms[hit]]
      names(obj[[state]][[geo]]) <- nms
    }
  }
  obj
}

test_that("preflight accepts SF1 legacy column names when year is 2020", {
  census <- as_sf1_names(readRDS(test_path("data/census_test_nj_block_2020.rds")))
  expect_no_error(census_data_preflight(census, "block", "2020"))
})

test_that("SF1 and redistricting legacy names give the same 2020 race proportions", {
  data(voters)
  redistricting <- readRDS(test_path("data/census_test_nj_block_2020.rds"))
  sf1 <- as_sf1_names(redistricting)

  race_cols <- c("r_whi", "r_bla", "r_his", "r_asi", "r_oth")
  args <- list(
    voter.file = voters, states = "NJ", geo = "tract",
    age = FALSE, sex = FALSE, year = "2020", use.counties = FALSE
  )
  x <- do.call(census_helper_new, c(args, list(census.data = redistricting)))
  y <- do.call(census_helper_new, c(args, list(census.data = sf1)))

  expect_equal(y[, race_cols], x[, race_cols])
})
