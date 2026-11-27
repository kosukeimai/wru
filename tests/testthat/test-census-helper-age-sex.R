## census_helper_new(age = TRUE and/or sex = TRUE) must return
## Pr(Race | Geo, Age, Sex), not Pr(Race | Geo).

## One 2020 DHC tract: White 10 men aged 20 + 30 women aged 65,
## Hispanic 30 men aged 20 + 10 women aged 65, Black 20 women aged 65.
## Pr(Race | Tract) = (whi 0.4, bla 0.2, his 0.4).
age_sex_census <- function(age, sex) {
  vars <- c(
    census_geo_api_names("2020", age = TRUE, sex = TRUE),
    census_geo_api_names("2020", sex = TRUE)
  )
  cols <- unique(unlist(vars))
  tract <- as.data.frame(as.list(stats::setNames(rep(0, length(cols)), cols)))
  tract$state <- "NJ"
  tract$county <- "001"
  tract$tract <- "000100"
  counts <- list(whi = c(10, 30), his = c(30, 10), bla = c(0, 20))
  for (r in names(counts)) {
    tract[vars[[paste0("r_mal_6_", r)]]] <- counts[[r]][1]  # men aged 20
    tract[vars[[paste0("r_fem_18_", r)]]] <- counts[[r]][2] # women aged 65-66
    tract[vars[[paste0("r_mal_", r)]]] <- counts[[r]][1]
    tract[vars[[paste0("r_fem_", r)]]] <- counts[[r]][2]
    tract[vars[[paste0("r_", r)]]] <- sum(counts[[r]])
  }
  list(NJ = list(state = "NJ", age = age, sex = sex, year = "2020", tract = tract))
}

age_sex_voters <- data.frame(
  id = 1:4, surname = "Doe", state = "NJ", county = "001", tract = "000100",
  age = c(20, 65, NA, 40), sex = c(0, 1, 0, 1)
)

run_helper <- function(age, sex) {
  out <- suppressMessages(census_helper_new(
    voter.file = age_sex_voters, geo = "tract", age = age, sex = sex,
    census.data = age_sex_census(age, sex), year = "2020"
  ))
  out[order(out$id), ]
}

test_that("age = TRUE conditions on the voter's age category", {
  out <- run_helper(age = TRUE, sex = FALSE)
  expect_equal(unname(unlist(out[1, c("r_whi", "r_bla", "r_his")])), c(0.25, 0, 0.75))
  expect_equal(unname(unlist(out[2, c("r_whi", "r_bla", "r_his")])), c(30, 20, 10) / 60)
})

test_that("sex = TRUE conditions on the voter's sex", {
  out <- run_helper(age = FALSE, sex = TRUE)
  expect_equal(unname(unlist(out[1, c("r_whi", "r_bla", "r_his")])), c(0.25, 0, 0.75))
  expect_equal(unname(unlist(out[2, c("r_whi", "r_bla", "r_his")])), c(30, 20, 10) / 60)
})

test_that("age = TRUE, sex = TRUE conditions on the age-by-sex cell", {
  out <- run_helper(age = TRUE, sex = TRUE)
  expect_equal(unname(unlist(out[1, c("r_whi", "r_bla", "r_his")])), c(0.25, 0, 0.75))
  expect_equal(unname(unlist(out[2, c("r_whi", "r_bla", "r_his")])), c(30, 20, 10) / 60)
})

test_that("missing age or an empty age cell falls back to Pr(Race | Geo)", {
  out <- run_helper(age = TRUE, sex = TRUE)
  expect_equal(unname(unlist(out[3, c("r_whi", "r_bla", "r_his")])), c(0.4, 0.2, 0.4))
  expect_equal(unname(unlist(out[4, c("r_whi", "r_bla", "r_his")])), c(0.4, 0.2, 0.4))
})

test_that("age = FALSE, sex = FALSE still returns Pr(Race | Geo)", {
  out <- run_helper(age = FALSE, sex = FALSE)
  expect_equal(unname(unlist(out[1, c("r_whi", "r_bla", "r_his")])), c(0.4, 0.2, 0.4))
})
