## The fBISG sampler uses race-by-geography counts only, so it cannot condition
## on age or sex. predict_race() used to accept the combination and return
## predictions that ignored both arguments.

fbisg_vf <- data.frame(
  surname = c("Smith", "Garcia"),
  state = "NJ",
  county = "021",
  tract = "004000",
  age = c(30, 45),
  sex = c(0, 1),
  stringsAsFactors = FALSE
)

test_that("predict_race(model = 'fBISG') refuses age = TRUE or sex = TRUE", {
  cd <- list(NJ = list(year = "2020", age = TRUE, sex = TRUE, tract = data.frame()))
  for (flags in list(c(TRUE, FALSE), c(FALSE, TRUE), c(TRUE, TRUE))) {
    expect_error(
      suppressMessages(predict_race(
        voter.file = fbisg_vf, census.geo = "tract", census.data = cd,
        age = flags[1], sex = flags[2], model = "fBISG",
        race.init = c(1L, 3L), control = list(verbose = FALSE)
      )),
      "not supported when model = 'fBISG'"
    )
  }
})

test_that("predict_race(model = 'BISG') still accepts age and sex", {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    predict_race_new = function(..., age = FALSE, sex = FALSE) {
      captured$age <- age
      captured$sex <- sex
      vf <- list(...)$voter.file
      for (r in c("whi", "bla", "his", "asi", "oth")) vf[[paste0("pred.", r)]] <- 0.2
      vf
    },
    .package = "wru"
  )
  cd <- list(NJ = list(year = "2020", age = TRUE, sex = TRUE, tract = data.frame()))

  suppressMessages(predict_race(
    voter.file = fbisg_vf, census.geo = "tract", census.data = cd,
    age = TRUE, sex = TRUE, model = "BISG"
  ))

  expect_true(captured$age)
  expect_true(captured$sex)
})

test_that("predict_race_me() takes no age or sex argument", {
  expect_false(any(c("age", "sex") %in% names(formals(predict_race_me))))
})
