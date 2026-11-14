## predict_race() with model = "fBISG" previously passed `sex = age` instead of
## `sex = sex` to predict_race_me().

test_that("predict_race(model = 'fBISG') forwards sex = sex, not sex = age, to predict_race_me()", {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    predict_race_me = function(..., age = FALSE, sex = FALSE) {
      captured$age <- age
      captured$sex <- sex
      vf <- list(...)$voter.file
      vf$pred.whi <- 0.2
      vf$pred.bla <- 0.2
      vf$pred.his <- 0.2
      vf$pred.asi <- 0.2
      vf$pred.oth <- 0.2
      vf
    },
    .package = "wru"
  )

  vf <- data.frame(
    surname = c("Smith", "Garcia"),
    state = "NJ",
    county = "021",
    tract = "004000",
    age = c(30, 45),
    sex = c(0, 1),
    stringsAsFactors = FALSE
  )
  cd <- list(NJ = list(year = "2020", age = FALSE, sex = TRUE, tract = data.frame()))

  suppressMessages(predict_race(
    voter.file = vf,
    census.geo = "tract",
    census.data = cd,
    age = FALSE,
    sex = TRUE,
    model = "fBISG",
    race.init = c(1L, 3L),
    control = list(verbose = FALSE)
  ))

  expect_false(captured$age)
  expect_true(captured$sex)

  suppressMessages(predict_race(
    voter.file = vf,
    census.geo = "tract",
    census.data = cd,
    age = TRUE,
    sex = FALSE,
    model = "fBISG",
    race.init = c(1L, 3L),
    control = list(verbose = FALSE)
  ))

  expect_true(captured$age)
  expect_false(captured$sex)
})
