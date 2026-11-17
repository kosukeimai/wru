test_that("supplying `party` warns that it has no effect", {
  voters <- data.frame(
    surname = c("Smith", "Nguyen", "Garcia"),
    state = "NJ", county = "021", tract = "007000",
    PID = c(1, 2, 0),
    stringsAsFactors = FALSE
  )

  expect_warning(
    suppressMessages(
      predict_race(voter.file = voters, surname.only = TRUE, party = "PID")
    ),
    "no effect"
  )
})

test_that("`party` does not change predictions", {
  # The argument was silently ignored rather than applied. Pin that the
  # predictions are identical, so that if party is ever wired back into the
  # current models this test fails and has to be revisited deliberately.
  voters <- data.frame(
    surname = c("Smith", "Nguyen", "Garcia"),
    state = "NJ", county = "021", tract = "007000",
    PID = c(1, 2, 0),
    stringsAsFactors = FALSE
  )

  a <- suppressMessages(predict_race(voter.file = voters, surname.only = TRUE))
  b <- suppressWarnings(suppressMessages(
    predict_race(voter.file = voters, surname.only = TRUE, party = "PID")
  ))

  cols <- grep("^pred", names(a), value = TRUE)
  expect_equal(a[cols], b[cols])
})
