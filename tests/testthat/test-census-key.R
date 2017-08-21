context("tests predictions using the census key")
rm(list=ls())

# Need a valid census key
k <- NULL

test_that("tests predictions using the census key", {
  # Set random seed
  set.seed(12345)

  data(voters)

  if (!is.null(k)) {
    x = predict_race(voter.file = voters, census.geo = "tract", census.key = k, party = "PID")
    expect_that(dim(x), is_equivalent_to(c(10,18)))
    expect_that(sum(is.na(x)), is_equivalent_to(0))
    expect_that(sum(x$surname == "Johnson"), is_equivalent_to(1))
    expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0819))
    expect_that(round(x[x$surname == "Morse", "pred.his"], 4), is_equivalent_to(0.0034))
  }
})

