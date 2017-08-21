context("tests surname only predictions")
rm(list=ls())

test_that("tests surname only predictions", {
  # Set random seed
  set.seed(12345)

  data(voters)
  x <- predict_race(voter.file = voters, surname.only = T)
  expect_that(dim(x), is_equivalent_to(c(10,18)))
  expect_that(sum(is.na(x)), is_equivalent_to(0))
  expect_that(round(x[x$surname == "Khanna", "pred.whi"], 4), is_equivalent_to(0.0676))
  expect_that(round(x[x$surname == "Johnson", "pred.his"], 4), is_equivalent_to(0.0236))
})
