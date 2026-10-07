skip_if_no_lbisg <- function() {
  skip_if_not_installed("reticulate")
  for (m in c("sklearn", "scipy", "torch", "sentence_transformers")) {
    skip_if_not(reticulate::py_module_available(m), paste("Python module", m, "not available"))
  }
}

## Two exclusive lists over 40 geographies with known group shares
sim_lists <- function(n = 8000, seed = 1) {
  set.seed(seed)
  geo <- sample(sprintf("g%02d", 1:40), n, replace = TRUE)
  share_a <- stats::setNames(seq(0.1, 0.9, length.out = 40), sprintf("g%02d", 1:40))
  grp <- ifelse(stats::runif(n) < share_a[geo], "a", "b")
  pool_a <- sprintf("A%03d", 1:200); pool_b <- sprintf("B%03d", 1:200)
  nm <- ifelse(grp == "a", sample(pool_a, n, TRUE), sample(pool_b, n, TRUE))
  list(names = nm, geo = geo, grp = grp, share_a = share_a,
       lists = list(a = pool_a[1:100], b = pool_b[1:120]),
       prior = cbind(a = share_a[geo], b = 1 - share_a[geo]))
}

test_that("lbisg_list_quality recovers coverage without labels", {
  skip_if_no_lbisg()
  s <- sim_lists()
  q <- lbisg_list_quality(s$names, s$geo, s$lists, s$prior)
  expect_equal(unname(q$coverage), c(0.5, 0.6), tolerance = 0.05)
  expect_equal(unname(q$precision), c(1, 1), tolerance = 0.05)
  expect_gt(q$D, 0.4)
})

test_that("lbisg_recover_prevalence recovers shares with and without a coarse prior", {
  skip_if_no_lbisg()
  s <- sim_lists()
  expect_error(lbisg_recover_prevalence(s$names, s$geo, s$lists), "exhaustive = TRUE")
  r <- lbisg_recover_prevalence(s$names, s$geo, s$lists, exhaustive = TRUE)
  expect_equal(unname(r$coverage), c(0.5, 0.6), tolerance = 0.05)
  expect_gt(stats::cor(r$prevalence$a, s$share_a[r$prevalence$geo]), 0.95)
  expect_equal(rowSums(r$prevalence[, c("a", "b")]), rep(1, 40), ignore_attr = TRUE)
  cp <- cbind(AB = rep(1, length(s$names)))
  r2 <- lbisg_recover_prevalence(s$names, s$geo, s$lists, coarse.prior = cp, coarse.map = c(a = "AB", b = "AB"),
                                 exhaustive = TRUE)
  expect_equal(r2$prevalence$a, r$prevalence$a, tolerance = 1e-6)
})

test_that("lbisg requires a list for every group but one residual", {
  skip_if_no_lbisg()
  s <- sim_lists()
  pr <- cbind(s$prior * 0.9, c = 0.05, d = 0.05)
  expect_error(lbisg(s$names, s$geo, s$lists, prior = pr), "only one residual group")
})

test_that("a coarse group without a list keeps its coarse share", {
  skip_if_no_lbisg()
  s <- sim_lists()
  cp <- cbind(AB = rep(0.9, length(s$names)), Z = 0.1)
  r <- lbisg_recover_prevalence(s$names, s$geo, s$lists, coarse.prior = cp,
                                coarse.map = c(a = "AB", b = "AB"), exhaustive = TRUE)
  expect_equal(r$prevalence$Z, rep(0.1, 40))
  expect_equal(r$prevalence$a + r$prevalence$b, rep(0.9, 40), tolerance = 1e-8)
  cp2 <- cbind(cp, Y = 0)
  expect_error(
    lbisg_recover_prevalence(s$names, s$geo, s$lists, coarse.prior = cp2,
                             coarse.map = c(a = "AB", b = "AB"), exhaustive = TRUE),
    "only one coarse group"
  )
})

test_that("lbisg accepts any embedding model ID and checks a fixed K", {
  expect_equal(lbisg_transformer("sentence-transformers/LaBSE"),
               "sentence-transformers/LaBSE")
  expect_equal(lbisg_transformer(list(transformer = "x/y", dim = 1L)), "x/y")
  expect_error(lbisg_transformer(1), "HuggingFace model ID")
  skip_if_no_lbisg()
  s <- sim_lists()
  expect_error(lbisg(s$names, s$geo, s$lists, prior = s$prior, K = 1),
               "at least the number of groups")
})

test_that("predict_race lBISG checks its inputs", {
  expect_error(
    predict_race(voter.file = data.frame(surname = "SMITH", state = "NJ"), model = "lBISG",
                 surname.only = TRUE, lists = list(whi = "SMITH")),
    "requires geography"
  )
  expect_error(
    predict_race(voter.file = data.frame(surname = "SMITH", state = "NJ"), model = "lBISG"),
    "requires 'lists'"
  )
})
