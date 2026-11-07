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

test_that("priors whose rows do not sum to one are rejected", {
  skip_if_no_lbisg()
  s <- sim_lists()
  expect_error(lbisg(s$names, s$geo, s$lists, prior = s$prior * 0.8),
               "must sum to 1")
  cp <- cbind(AB = rep(0.7, length(s$names)))
  expect_error(
    lbisg_recover_prevalence(s$names, s$geo, s$lists, coarse.prior = cp,
                             coarse.map = c(a = "AB", b = "AB"), exhaustive = TRUE),
    "must sum to 1"
  )
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

## A fitted lbisg object with simulated list scores for one or two name
## fields: three groups over 100 geographies in 5 states, with scores that
## depend only on the group and a three-level outcome that depends only on the
## group
sim_downstream <- function(n_geo = 100, seed = 1) {
  set.seed(seed)
  M <- t(replicate(n_geo, { x <- stats::rgamma(3, 0.7); x / sum(x) }))
  colnames(M) <- c("a", "b", "c")
  sizes <- stats::rpois(n_geo, 150) + 20
  gi <- rep(seq_len(n_geo), sizes)
  n <- length(gi)
  grp <- vapply(gi, function(g) sample.int(3, 1, prob = M[g, ]), integer(1))
  centers <- rbind(c(2, -2, -2), c(-2, 2, -2), c(-1, -1, 1.5))
  scores <- function() {
    stats::plogis(centers[grp, ] + matrix(stats::rnorm(n * 3, 0, 1.6), n))
  }
  p_dem <- c(0.8, 0.5, 0.3)[grp]
  p_rep <- c(0.15, 0.4, 0.5)[grp]
  u <- stats::runif(n)
  party <- ifelse(u < p_dem, "DEM", ifelse(u < p_dem + p_rep, "REP", "OTH"))
  county <- (gi - 1) %/% (n_geo / 20)
  list(
    fit = structure(list(prior = M[gi, ], fields = list(
      surname = list(scores = scores(), K = 30L),
      first = list(scores = scores(), K = 20L))), class = "lbisg"),
    geo = sprintf("g%03d", gi),
    state = sprintf("s%d", county %/% 4),
    party = party,
    dem = c(a = 0.8, b = 0.5, c = 0.3),
    share = c(a = 0.8 / 0.95, b = 0.5 / 0.9, c = 0.3 / 0.8)
  )
}

test_that("lbisg_downstream estimates the group means of each geographic unit", {
  skip_if_no_lbisg()
  s <- sim_downstream()
  for (fields in list("surname", c("surname", "first"))) {
    K <- c(surname = 30L, first = 20L)[fields]
    ds <- lbisg_downstream(s$fit, s$party, s$geo, folds = s$state, K = K,
                           fields = fields, verbose = FALSE)
    expect_equal(nrow(ds$estimates), 100 * 3 * 3)
    expect_named(ds$estimates,
                 c("geo", "group", "outcome", "estimate", "se", "lower", "upper"))
    expect_equal(dim(ds$mu), c(100, 3, 3))
    expect_equal(dim(ds$K), c(5, length(fields)))
    expect_equal(rownames(ds$K), sort(unique(s$state)))
    expect_false(any(ds$singular))
    expect_true(all(ds$estimates$se > 0))
    # the rows and the covariance matrices line up
    row <- ds$estimates[734, ]
    expect_equal(ds$mu[row$geo, row$group, row$outcome], row$estimate)
    lab <- paste(row$group, row$outcome, sep = ":")
    expect_equal(sqrt(ds$vcov[[row$geo]][lab, lab]), row$se)
    dem <- apply(ds$mu[, , "DEM"], 2, stats::median)
    expect_equal(dem[names(s$dem)], s$dem, tolerance = 0.25)
    sh <- lbisg_downstream_share(ds, "DEM", c("DEM", "REP"))
    expect_equal(nrow(sh), 100 * 3)
    expect_true(all(sh$se > 0))
    med <- tapply(sh$share, sh$group, stats::median)
    expect_equal(as.numeric(med[names(s$share)]), unname(s$share),
                 tolerance = 0.25)
  }
})

test_that("lbisg_downstream chooses K in each fold and partitions at random", {
  skip_if_no_lbisg()
  s <- sim_downstream(n_geo = 40)
  ctl <- lbisg_control(K.grid = c(3, 5, 10), geo.folds = 3, seed = 7)
  ds <- lbisg_downstream(s$fit, s$party, s$geo, fields = "surname", V = 3,
                         se = FALSE, control = ctl, verbose = FALSE)
  expect_equal(dim(ds$K), c(3, 1))
  expect_true(all(ds$K >= 3))
  expect_null(ds$vcov)
  expect_true(all(is.na(ds$estimates$se)))
  expect_setequal(ds$folds, c("0", "1", "2"))
  expect_equal(names(ds$folds), sort(unique(s$geo)))
})

test_that("lbisg_downstream gives NaN and a warning when a group has no prior share", {
  skip_if_no_lbisg()
  s <- sim_downstream(n_geo = 40)
  at <- s$geo == "g003"
  s$fit$prior[at, ] <- matrix(c(0.6, 0, 0.4), sum(at), 3, byrow = TRUE)
  expect_warning(
    ds <- lbisg_downstream(s$fit, s$party, s$geo, folds = s$state,
                           fields = "surname", K = 10L, verbose = FALSE),
    "1 of 40 geographic units"
  )
  expect_identical(names(which(ds$singular)), "g003")
  expect_true(all(is.nan(ds$mu["g003", , ])))
  expect_false(anyNA(ds$mu["g004", , ]))
})

test_that("lbisg_downstream checks its inputs", {
  skip_if_no_lbisg()
  s <- sim_downstream(n_geo = 40)
  expect_error(lbisg_downstream(s$fit, s$party, s$geo, fields = "middle"),
               "no name field")
  expect_error(lbisg_downstream(s$fit, s$party, s$geo, K = c(10L, 10L, 10L)),
               "one value per name field")
  by_person <- rep(c("x", "y"), length.out = length(s$geo))
  expect_error(lbisg_downstream(s$fit, s$party, s$geo, folds = by_person),
               "constant within each geographic unit")
  expect_error(lbisg_downstream(s$fit, s$party[-1], s$geo),
               "one value per person")
})

test_that("kmeans_occupied leaves every cluster with a fitting observation", {
  skip_if_no_lbisg()
  mod <- get_lbisg_module()
  set.seed(3)
  Z <- round(matrix(stats::rnorm(9000), ncol = 3), 1)
  rows <- sort(sample.int(3000, 2000)) - 1L
  res <- mod$kmeans_occupied(Z, rows, 300L, 0L)
  labels <- as.integer(res[[1]])
  K_used <- as.integer(res[[2]])
  expect_setequal(unique(labels[rows + 1L]), seq_len(K_used) - 1L)
  expect_true(all(labels < K_used))
})
