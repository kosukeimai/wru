## resolve_name_source(): maps the deprecated census.surname flag to name_source.

test_that("name_source is returned unchanged when census.surname is not supplied", {
  expect_equal(resolve_name_source("mixed", NULL), "mixed")
  expect_equal(resolve_name_source("census_only", NULL), "census_only")
  expect_equal(resolve_name_source("vf_only", NULL), "vf_only")
})

test_that("deprecated census.surname warns and maps: TRUE->mixed, FALSE->vf_only", {
  expect_warning(r_true <- resolve_name_source("mixed", TRUE), "deprecated")
  expect_equal(r_true, "mixed")
  expect_warning(r_false <- resolve_name_source("mixed", FALSE), "deprecated")
  expect_equal(r_false, "vf_only")
})
