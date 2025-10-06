options("piggyback.verbose" = FALSE)
options("wru_data_wd" = TRUE)

test_that("legacy data returns expected geo groups",{
  skip_on_cran()
  de <- format_legacy_data(PL94171::pl_url('DE', 2020), state = "DE")
  
  expect_named(de, c("county", "tract", "blockGroup", "block"))
})
