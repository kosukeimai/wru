library(dplyr)
library(tibble)
library(tidycensus)

state_fips <- tidycensus::fips_codes |> 
  tibble::as_tibble() |> 
  dplyr::distinct(state, state_code, state_name) |> 
  tibble::remove_rownames()

usethis::use_data(state_fips, overwrite = TRUE)
