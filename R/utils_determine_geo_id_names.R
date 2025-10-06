determine_geo_id_names <- function(census.geo) {
  switch(
    census.geo,
    "tract" = c("county", "tract"),
    "block_group" = c("county", "tract", "block_group"),
    "block" = c("county", "tract", "block"),
    # Return `census.geo` unchanged for county, place, and zcta
    census.geo
  )
}
