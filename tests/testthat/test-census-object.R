# Set random seed
set.seed(12345)

data(voters)

# Need a valid census key
k <- NULL

if (!is.null(k)) {
  voters.dc.nj <- voters[c(-3, -7), ]  # remove two NY cases from dataset
  census.dc.nj <- get_census_data(key = k, state = c("DC", "NJ"), census.geo = "tract", age = TRUE, sex = FALSE)  # create Census data object covering DC and NJ 
  predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = census.dc.nj, age = TRUE, sex = FALSE, party = "PID")

  # Or build the census object by parts
  censusObj2  <- list()
  county.dc <- census_geo_api(key = k, state = "DC", geo = "county", age = TRUE, sex = FALSE)
  tract.dc <- census_geo_api(key = k, state = "DC", geo = "tract", age = TRUE, sex = FALSE)
  censusObj2[["DC"]] <- list(state = "DC", county = county.dc, tract = tract.dc, age = TRUE, sex = FALSE)
  tract.nj <- census_geo_api(key = k, state = "NJ", geo = "tract", age = TRUE, sex = FALSE)
  county.nj <- census_geo_api(key = k, state = "NJ", geo = "county", age = TRUE, sex = FALSE)
  censusObj2[["NJ"]] <- list(state = "NJ", county = county.nj, tract = tract.nj, age = TRUE, sex = FALSE)

  predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = censusObj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, County)
  predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract)
  predict_race(voter.file = voters.dc.nj, census.geo = "county", census.data = censusObj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, County, Party)
  predict_race(voter.file = voters.dc.nj, census.geo = "tract", census.data = censusObj2, party = "PID", age = TRUE, sex = FALSE)  # Pr(Race | Surname, Tract, Party)
}
