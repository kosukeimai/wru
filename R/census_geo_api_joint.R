census_geo_api_joint <- function(key, state, geo = "tract", age = FALSE, sex = FALSE,  retry = 0) 
{
  if (missing(key)) {
    stop("Must enter U.S. Census API key, which can be requested at https://api.census.gov/data/key_signup.html.")
  }
  state <- toupper(state)
  df.out <- NULL
  fips.codes <- get("State.FIPS")
  state.fips <- fips.codes[fips.codes$State == state, "FIPS"]
  state.fips <- ifelse(nchar(state.fips) == 1, paste0("0", 
                                                      state.fips), state.fips)
  if (age == FALSE & sex == FALSE) {
    num <- ifelse(3:10 != 10, paste("0", 3:10, sep = ""), 
                  "10")
    vars <- paste("P0050", num, sep = "")
  }
  if (age == FALSE & sex == TRUE) {
    eth.let <- c("I", "B", "H", "D", "E", "F", "C")
    num <- as.character(c("01", "02", "26"))
    vars <- NULL
    for (e in 1:length(eth.let)) {
      vars <- c(vars, paste("P012", eth.let[e], "0", num, 
                            sep = ""))
    }
  }
  if (age == TRUE & sex == FALSE) {
    eth.let <- c("I", "B", "H", "D", "E", "F", "C")
    num <- as.character(c(c("01", "03", "04", "05", "06", 
                            "07", "08", "09"), seq(10, 25), seq(27, 49)))
    vars <- NULL
    for (e in 1:length(eth.let)) {
      vars <- c(vars, paste("P012", eth.let[e], "0", num, 
                            sep = ""))
    }
  }
  if (age == TRUE & sex == TRUE) {
    eth.let <- c("I", "B", "H", "D", "E", "F", "C")
    num <- as.character(c(c("01", "03", "04", "05", "06", 
                            "07", "08", "09"), seq(10, 25), seq(27, 49)))
    vars <- NULL
    for (e in 1:length(eth.let)) {
      vars <- c(vars, paste("P012", eth.let[e], "0", num, 
                            sep = ""))
    }
  }
  if (geo == "place") {
    geo.merge <- c("state", "place")
    region <- paste("for=place:*&in=state:", state.fips, 
                    sep = "")
    census <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", 
                             key = key, vars = vars, region = region, retry)
  }
  if (geo == "county") {
    geo.merge <- c("state", "county")
    region <- paste("for=county:*&in=state:", state.fips, 
                    sep = "")
    census <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", 
                             key = key, vars = vars, region = region, retry)
  }
  if (geo == "tract") {
    geo.merge <- c("state", "county", "tract")
    region_county <- paste("for=county:*&in=state:", state.fips, 
                           sep = "")
    county_df <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", 
                                key = key, vars = vars, region = region_county, retry)
    county_list <- county_df$county
    census <- NULL
    for (c in 1:length(county_list)) {
      print(paste("County ", c, " of ", length(county_list), 
                  ": ", county_list[c], sep = ""))
      region_county <- paste("for=tract:*&in=state:", state.fips, 
                             "+county:", county_list[c], sep = "")
      census.temp <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", 
                                    key = key, vars = vars, region = region_county, 
                                    retry)
      census <- rbind(census, census.temp)
    }
    rm(census.temp)
  }
  if (geo == "block") {
    geo.merge <- c("state", "county", "tract", "block")
    region_county <- paste("for=county:*&in=state:", state.fips, 
                           sep = "")
    county_df <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", 
                                key = key, vars = vars, region = region_county, retry)
    county_list <- county_df$county
    census <- NULL
    for (c in 1:length(county_list)) {
      print(paste("County ", c, " of ", length(county_list), 
                  ": ", county_list[c], sep = ""))
      region_tract <- paste("for=tract:*&in=state:", state.fips, 
                            "+county:", county_list[c], sep = "")
      print(region_tract)
      tract_df <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", 
                                 key = key, vars = vars, region = region_tract, 
                                 retry)
      tract_list <- tract_df$tract
      for (t in 1:length(tract_list)) {
        print(paste("Tract ", t, " of ", length(tract_list), 
                    ": ", tract_list[t], sep = ""))
        region_block <- paste("for=block:*&in=state:", 
                              state.fips, "+county:", county_list[c], "+tract:", 
                              tract_list[t], sep = "")
        census.temp <- get_census_api("https://api.census.gov/data/2010/dec/sf1?", 
                                      key = key, vars = vars, region = region_block, 
                                      retry)
        census <- rbind(census, census.temp)
      }
    }
    rm(census.temp)
  }
  census$state <- state
  if (age == FALSE & sex == FALSE) {
    census$r_whi <- census$P005003/sum(census$P005003)
    census$r_bla <- census$P005004/sum(census$P005004)
    census$r_his <- census$P005010/sum(census$P005010)
    census$r_asi <- (census$P005006 + census$P005007)/(sum(census$P005006) + 
                                                         sum(census$P005007))
    census$r_oth <- (census$P005005 + census$P005008 + census$P005009)/(sum(census$P005005) + 
                                                                          sum(census$P005008) + sum(census$P005009))
  }
  if (age == FALSE & sex == TRUE) {
    eth.cen <- c("whi", "bla", "his", "asi", "oth")
    eth.let <- c("I", "B", "H", "D", "F")
    for (i in 1:length(eth.cen)) {
      if (i != 4 & i != 5) {
        census[paste("r_mal", eth.cen[i], sep = "_")] <- census[paste("P012", 
                                                                      eth.let[i], "002", sep = "")]
        census[paste("r_fem", eth.cen[i], sep = "_")] <- census[paste("P012", 
                                                                      eth.let[i], "026", sep = "")]
      }
      if (i == 4) {
        census[paste("r_mal", eth.cen[i], sep = "_")] <- (census$P012D002 + 
                                                            census$P012E002)
        census[paste("r_fem", eth.cen[i], sep = "_")] <- (census$P012D026 + 
                                                            census$P012E026)
      }
      if (i == 5) {
        census[paste("r_mal", eth.cen[i], sep = "_")] <- (census$P012C002 + 
                                                            census$P012F002)
        census[paste("r_fem", eth.cen[i], sep = "_")] <- (census$P012C026 + 
                                                            census$P012F026)
      }
    }
  }
  if (age == TRUE & sex == FALSE) {
    eth.cen <- c("whi", "bla", "his", "asi", "oth")
    eth.let <- c("I", "B", "H", "D", "F")
    age.cat <- c(seq(1, 23), seq(1, 23))
    age.cen <- as.character(c(c("03", "04", "05", "06", "07", 
                                "08", "09"), seq(10, 25), seq(27, 49)))
    for (i in 1:length(eth.cen)) {
      for (j in 1:23) {
        if (i != 4 & i != 5) {
          census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012",eth.let[i], "0", age.cen[j], sep = "")] + 
                                                                      census[paste("P012", eth.let[i], "0", age.cen[j + 23], sep = "")])
        }
        if (i == 4) {
          census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012D0", age.cen[j], sep = "")] + 
                                                                      census[paste("P012D0", age.cen[j + 23], sep = "")] +
                                                                      census[paste("P012E0", age.cen[j], sep = "")] + 
                                                                      census[paste("P012E0", age.cen[j + 23], sep = "")])
        }
        if (i == 5) {
          census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012C0",age.cen[j], sep = "")] + 
                                                                      census[paste("P012C0", age.cen[j + 23], sep = "")] + 
                                                                      census[paste("P012F0", age.cen[j], sep = "")] + 
                                                                      census[paste("P012F0",age.cen[j + 23], sep = "")])
        }
      }
    }
  }
  if (age == TRUE & sex == TRUE) {
    eth.cen <- c("whi", "bla", "his", "asi", "oth")
    eth.let <- c("I", "B", "H", "D", "F")
    sex.let <- c("mal", "fem")
    age.cat <- c(seq(1, 23), seq(1, 23))
    age.cen <- as.character(c(c("03", "04", "05", "06", "07", 
                                "08", "09"), seq(10, 25), seq(27, 49)))
    for (i in 1:length(eth.cen)) {
      for (k in 1:length(sex.let)) {
        for (j in 1:23) {
          if (k == 2) {
            j <- j + 23
          }
          if (i != 4 & i != 5) {
            census[paste("r", sex.let[k], age.cat[j], 
                         eth.cen[i], sep = "_")] <- census[paste("P012",eth.let[i], "0", age.cen[j], sep = "")]
          }
          if (i == 4) {
            census[paste("r", sex.let[k], age.cat[j], 
                         eth.cen[i], sep = "_")] <- (census[paste("P012D0", age.cen[j], sep = "")] + 
                                                       census[paste("P012E0", age.cen[j], sep = "")])
          }
          if (i == 5) {
            census[paste("r", sex.let[k], age.cat[j], 
                         eth.cen[i], sep = "_")] <- (census[paste("P012C0",  age.cen[j], sep = "")] + 
                                                       census[paste("P012F0", age.cen[j], sep = "")])
          }
        }
      }
    }
  }
  return(census)
}
