#' Census helper function.
#'
#' \code{census_helper} links user-input dataset with Census geographic data.
#'
#' This function allows users to link their geocoded dataset (e.g., voter file)
#' with U.S. Census data (2010 or 2020). The function extracts Census Summary File data
#' at the county, tract, block, or place level. Census data calculated are
#' Pr(Geolocation | Race) where geolocation is county, tract, block, or place.
#'
#' @param key A required character object. Must contain user's Census API
#'  key, which can be requested \href{https://api.census.gov/data/key_signup.html}{here}.
#' @param voter.file An object of class \code{data.frame}. Must contain field(s) named
#'  \code{\var{county}}, \code{\var{tract}}, \code{\var{block}}, and/or \code{\var{place}}
#'  specifying geolocation. These should be character variables that match up with
#'  U.S. Census categories. County should be three characters (e.g., "031" not "31"),
#'  tract should be six characters, and block should be four characters.
#'  Place should be five characters if it is included.
#' @param states A character vector specifying which states to extract
#'  Census data for, e.g. \code{c("NJ", "NY")}. Default is \code{"all"}, which extracts
#'  Census data for all states contained in user-input data.
#' @param geo A character object specifying what aggregation level to use.
#'  Use \code{"county"}, \code{"tract"}, \code{"block"} or \code{"place"}. Default is \code{"tract"}.
#'  Warning: extracting block-level data takes very long.
#' @param age A \code{TRUE}/\code{FALSE} object indicating whether to condition on
#'  age or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Age | Race).
#'  If \code{\var{sex}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param sex A \code{TRUE}/\code{FALSE} object indicating whether to condition on
#'  sex or not. If \code{FALSE} (default), function will return Pr(Geolocation | Race).
#'  If \code{TRUE}, function will return Pr(Geolocation, Sex | Race).
#'  If \code{\var{age}} is also \code{TRUE}, function will return Pr(Geolocation, Age, Sex | Race).
#' @param year A character object specifying the year of U.S. Census data to be downloaded.
#'  Use \code{"2010"}, or \code{"2020"}. Default is \code{"2010"}.
#'  Warning: 2020 U.S. Census data is downloaded only when \code{\var{age}} and
#'  \code{\var{sex}} are both \code{FALSE}.
#' @param census.data A optional census object of class \code{list} containing
#' pre-saved Census geographic data. Can be created using \code{get_census_data} function.
#' If \code{\var{census.data}} is provided, the \code{\var{age}} element must have the same value
#' as the \code{\var{age}} option specified in this function (i.e., \code{TRUE} in both or
#' \code{FALSE} in both). Similarly, the \code{\var{sex}} element in the object provided in
#' \code{\var{census.data}} must have the same value as the \code{\var{sex}} option here.
#' Moreover, the \code{\var{year}} element in the object provided in \code{\var{census.data}}
#' must have the same value as the \code{\var{year}} option in the function (i.e., \code{"2010"}
#' in both or \code{"2020"} in both).
#' If \code{\var{census.data}} is missing, Census geographic data will be obtained via Census API.
#' @param retry The number of retries at the census website if network interruption occurs.
#' @return Output will be an object of class \code{data.frame}. It will
#'  consist of the original user-input data with additional columns of
#'  Census data.
#'
#' @examples
#' \dontshow{
#' data(voters)
#' }
#' \dontrun{
#' census_helper(key = "...", voter.file = voters, states = "nj", geo = "block")
#' }
#' \dontrun{
#' census_helper(
#'   key = "...", voter.file = voters, states = "all", geo = "tract",
#'   age = TRUE, sex = TRUE
#' )
#' }
#' \dontrun{
#' census_helper(
#'   key = "...", voter.file = voters, states = "all", geo = "county",
#'   age = FALSE, sex = FALSE, year = "2020"
#' )
#' }
#'
#' @export
census_helper <- function(key, voter.file, states = "all", geo = "tract", age = FALSE, sex = FALSE, year = "2010", census.data = NA, retry = 3, use_counties = FALSE) {
  if (is.na(census.data) || (typeof(census.data) != "list")) {
    toDownload <- TRUE
  } else {
    toDownload <- FALSE
  }

  if (toDownload) {
    if (missing(key)) {
      stop("Must enter U.S. Census API key, which can be requested at https://api.census.gov/data/key_signup.html.")
    }
  }

  states <- toupper(states)
  if (states == "ALL") {
    states <- toupper(as.character(unique(voter.file$state)))
  }

  df.out <- NULL

  for (s in 1:length(states)) {
    message(paste("State ", s, " of ", length(states), ": ", states[s], sep = ""))
    state <- toupper(states[s])

    if (geo == "place") {
      geo.merge <- c("place")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex) || (census.data[[state]]$year != year)) {
        census <- census_geo_api(key, state, geo = "place", age, sex, year, retry)
      } else {
        census <- census.data[[toupper(state)]]$place
      }
    }

    if (geo == "county") {
      geo.merge <- c("county")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex) || (census.data[[state]]$year != year)) {
        census <- census_geo_api(key, state, geo = "county", age, sex, year, retry)
      } else {
        census <- census.data[[toupper(state)]]$county
      }
    }

    if (geo == "tract") {
      geo.merge <- c("county", "tract")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex) || (census.data[[state]]$year != year)) {
        if (use_counties) {
          census <- census_geo_api(key, state, geo = "tract", age, sex, year, retry, 
                                   # Only those counties within the target state
                                   counties = unique(voter.file$county[voter.file$state == state]))
        } else {
          census <- census_geo_api(key, state, geo = "tract", age, sex, year, retry)
        }
      } else {
        census <- census.data[[toupper(state)]]$tract
      }
    }

    if (geo == "block") {
      geo.merge <- c("county", "tract", "block")
      if ((toDownload) || (is.null(census.data[[state]])) || (census.data[[state]]$age != age) || (census.data[[state]]$sex != sex) || (census.data[[state]]$year != year)) {
        if (use_counties) {
          census <- census_geo_api(key, state, geo = "block", age, sex, year, retry, 
                                   # Only those counties within the target state
                                   counties = unique(voter.file$county[voter.file$state == state]))
        } else {
          census <- census_geo_api(key, state, geo = "block", age, sex, year, retry)
        }
      } else {
        census <- census.data[[toupper(state)]]$block
      }
    }

    if (is.null(census) & use_counties) {
      message("No intersecting counties in counties supplied")
      return(NULL)
    }

    census$state <- state

    if (age == T) {
      ## Add Census Age Categories
      voter.file$agecat <- NA
      voter.file$agecat <- ifelse(voter.file$age <= 4, 1, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 5 & voter.file$age <= 9, 2, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 10 & voter.file$age <= 14, 3, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 15 & voter.file$age <= 17, 4, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 18 & voter.file$age <= 19, 5, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age == 20, 6, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age == 21, 7, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 22 & voter.file$age <= 24, 8, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 25 & voter.file$age <= 29, 9, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 30 & voter.file$age <= 34, 10, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 35 & voter.file$age <= 39, 11, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 40 & voter.file$age <= 44, 12, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 45 & voter.file$age <= 49, 13, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 50 & voter.file$age <= 54, 14, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 55 & voter.file$age <= 59, 15, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 60 & voter.file$age <= 61, 16, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 62 & voter.file$age <= 64, 17, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 65 & voter.file$age <= 66, 18, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 67 & voter.file$age <= 69, 19, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 70 & voter.file$age <= 74, 20, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 75 & voter.file$age <= 79, 21, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 80 & voter.file$age <= 84, 22, voter.file$agecat)
      voter.file$agecat <- ifelse(voter.file$age >= 85, 23, voter.file$agecat)
    }

    if (age == F & sex == F) {

      ## Calculate Pr(Geolocation | Race)
      if (year == "2010") {
        geoPopulations <- rowSums(census[, grepl("P00", names(census))])
        vars <- c(
          pop_white = "P005003", pop_black = "P005004",
          pop_aian = "P005005", pop_asian = "P005006",
          pop_nhpi = "P005007", pop_other = "P005008",
          pop_two = "P005009", pop_hisp = "P005010"
        )
        drop <- c(grep("state", names(census)), grep("P005", names(census)))
      } else if (year == "2020") {
        geoPopulations <- rowSums(census[, grepl("P2_", names(census))])
        vars <- c(
          pop_white = "P2_005N", pop_black = "P2_006N",
          pop_aian = "P2_007N", pop_asian = "P2_008N",
          pop_nhpi = "P2_009N", pop_other = "P2_010N",
          pop_two = "P2_011N", pop_hisp = "P2_002N"
        )
        drop <- c(grep("state", names(census)), grep("P2_", names(census)))
      }

      census$r_whi <- census[, vars["pop_white"]] / sum(census[, vars["pop_white"]]) # Pr(Geo | White)
      census$r_bla <- census[, vars["pop_black"]] / sum(census[, vars["pop_black"]]) # Pr(Geo | Black)
      census$r_his <- census[, vars["pop_hisp"]] / sum(census[, vars["pop_hisp"]]) # Pr(Geo | Latino)
      census$r_asi <- (census[, vars["pop_asian"]] + census[, vars["pop_nhpi"]]) / (sum(census[, vars["pop_asian"]]) + sum(census[, vars["pop_nhpi"]])) # Pr(Geo | Asian or NH/PI)
      census$r_oth <- (census[, vars["pop_aian"]] + census[, vars["pop_other"]] + census[, vars["pop_two"]]) / (sum(census[, vars["pop_aian"]]) + sum(census[, vars["pop_other"]]) + sum(census[, vars["pop_two"]])) # Pr(Geo | AI/AN, Other, or Mixed)

      voters.census <- merge(voter.file[toupper(voter.file$state) == toupper(states[s]), ], census[, -drop], by = geo.merge, all.x = T)
    }

    if (age == F & sex == T) {

      ## Calculate Pr(Geolocation, Sex | Race)
      eth.cen <- c("whi", "bla", "his", "asi", "oth")
      eth.let <- c("I", "B", "H", "D", "F")

      for (i in 1:length(eth.cen)) {
        if (i != 4 & i != 5) {
          census[paste("r_mal", eth.cen[i], sep = "_")] <- census[paste("P012", eth.let[i], "002", sep = "")] / sum(census[paste("P012", eth.let[i], "001", sep = "")])
          census[paste("r_fem", eth.cen[i], sep = "_")] <- census[paste("P012", eth.let[i], "026", sep = "")] / sum(census[paste("P012", eth.let[i], "001", sep = "")])
        }
        if (i == 4) {
          ## Combine Asian and Native Hawaiian/Pacific Islander
          census[paste("r_mal", eth.cen[i], sep = "_")] <- (census$P012D002 + census$P012E002) / sum(census$P012D001 + census$P012E001)
          census[paste("r_fem", eth.cen[i], sep = "_")] <- (census$P012D026 + census$P012E026) / sum(census$P012D001 + census$P012E001)
        }
        if (i == 5) {
          ## Combine American India/Alaska Native and Other
          census[paste("r_mal", eth.cen[i], sep = "_")] <- (census$P012C002 + census$P012F002) / sum(census$P012C001 + census$P012F001)
          census[paste("r_fem", eth.cen[i], sep = "_")] <- (census$P012C026 + census$P012F026) / sum(census$P012C001 + census$P012F001)
        }
      }

      voters.census <- merge(voter.file[toupper(voter.file$state) == toupper(states[s]), ], census[names(census) != "state"], by = geo.merge, all.x = T)
      for (i in 1:length(eth.cen)) {
        voters.census[voters.census$sex == 0, paste("r", eth.cen[i], sep = "_")] <-
          voters.census[voters.census$sex == 0, paste("r_mal", eth.cen[i], sep = "_")]
        voters.census[voters.census$sex == 1, paste("r", eth.cen[i], sep = "_")] <-
          voters.census[voters.census$sex == 1, paste("r_fem", eth.cen[i], sep = "_")]
      }
    }

    if (age == T & sex == F) {

      ## Calculate Pr(Geolocation, Age Category | Race)
      eth.cen <- c("whi", "bla", "his", "asi", "oth")
      eth.let <- c("I", "B", "H", "D", "F")
      age.cat <- c(seq(1, 23), seq(1, 23))
      age.cen <- as.character(c(c("03", "04", "05", "06", "07", "08", "09"), seq(10, 25), seq(27, 49)))

      for (i in 1:length(eth.cen)) {
        for (j in 1:23) {
          if (i != 4 & i != 5) {
            census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012", eth.let[i], "0", age.cen[j], sep = "")] + census[paste("P012", eth.let[i], "0", age.cen[j + 23], sep = "")]) / sum(census[paste("P012", eth.let[i], "001", sep = "")])
          }
          if (i == 4) {
            ## Combine Asian and Native Hawaiian/Pacific Islander
            census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012D0", age.cen[j], sep = "")] + census[paste("P012D0", age.cen[j + 23], sep = "")] + census[paste("P012E0", age.cen[j], sep = "")] + census[paste("P012E0", age.cen[j + 23], sep = "")]) / sum(census$P012D001 + census$P012E001)
          }
          if (i == 5) {
            ## Combine American India/Alaska Native and Other
            census[paste("r", age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012C0", age.cen[j], sep = "")] + census[paste("P012C0", age.cen[j + 23], sep = "")] + census[paste("P012F0", age.cen[j], sep = "")] + census[paste("P012F0", age.cen[j + 23], sep = "")]) / sum(census$P012C001 + census$P012F001)
          }
        }
      }

      voters.census <- merge(voter.file[toupper(voter.file$state) == toupper(states[s]), ], census[names(census) != "state"], by = geo.merge, all.x = T)
      for (i in 1:length(eth.cen)) {
        for (j in 1:23) {
          voters.census[voters.census$agecat == j, paste("r", eth.cen[i], sep = "_")] <-
            voters.census[voters.census$agecat == j, paste("r", j, eth.cen[i], sep = "_")]
        }
      }
    }

    if (age == T & sex == T) {

      ## Calculate Pr(Tract, Sex, Age Category | Race)
      eth.cen <- c("whi", "bla", "his", "asi", "oth")
      eth.let <- c("I", "B", "H", "D", "F")
      sex.let <- c("mal", "fem")
      age.cat <- c(seq(1, 23), seq(1, 23))
      age.cen <- as.character(c(c("03", "04", "05", "06", "07", "08", "09"), seq(10, 25), seq(27, 49)))

      for (i in 1:length(eth.cen)) {
        for (k in 1:length(sex.let)) {
          for (j in 1:23) {
            if (k == 2) {
              j <- j + 23
            }
            if (i != 4 & i != 5) {
              census[paste("r", sex.let[k], age.cat[j], eth.cen[i], sep = "_")] <- census[paste("P012", eth.let[i], "0", age.cen[j], sep = "")] / sum(census[paste("P012", eth.let[i], "001", sep = "")])
            }
            if (i == 4) {
              ## Combine Asian and Native Hawaiian/Pacific Islander
              census[paste("r", sex.let[k], age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012D0", age.cen[j], sep = "")] + census[paste("P012E0", age.cen[j], sep = "")]) / sum(census$P012D001 + census$P012E001)
            }
            if (i == 5) {
              ## Combine American India/Alaska Native and Other
              census[paste("r", sex.let[k], age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012C0", age.cen[j], sep = "")] + census[paste("P012F0", age.cen[j], sep = "")]) / sum(census$P012C001 + census$P012F001)
            }
          }
        }
      }

      voters.census <- merge(voter.file[toupper(voter.file$state) == toupper(states[s]), ], census[names(census) != "state"], by = geo.merge, all.x = T)
      for (i in 1:length(eth.cen)) {
        for (j in 1:23) {
          voters.census[
            voters.census$sex == 0 & voters.census$agecat == j,
            paste("r", eth.cen[i], sep = "_")
          ] <-
            voters.census[
              voters.census$sex == 0 & voters.census$agecat == j,
              paste("r_mal", j, eth.cen[i], sep = "_")
            ]
          voters.census[
            voters.census$sex == 1 & voters.census$agecat == j,
            paste("r", eth.cen[i], sep = "_")
          ] <-
            voters.census[
              voters.census$sex == 1 & voters.census$agecat == j,
              paste("r_fem", j, eth.cen[i], sep = "_")
            ]
        }
      }
    }

    keep.vars <- c(
      names(voter.file)[names(voter.file) != "agecat"],
      paste("r", c("whi", "bla", "his", "asi", "oth"), sep = "_")
    )
    df.out <- as.data.frame(rbind(df.out, voters.census[keep.vars]))
  }

  return(df.out)
}
