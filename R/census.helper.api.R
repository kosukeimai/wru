#' Census helper function.
#'
#' \code{census.helper.api} links user-input dataset with Census data.
#'
#' This function allows users to link their geocoded dataset (e.g., voter file) 
#' with U.S. Census 2010 data. The function extracts Census Summary File data 
#' at the tract or block level using the 'UScensus2010' package. Census data 
#' calculated are Pr(Geolocation | Race) where geolocation is tract or block.
#'
#' @param key A required character object. Must contain user's Census API
#'  key, which can be requested \href{http://api.census.gov/data/key_signup.html}{here}.
#' @param voters An object of class \code{data.frame}. Must contain field(s) 
#'  named \code{\var{county}}, \code{\var{tract}}, and/or \code{\var{block}} 
#'  that specifies geolocation.
#' @param states A character vector specifying which states to extract 
#'  Census data for, e.g. \code{c("NJ", "NY")}. Default is \code{"all"}, which extracts 
#'  Census data for all states contained in user-input data.
#' @param geo A character object specifying what aggregation level to use. 
#'  Use \code{"county"}, \code{"tract"}, or \code{"block"}. Default is \code{"tract"}. 
#'  Warning: extracting block-level data takes very long.
#' @param demo A \code{TRUE}/\code{FALSE} object indicating whether to condition on 
#'  demographics (i.e., age and sex) or not. If \code{TRUE}, function will return 
#'  Pr(Geolocation, Age, Sex | Race). If \code{FALSE}, function wil return 
#'  Pr(Geolocation | Race). Default is \code{FALSE}.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns of 
#'  Census data.
#'
#' @examples
#' \dontshow{data(voters)}
#' \dontrun{census.helper.api(key = "...", voters = voters, states = "nj", geo = "block")}
#' \dontrun{census.helper.api(key = "...", voters = voters, states = "all", geo = "tract", 
#' demo = TRUE)}
#'
#' @references
#' Relies on getCensusApi, getCensusApi2, and vecToChunk functions authored by Nicholas Nagle, 
#' available \href{http://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html}{here}.
#' 
#' @export
census.helper.api <- function(key, voters, states = "all", geo = "tract", demo = FALSE) {

  if (missing(key)) {
    stop('Must enter U.S. Census API key, which can be requested at http://api.census.gov/data/key_signup.html.')
  }

  states <- toupper(states)
  if (states == "ALL") {
    states <- as.character(unique(voters$state))
  }
  
  df.out <- NULL
  
  for (s in 1:length(states)) {
    
    print(paste("State ", s, " of ", length(states), ": ", states[s], sep = ""))
    fips.codes <- get("State.FIPS")
    state.fips <- fips.codes[fips.codes$State == states[s], "FIPS"]

    if (demo == F) {
      num <- ifelse(3:10 != 10, paste("0", 3:10, sep = ""), "10")
      vars <- paste("P00500", num, sep = "")
    }
    
    if (demo == T) {
      eth.let <- c("I", "B", "H", "D", "E", "F", "C")
      num <- as.character(c(c("01", "07", "08", "09"), seq(10, 25), seq(31, 49)))
      vars <- NULL
      for (e in 1:length(eth.let)) {
        vars <- c(vars, paste("P012", eth.let[e], "0", num, sep = ""))
      }
    }
    
    if (geo == "county") {
      geo.merge <- c("state", "county")
      region <- paste("for=county:*&in=state:", state.fips, sep = "")
      census <- getCensusApi("http://api.census.gov/data/2010/sf1?", 
                             key = key, vars = vars, region = region)
    }
    
    if (geo == "tract") {

      geo.merge <- c("state", "county", "tract")
      
      region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
      county_df <- getCensusApi("http://api.census.gov/data/2010/sf1?", key = key, vars = vars, region = region_county)
      county_list <- county_df$county
      
      census <- NULL
      for (c in 1:length(county_list)) {
        print(paste("County ", c, " of ", length(county_list), ": ", county_list[c], sep = ""))
        region_county <- paste("for=tract:*&in=state:", state.fips, "+county:", county_list[c], sep = "")
        census.temp <- getCensusApi("http://api.census.gov/data/2010/sf1?", 
                                    key = key, vars = vars, region = region_county)
        census <- rbind(census, census.temp)
      }
      rm(census.temp)
    }
    
    if (geo == "block") {
      
      geo.merge <- c("state", "county", "tract", "block")
      
      region_county <- paste("for=county:*&in=state:", state.fips, sep = "")
      county_df <- getCensusApi("http://api.census.gov/data/2010/sf1?", key = key, vars = vars, region = region_county)
      county_list <- county_df$county

      census <- NULL
      
      for (c in 1:length(county_list)) {
        print(paste("County ", c, " of ", length(county_list), ": ", county_list[c], sep = ""))
        
        region_tract <- paste("for=tract:*&in=state:", state.fips, "+county:", county_list[c], sep = "")
        print(region_tract)
        tract_df <- getCensusApi("http://api.census.gov/data/2010/sf1?", 
                                 key = key, vars = vars, region = region_tract)
        tract_list <- tract_df$tract

        for (t in 1:length(tract_list)) {
          print(paste("Tract ", t, " of ", length(tract_list), ": ", tract_list[t], sep = ""))
          
          region_block <- paste("for=block:*&in=state:", state.fips, "+county:", county_list[c], "+tract:", tract_list[t], sep = "")
          census.temp <- getCensusApi("http://api.census.gov/data/2010/sf1?", 
                                      key = key, vars = vars, region = region_block)
          census <- rbind(census, census.temp)
        }
      }
      rm(census.temp)
      
    }

    if (demo == F) {
      
      census$state <- states[s]
      
      ## Calculate Pr(Geolocation | Race)
      census$r_whi <- census$P0050003 / sum(census$P0050003) #Pr(Tract|White)
      census$r_bla <- census$P0050004 / sum(census$P0050004) #Pr(Tract|Black)
      census$r_his <- census$P0050010 / sum(census$P0050010) #Pr(Tract|Latino)
      census$r_asi <- (census$P0050006 + census$P0050007) / (sum(census$P0050006) + sum(census$P0050007)) #Pr(Tract | Asian or NH/PI)
      census$r_oth <- (census$P0050005 + census$P0050008 + census$P0050009) / (sum(census$P0050005) + sum(census$P0050008) + sum(census$P0050009)) #Pr(Tract | AI/AN, Other, or Mixed)
      
      drop <- grep("P005", names(census))
      voters.census <- merge(voters[voters$state == toupper(states[s]), ], census[, -drop], by = geo.merge, all.x  = T)
      
    }
    
    if (demo == T) {
      
      census$state <- states[s]
      
      ## Calculate Pr(Tract, Sex, Age Category | Race)
      eth.cen <- c("whi", "bla", "his", "asi", "oth")
      eth.let <- c("I", "B", "H", "D", "F")
      sex <- c("mal", "fem")
      age.cat <- c(seq(5, 23), seq(5, 23))
      age.cen <- as.character(c(c("07", "08", "09"), seq(10, 25), seq(31, 49)))
      
      for (i in 1:length(eth.cen)) {
        for (k in 1:length(sex)) {
          for (j in 1:19) {
            if (k == 2) {
              j <- j + 19
            }
            if (i != 4 & i != 5) {
              census[paste("r", sex[k], age.cat[j], eth.cen[i], sep = "_")] <- census[paste("P012", eth.let[i], "0", age.cen[j], sep = "")] / sum(census[paste("P012", eth.let[i], "001", sep = "")])
            }
            if (i == 4) {
              ## Combine Asian and Native Hawaiian/Pacific Islander
              census[paste("r", sex[k], age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012D0", age.cen[j], sep = "")] + census[paste("P012E0", age.cen[j], sep = "")]) / sum(census$P012D001 + census$P012E001)
            }
            if (i == 5) {
              ## Combine American India/Alaska Native and Other
              census[paste("r", sex[k], age.cat[j], eth.cen[i], sep = "_")] <- (census[paste("P012C0", age.cen[j], sep = "")] + census[paste("P012F0", age.cen[j], sep = "")]) / sum(census$P012C001 + census$P012F001)
            }
          }
        }
      }
      
      drop <- grep("P012", names(census))
      voters.census <- merge(voters[voters$state == toupper(states[s]), ], census[, -drop], by = geo.merge, all.x  = T)
      
      ## Add Census Age Categories
      voters.census$agecat <- NA
      voters.census$agecat <- ifelse(voters.census$age >= 18 & voters.census$age <= 19, 5, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age == 20, 6, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age == 21, 7, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 22 & voters.census$age <= 24, 8, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 25 & voters.census$age <= 29, 9, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 30 & voters.census$age <= 34, 10, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 35 & voters.census$age <= 39, 11, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 40 & voters.census$age <= 44, 12, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 45 & voters.census$age <= 49, 13, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 50 & voters.census$age <= 54, 14, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 55 & voters.census$age <= 59, 15, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 60 & voters.census$age <= 61, 16, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 62 & voters.census$age <= 64, 17, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 65 & voters.census$age <= 66, 18, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 67 & voters.census$age <= 69, 19, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 70 & voters.census$age <= 74, 20, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 75 & voters.census$age <= 79, 21, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 80 & voters.census$age <= 84, 22, voters.census$agecat)
      voters.census$agecat <- ifelse(voters.census$age >= 85, 23, voters.census$agecat)

      for (i in 1:length(eth.cen)) {
        for (j in 5:23) {
          voters.census[voters.census$sex == 0 & voters.census$agecat == j, 
                        paste("r", eth.cen[i], sep = "_")] <- voters.census[paste("r_mal", j, eth.cen[i], sep = "_")]
          voters.census[voters.census$sex == 1 & voters.census$agecat == j, 
                        paste("r", eth.cen[i], sep = "_")] <- voters.census[paste("r_fem", j, eth.cen[i], sep = "_")]
        }
      }
      
      drop <- c(grep("_mal_", names(voters.census)), grep("_fem_", names(voters.census)))
      voters.census <- voters.census[, -drop]
    }
  
  df.out <- as.data.frame(rbind(df.out, voters.census[names(voters.census) != "agecat"]))
  
  }
  
  return(df.out)
}
