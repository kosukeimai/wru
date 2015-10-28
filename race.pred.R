## Create Fake Voter File
VoterID <- seq(1:10)
surname <- c("Khanna", "Imai", "Velasco", "Fifield", "Zhou", "Ratkovic", "Johnson", "Lopez", "Wantchekon", "Morse")
state <- c("NJ", "NJ", "NY", "NJ", "NJ", "NJ", "NY", "NJ", "NJ", "DC")
CD <- c(12, 12, NA, 12, 12, 12, NA, 12, 12, NA)
county <- c("34021", "34021", "", "34021", "34021", "34021", "", "34021", "34021", "")
tract <- c("34021004000", "34021004501", "34021004501", "34021004501", "34021004501", "34021004501", "34021004000", "34021004501", "34021004501", "")
block <- c("3001", "1025", "", "1025", "1025", "1025", "", "1025", "1025", "")
precinct <- c("6", "", "", "", "", "", "", "", "", "")
age <- c(29, 40, 33, 27, 28, 35, 25, 33, 50, 29)
female <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
party <- c("Ind", "Dem", "Rep", "Dem", "Dem", "Ind", "Dem", "Rep", "Rep", "Rep")

voters <- as.data.frame(cbind(VoterID, surname, state, CD, county, tract, block, precinct, age, female, party))


## Race Prediction Function
race.pred <- function(voters, vars, races, UScensus2010) {
  
  ## Merge in Pr(Race | Surname) -- no surname cleaning yet
  voters$surname <- toupper(voters$surname)
  voters <- merge(voters, names, by = "surname", all.x = T)
  voters$p_whi <- ifelse(is.na(voters$p_whi), .621, voters$p_whi)
  voters$p_bla <- ifelse(is.na(voters$p_bla), .132, voters$p_bla)
  voters$p_his <- ifelse(is.na(voters$p_his), .174, voters$p_his)
  voters$p_asi <- ifelse(is.na(voters$p_asi), .054, voters$p_asi)
  voters$p_oth <- ifelse(is.na(voters$p_oth), .019, voters$p_oth)
  
  ## Merge in Pr(Party | Race)
  voters <- merge(voters, pid, by = "party", all.x = T) 
    
  if ("block" %in% vars) {
    geo <- "blk"
  }

  if ("precinct" %in% vars) {
    geo <- "prc"
    if (UScensus2010 == TRUE) {
      stop('Error: precinct-level data currently unavailable using UScensus2010.')
    }
  }

  if ("tract" %in% vars) {
    geo <- "trt"
  }
  
  if (UScensus2010 == TRUE) {
    #source("Census.Helper.R")
    warning("Extracting U.S. Census 2010 data using UScensus2010 -- may take a long time!")
    voters <- census.helper(voters = voters, states = "all", geo = geo)
  }
  
  ## Subset user-specified races (maximum of five)
  eth <- c("whi", "bla", "his", "asi", "oth")[c("white", "black", "latino", "asian", "other") %in% races]
  
  voters.temp <- voters
  
  ## Pr(Race | Surname, Geolocation)
  for (k in 1:length(eth)) {
    voters.temp[paste("u1", eth[k], sep = "_")] <- voters.temp[paste("p", eth[k], sep = "_")] * voters.temp[paste("r", eth[k], sep = "_")]
  }
  voters.temp$u1_tot <- apply(voters.temp[paste("u1", eth, sep = "_")], 1, sum, na.rm = T)
  for (k in 1:length(eth)) {
    voters.temp[paste("q1", eth[k], sep = "_")] <- voters.temp[paste("u1", eth[k], sep = "_")] / voters.temp$u1_tot
  }    
  
  ## Pr(Race | Surname, Geolocation, Party)
  for (k in 1:length(eth)) {
    voters.temp[paste("u2", eth[k], sep = "_")] <- voters.temp[paste("p", eth[k], sep = "_")] * voters.temp[paste("r", eth[k], sep = "_")] * voters.temp[paste("r_pid", eth[k], sep = "_")]
  }
  voters.temp$u2_tot <- apply(voters.temp[paste("u2", eth, sep = "_")], 1, sum, na.rm = T)
  for (k in 1:length(eth)) {
    voters.temp[paste("q2", eth[k], sep = "_")] <- voters.temp[paste("u2", eth[k], sep = "_")] / voters.temp$u2_tot
  }
  
  if ("party" %in% vars == F) {
    for (k in 1:length(eth)) {
      voters[paste("pred", eth[k], sep = ".")] <- voters.temp[paste("q1", eth[k], sep = "_")]
    }
    pred <- paste("pred", eth, sep = ".")
  }
  
  if ("party" %in% vars == T) {
    for (k in 1:length(eth)) {
      voters[paste("pred", eth[k], sep = ".")] <- voters.temp[paste("q2", eth[k], sep = "_")]
    }
    pred <- paste("pred", eth, sep = ".")
  }
  
  return(voters)
}


## Examples
df.out1 <- race.pred(voters = voters, 
                     vars = c("surname", "tract"), 
                     races = c("white", "black", "latino"), 
                     UScensus2010 = TRUE)

df.out2 <- race.pred(voters = voters, 
                     vars = c("surname", "tract", "party"), 
                     races = c("white", "black", "latino", "asian", "other"), 
                     UScensus2010 = TRUE)
