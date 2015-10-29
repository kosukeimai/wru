## Load names.RData and pid.RData
names <- load("data/names.RData")
pid <- load("data/pid.RData")

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
    source("Census.Helper.R")
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

df.out1 <- race.pred(voters = voters, 
                     vars = c("surname", "tract"), 
                     races = c("white", "black", "latino"), 
                     UScensus2010 = TRUE)

df.out2 <- race.pred(voters = voters, 
                     vars = c("surname", "tract", "party"), 
                     races = c("white", "black", "latino", "asian", "other"), 
                     UScensus2010 = TRUE)

df.out3 <- race.pred(voters = voters, 
                     vars = c("surname", "block"), 
                     races = c("white", "black", "latino", "asian", "other"), 
                     UScensus2010 = TRUE)

df.out4 <- race.pred(voters = voters, 
                     vars = c("surname", "block", "party"), 
                     races = c("white", "black", "latino", "asian", "other"), 
                     UScensus2010 = TRUE)
