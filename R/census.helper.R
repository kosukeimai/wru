## Step 1: Download relevant data from Census Summary File
## Step 2: Compute Pr(Geolocation | Race) using raw counts
## Step 3: Compute Pr(Geolocation & Age & Sex | Race) using raw counts


## Note 1: 'voters' object must be voter file with state and geolocation specified.
## Note 2: Set states to "all" for all states or a subset of states in voter file.

census.helper <- function(voters, states, geo) {
  require(UScensus2010)
  
  if (geo == "trt") {
    geo <- "tract"
    install.tract("linux")
  }

  if (geo == "blk") {
    geo.name <- "block"
    install.block("linux")
  }

  if (states == "all") {
    states <- tolower(unique(voters$state))
  }
  
  df.out <- NULL
  for (k in 1:length(states)) {
  ## Calculate Pr(Geolocation | Race)
  for (i in 3:10) {
    j <- ifelse(i != 10, paste("0", i, sep = ""), "10")
    assign(paste(states[k], geo, i, sep = "."), demographics(dem = paste("P00500", j, sep = ""), state = states[k], level = geo))
  }
  
  state.k <- as.data.frame(mget(paste(states[k], geo, 3:10, sep = ".")))
  state.k$state <- toupper(states[k])
  state.k[geo.name] <- rownames(state.k)
  state.k <- state.k[c("state", geo.name, colnames(state.k)[1:8])]
  
  state.k$r_whi <- state.k$P0050003 / sum(state.k$P0050003) #Pr(Tract|White)
  state.k$r_bla <- state.k$P0050004 / sum(state.k$P0050004) #Pr(Tract|Black)
  state.k$r_his <- state.k$P0050010 / sum(state.k$P0050010) #Pr(Tract|Latino)
  state.k$r_asi <- (state.k$P0050006 + state.k$P0050007) / (sum(state.k$P0050006) + sum(state.k$P0050007)) #Pr(Tract | Asian or NH/PI)
  state.k$r_oth <- (state.k$P0050005 + state.k$P0050008 + state.k$P0050009) / (sum(state.k$P0050005) + sum(state.k$P0050008) + sum(state.k$P0050009)) #Pr(Tract | AI/AN, Other, or Mixed)
  assign(paste(states[k], geo, sep = "."), state.k[, -c(3:10)])
  
  assign(paste("voters", states[k], geo, sep = "."), merge(voters[voters$state == toupper(states[k]), ], state.k[, -c(3:10)], by = c("state", geo.name), all.x  = T))
  
  df.out <- as.data.frame(rbind(df.out, get(paste("voters", states[k], geo, sep = "."))))
  }
  
  return(df.out)
}


## NJ Tract-Level Race Counts by Age and Sex
eth.cen <- c("whi", "bla", "his", "asi", "npi", "aian", "oth", "mix")
eth.let <- c("I", "B", "H", "D", "E", "C", "F", "G")
sex <- c("male", "fem")
age.cat <- c(seq(5, 23), seq(5, 23))
age.cen <- as.character(c(c("07", "08", "09"), seq(10, 25), seq(31, 49)))

for (i in 1:length(eth.let)) {
  for (k in 1:length(sex)) {
    for (j in 1:23) {
      if (k == 2) {
        j <- j + 23
      }
      assign(paste("nj.trt.demo.", eth.let[i], age.cen[j], sep = ""), 
             demographics(dem = paste("P012", eth.let[i], "0", age.cen[j], sep = ""), state = "nj", level = c("tract")))
    }
  }
}

test <- demographics(dem = "P012H007", state = "nj", level = c("tract"))
