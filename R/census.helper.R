#' Census helper function.
#'
#' \code{census.helper} links user-input dataset with Census data.
#'
#' This function allows users to link their geocoded dataset (e.g., voter file) 
#' with U.S. Census 2010 data. The function extracts Census data at the tract 
#' or block level using the 'UScensus2010' package. Census data calculated are 
#' Pr(geolocation | race) where geolocation is tract or block.
#'
#' @param voters An object of class \code{data.frame}. Must contain a field 
#'  named 'tract' or 'block' that specifies geolocation.
#' @param states A character vector specifying which states to extract 
#'  Census data for, e.g. c("NJ", "NY"). May also be "all" to extract 
#'  Census data for all states contained in user-input data.
#' @param geo A character object specifying what aggregation level to use. 
#'  Use "trt" for tract or "blk" for block. Warning: block takes very long.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns of 
#'  Census data.
#'
#' @examples
#' census.helper(NJ.voter.file, "nj", "blk")
#' census.helper(voters.all, c("nj", "ny"), "trt")
#'
#' @export
census.helper <- function(voters, states, geo) {
  require(UScensus2010)
  
  if (geo == "trt") {
    geo <- geo.name <- "tract"
    install.tract("linux")
  }

  if (geo == "blk") {
    geo.name <- "block"
    install.block("linux")
  }

  if (states == "All") {
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
