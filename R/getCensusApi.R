#' Census API function.
#'
#' \code{getCensusApi} obtains U.S. Census data via the public API.
#' 
#' This function obtains U.S. Census data via the public API. User 
#' can specify the variables and region(s) for which to obtain data.
#'
#' @param data_url URL root of the API, including the question mark, 
#'  e.g., \code{"http://api.census.gov/data/2010/sf1?"}.
#' @param key A required character object containing user's Census API key, 
#'  which can be requested \href{http://api.census.gov/data/key_signup.html}{here}.
#' @param vars A character vector of variables to get, 
#'  e.g., \code{c("P0050003","P0050004","P0050005", "P0050006")}.
#'  If there are more than 50 variables, then function will automatically 
#'  split variables into separate queries.
#' @param region Character object specifying which region to obtain data for.
#'  Must contain "for" and possibly "in", 
#'  e.g., \code{"for=block:1213&in=state:47+county:015+tract:*"}.
#' @return If successful, output will be an object of class \code{data.frame}. 
#'  If unsuccessful, function prints the URL query that caused the error.
#'
#' @examples
#' \dontrun{getCensusApi(data_url = "http://api.census.gov/data/2010/sf1?", key = "...", 
#' vars = c("P0050003","P0050004","P0050005", "P0050006"), region = "for=county:*&in=state:34")}
#'
#' @references
#' Based on code authored by Nicholas Nagle, which is available 
#' \href{http://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html}{here}.
#'
#' @export
getCensusApi <- function(data_url, key, vars, region) {
  if(length(vars) > 50){
    vars <- vecToChunk(vars) # Split variables into a list
    get <- lapply(vars, function(x) paste(x, sep='', collapse=","))
    data <- lapply(vars, function(x) getCensusApi2(data_url,key, x, region))
    }
  else {
      get <- paste(vars, sep='', collapse=',')
      data <- list(getCensusApi2(data_url, key, get, region))
      }
  ## Format output. If there were no errors, than paste the data together. If there is an error, just return the unformatted list.
  if(all(sapply(data, is.data.frame))){
    colnames <- unlist(lapply(data, names))
    data <- do.call(cbind,data)
    names(data) <- colnames
    ## Prettify the output and remove any non-unique columns
    data <- data[,unique(colnames, fromLast=TRUE)]
    ## Reorder columns so that numeric fields follow non-numeric fields
    data <- data[,c(which(sapply(data, class)!='numeric'), which(sapply(data, class)=='numeric'))]
    return(data)
  }
  else{
    print('unable to create single data.frame in getCensusApi')
    return(data)
    }
}
