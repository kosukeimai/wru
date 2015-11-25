# getCensusApi
# get Census data via the public API: loop through variables if needed
# Inputs:
#   data_url: the url root of the api, including the '?'
#     example: http://api.census.gov/data/2010/sf1?
#   key: your API key
#   vars: a character vector of variables to get.
#     example c("H0110001","H0110002","H0110003")
#     If there are more than 50, then it will be automatically split into separate queries.
#   region: region to get data for.  contains a for:, and possibly an in:
#     example: for=block:1213&in=state:47+county:015+tract:*
# Output:
#   If successful, a data.frame
#   If unsuccessful, prints the url query that caused the error.

#' get Census data via the public API: loop through variables if needed
#'
#' @examples
#' getCensusApi(data_url = "http://api.census.gov/data/2010/sf1?", key = "", region = "")
#'
#' @export
getCensusApi <- function(data_url, key, vars, region, numeric = TRUE) {
  if(length(vars)>50){
    vars <- vecToChunk(vars) # Split vars into a list
    get <- lapply(vars, function(x) paste(x, sep='', collapse=","))
    data <- lapply(vars, function(x) getCensusApi2(data_url,key, x, region, numeric=TRUE))
    } else {
      get <- paste(vars, sep='', collapse=',')
      data <- list(getCensusApi2(data_url,key, get, region, numeric=TRUE))
      }
  # Format output.  If there were no errors, than paste the data together
  # If there is an error, just return the unformatted list.
  if(all(sapply(data, is.data.frame))){
    colnames <- unlist(lapply(data, names))
    data <- do.call(cbind,data)
    names(data) <- colnames
    # Prettify the output
    # If there are nonunique colums, remove them
    data <- data[,unique(colnames, fromLast=TRUE)]
    # Reorder columns so that numeric fields follow non-numeric fields
    data <- data[,c(which(sapply(data, class)!='numeric'), which(sapply(data, class)=='numeric'))]
    return(data)
  }else{
    print('unable to create single data.frame in getCensusApi')
    return(data)
    }
}
