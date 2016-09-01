#' Census API URL assembler.
#'
#' \code{getCensusApi2} assembles URL components for \code{getCensusApi}.
#' 
#' This function assembles the URL components and sends the request to the Census server. 
#' It is used by the \code{getCensusApi} function. The user should not need to call this 
#' function directly.
#'
#' @param data_url URL root of the API, including the question mark, 
#'  e.g., \code{"http://api.census.gov/data/2010/sf1?"}.
#' @param key A required character object containing user's Census API key, 
#'  which can be requested \href{http://api.census.gov/data/key_signup.html}{here}.
#' @param get A character vector of variables to get, 
#'  e.g., \code{c("P0050003","P0050004","P0050005", "P0050006")}.
#'  If there are more than 50 variables, then function will automatically 
#'  split variables into separate queries.
#' @param region Character object specifying which region to obtain data for.
#'  Must contain "for" and possibly "in", 
#'  e.g., \code{"for=block:1213&in=state:47+county:015+tract:*"}.
#' @return If successful, output will be an object of class \code{data.frame}. 
#'  If unsuccessful, function prints the URL query that was constructed.
#'
#' @examples
#' \dontrun{getCensusApi2(data_url = "http://api.census.gov/data/2010/sf1?", key = "...", 
#' get = c("P0050003","P0050004","P0050005", "P0050006"), region = "for=county:*&in=state:34")}
#' 
#' @references
#' Based on code authored by Nicholas Nagle, which is available 
#' \href{http://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html}{here}.
#'
#' @export
getCensusApi2 <- function(data_url, key, get, region){
  if(length(get)>1) get <- paste(get, collapse=',', sep='')
  api_call <- paste(data_url, 
                    'key=', key, 
                    '&get=', get,
                    '&', region,
                    sep='')

  dat_raw <- try(readLines(api_call, warn="F"))
  if(class(dat_raw) == 'try-error') {
    print(api_call)
    return
    }
  dat_df <- data.frame()

  ## Split the datastream into a list with each row as an element.
  tmp <- strsplit(gsub("[^[:alnum:], _]", '', dat_raw), "\\,")
  dat_df <- as.data.frame(do.call(rbind, tmp[-1]), stringsAsFactors=FALSE)
  names(dat_df) <- tmp[[1]]
  ## Convert to numeric
  value_cols <- grep("[0-9]", names(dat_df), value=TRUE)
  for(col in value_cols) dat_df[,col] <- as.numeric(as.character(dat_df[,col]))
  return(dat_df)
}
