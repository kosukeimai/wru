#' Census API URL assembler.
#'
#' \code{get_census_api_2} assembles URL components for \code{get_census_api}.
#' 
#' This function assembles the URL components and sends the request to the Census server. 
#' It is used by the \code{get_census_api} function. The user should not need to call this 
#' function directly.
#'
#' @param data_url URL root of the API,
#'  e.g., \code{"https://api.census.gov/data/2020/dec/pl"}.
#' @param key A required character object containing user's Census API key, 
#'  which can be requested \href{https://api.census.gov/data/key_signup.html}{here}.
#' @param get A character vector of variables to get, 
#'  e.g., \code{c("P2_005N", "P2_006N", "P2_007N", "P2_008N")}.
#'  If there are more than 50 variables, then function will automatically 
#'  split variables into separate queries.
#' @param region Character object specifying which region to obtain data for.
#'  Must contain "for" and possibly "in", 
#'  e.g., \code{"for=block:1213&in=state:47+county:015+tract:*"}.
#' @param retry The number of retries at the census website if network interruption occurs.
#' @return If successful, output will be an object of class \code{data.frame}. 
#'  If unsuccessful, function prints the URL query that was constructed.
#'
#' @examples
#' \dontrun{try(get_census_api_2(data_url = "https://api.census.gov/data/2020/dec/pl", key = "...", 
#' get = c("P2_005N", "P2_006N", "P2_007N", "P2_008N"), region = "for=county:*&in=state:34"))}
#' 
#' @references
#' Based on code authored by Nicholas Nagle, which is available 
#' \href{https://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html}{here}.
#'
#' @keywords internal
get_census_api_2 <- function(data_url, key, get, region, retry = 3){
  if(length(get) > 1) {
    get <- paste(get, collapse=',', sep='')
  }
  
  api_call <- paste(data_url, 'key=', key, '&get=', get, '&', region, sep='')
  
  dat_raw <- try(readLines(api_call, warn="F"))

  while (inherits(dat_raw, "try-error") && (retry > 0)) {
    message(paste("Try census server again:", data_url))
    Sys.sleep(1)
    retry <- retry - 1
    dat_raw <- try(readLines(api_call, warn="F"))
  }

  if (inherits(dat_raw, "try-error")) {
    message("Data access failure at the census website, please try again by re-run the previous command")
    stop(message(api_call))
    return()
  }
  if (!inherits(dat_raw, "try-error") & "TRUE" %in% names(table(grepl("Invalid Key", dat_raw)))) {
    stop('Invalid Key: 
         A valid key must be included with each data API request. 
         You included a key with this request, however, it is not valid. 
         Please check your key and try again.'
         )
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
