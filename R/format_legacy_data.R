#' Legacy data formatting function.
#'
#' \code{format_legacy_data} formats legacy data from the U.S. census to allow
#' for Bayesian name geocoding. 
#'
#' This function allows users to construct datasets for analysis using the census legacy data format. 
#' These data are available for the 2020 census at 
#' https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/. 
#' This function returns data structured analogously to data from the Census API, which is not yet 
#' available for the 2020 Census as of September 2021. 
#'
#' @param legacyFilePath A character vector giving the location of a legacy census data folder,
#' sourced from https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/. 
#' These file names should end in ".pl". 
#' @param outFile Optional character vector determining whether the formatted RData object should be saved. The
#' filepath should end in ".RData". 
#'
#' @import PL94171
#' @importFrom dplyr `%>%` .data
#' 
#' @examples
#' \dontrun{
#' gaCensusData <- format_legacy_data(PL94171::pl_url('GA', 2020))
#' predict_race_new(ga.voter.file, namesToUse = 'last, first, mid', census.geo = 'block',
#'      census.data = gaCensusData)
#' }
#'
#' @export
format_legacy_data <- function(legacyFilePath, outFile = NULL) {
  
  # aggregation levels to convert (county, tract, block group, and block)
  summaryLevels <- c('050', '140', '150', '750')
  
  # read in the data
  pl <- PL94171::pl_read(legacyFilePath)
  pl <- PL94171::pl_select_standard(pl) 
  
  # iterate through the levels 
  censusData.2020 <- lapply(summaryLevels, FUN = function(level) {
    levelData <- PL94171::pl_subset(pl, level)
    
    # construct the base data frame
    df <- levelData %>% 
      dplyr::select(GEOID = .data$GEOID, 
                    state = toupper(.data$state), 
                    county = .data$county,
                    P005003 = .data$pop_white, 
                    P005004 = .data$pop_black, 
                    P005010 = .data$pop_hisp,
                    P005006 = .data$pop_asian,
                    P005007 = .data$pop_nhpi,
                    P005005 = .data$pop_aian, 
                    P005008 = .data$pop_other, 
                    P005009 = .data$pop_two
      )
    
    # add geographic levels
    if(level != '050') {
      df <- df %>% dplyr::mutate(tract = substr(.data$GEOID, nchar(.data$GEOID) - 5, nchar(.data$GEOID)))
      if(level != '140') {
        df <- df %>% dplyr::mutate(blockGroup = substr(.data$GEOID, nchar(.data$GEOID), nchar(.data$GEOID)))
        if(level != '150') {
          df <- df %>% dplyr::mutate(block = substr(.data$GEOID, nchar(.data$GEOID) - 2, nchar(.data$GEOID)))
        }
      }
    }
    
    df
  })
  
  # format and optionally save the file 
  names(censusData.2020) <- c('county', 'tract', 'blockGroup', 'block')
  if(!is.null(outFile))
    save(censusData.2020, file = outFile)
  
  # return the object
  return(censusData.2020)
}
