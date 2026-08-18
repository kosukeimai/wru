#' Legacy data formatting function.
#'
#' \code{format_legacy_data} formats legacy data from the U.S. census to allow
#' for Bayesian name geocoding. 
#'
#' This function allows users to construct datasets for analysis using the census legacy data format.
#' These data are available for the 2020 census at
#' https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/.
#' It is an offline alternative to \code{\link{get_census_data}}, which reads the same
#' 2020 redistricting counts from the Census API and needs an API key.
#'
#' @param legacyFilePath A character vector giving the location of a legacy census data folder,
#' sourced from https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/.
#' These file names should end in ".pl".
#' @param state The two letter state postal code.
#' @param outFile Optional character vector determining whether the formatted RData object should be saved. The
#' filepath should end in ".RData".
#'
#' @return A list with the same structure as \code{\link{get_census_data}} returns:
#' one element named for \code{\var{state}}, holding \code{state}, \code{age},
#' \code{sex} and \code{year} plus a \code{county}, \code{tract},
#' \code{block_group} and \code{block} data frame. Population counts carry the
#' 2020 redistricting column names (\code{P2_002N} and \code{P2_005N} through
#' \code{P2_011N}).
#'
#' @import PL94171
#'
#' @examples
#' \dontrun{
#' gaCensusData <- format_legacy_data(PL94171::pl_url('GA', 2020), state = 'GA')
#' predict_race(ga.voter.file, names.to.use = 'surname, first', census.geo = 'block',
#'      census.data = gaCensusData, year = '2020')
#'}
#'
#' @export
format_legacy_data <- function(legacyFilePath, state, outFile = NULL) {
  
  # aggregation levels to convert (county, tract, block group, and block)
  summaryLevels <- c('050', '140', '150', '750')
  
  # read in the data
  pl <- pl_read(legacyFilePath)
  pl <- pl_select_standard(pl) 
  
  # iterate through the levels
  geoLevels <- lapply(summaryLevels, FUN = function(level) {
    levelData <- pl[pl$summary_level == level,]

    # construct the base data frame, using the 2020 redistricting (PL 94-171)
    # column names the rest of the package expects for year 2020
    df <- data.frame(state = toupper(state),
                     county = levelData$county,
                     P2_005N = levelData$pop_white,
                     P2_006N = levelData$pop_black,
                     P2_002N = levelData$pop_hisp,
                     P2_008N = levelData$pop_asian,
                     P2_009N = levelData$pop_nhpi,
                     P2_007N = levelData$pop_aian,
                     P2_010N = levelData$pop_other,
                     P2_011N = levelData$pop_two)

    # add geographic levels. The GEOID carries a summary level prefix ending in
    # "US", followed by state (2), county (3), tract (6) and block (4) digits,
    # so read the codes from the left of that FIPS string.
    fips <- sub('^.*US', '', levelData$GEOID)
    if(level != '050') {
      df$tract <- substr(fips, 6, 11)
      if(level == '150')
        df$block_group <- substr(fips, 12, 12)
      if(level == '750')
        df$block <- substr(fips, 12, 15)
    }

    df
  })
  names(geoLevels) <- c('county', 'tract', 'block_group', 'block')

  # nest under the state, matching what get_census_data() returns
  censusData.2020 <- list(
    c(
      list(state = toupper(state), age = FALSE, sex = FALSE, year = "2020"),
      geoLevels
    )
  )
  names(censusData.2020) <- toupper(state)

  # optionally save the file
  if(!is.null(outFile))
    save(censusData.2020, file = outFile)

  # return the object
  return(censusData.2020)
}
