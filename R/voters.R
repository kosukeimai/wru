#' Example voter file.
#'
#' An example dataset containing voter file information.
#'
#' @format A data frame with 10 rows and 12 variables:
#' \describe{
#'   \item{VoterID}{Voter identifier (numeric)}
#'   \item{surname}{Surname}
#'   \item{state}{State of residence}
#'   \item{CD}{Congressional district}
#'   \item{county}{Census county (three-digit code)}
#'   \item{tract}{Census tract (six-digit code)}
#'   \item{block}{Census block (four-digit code)}
#'   \item{precinct}{Voting precinct}
#'   \item{age}{Age in years}
#'   \item{sex}{0=male, 1=female}
#'   \item{party}{Party registration (character)}
#'   \item{PID}{Party registration (numeric)}
#'   #' }
"voters"
