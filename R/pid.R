#' Party and race
#'
#' A table for probability of party by race.
#'
#' @format A data frame with 3 rows and 7 variables:
#' \describe{
#'   \item{party}{Dem/Rep/Ind}
#'   \item{PID}{0/1/2}
#'   \item{r_pid_whi}{Pr(PID | White)}
#'   \item{r_pid_bla}{Pr(PID | Black)}
#'   \item{r_pid_his}{Pr(PID | Hispanic/Latino)}
#'   \item{r_pid_asi}{Pr(PID | Asian/Pacific Islander)}
#'   \item{r_pid_oth}{Pr(PID | Other)}
#'   #' }
#'
#' @docType data
#' @keywords datasets
#' @name pid
#' @examples
#' data(pid)
"pid"
