#' Obtain keynames from census table. Internal function
#'
#' @param names Character vector of potential keynames
#' @param freq_table Matrix with overall name ferquency, by race (columns)
#' @param method Character string. Which method should be used to select keynames?
#'               Currently, the only implemented method is \code{mutual.inf}, which
#'               selectes names with highest mutual information by race. 
#' @param n_keynames Number of keynames to select by race. Defaults to 100. 
#'
#' @return A character matrix of top \code{n_keynames} by race. 
#'
#' @keywords internal
.find_keynames <- function(names, 
                          freq_table,
                          method = "mutual.inf", 
                          n_keynames = 100)
{
  if(method == "mutual.inf"){
    joint_p <- freq_table/sum(freq_table)
    marginal_race <- proportions(freq_table, 2)
    marginal_name <- proportions(freq_table, 1)
    mi <- joint_p * log(joint_p/(marginal_race * marginal_name))
    return(apply(mi, 2, 
                 function(x)names[order(x, decreasing = FALSE)[1:n_keynames]]))
  } else {
    stop("'mutual.inf' is the only method currently implemented.")
  }
}
