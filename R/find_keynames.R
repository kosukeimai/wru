#' Obtain keynames from census table. Internal function
#'
#' @param names Character vector of potential keynames
#' @param freq_table Matrix with overall name ferquency, by race (columns)
#' @param obs_names Character vector of observed names
#' @param method Character string. Which method should be used to select keynames?
#'               Currently, the only implemented method is \code{mutual.inf}, which
#'               selects names with highest mutual information by race. 
#' @param max_keynames Maximum number of keynames to select by race. Defaults to 100. 
#'
#' @return A list of character vectors top \code{n_keynames} by race. 
#'
#' @keywords internal
.find_keynames <- function(names, 
                          freq_table,
                          obs_names,
                          method = "mutual.inf", 
                          max_keynames = 100)
{
  if(method == "mutual.inf"){
    joint_p <- freq_table/sum(freq_table)
    marginal_race <- proportions(freq_table, 2)
    marginal_name <- proportions(freq_table, 1)
    mi <- joint_p * log(joint_p/(marginal_race * marginal_name))
    key_mat <- apply(mi, 2, 
                 function(x)names[order(x)][1:max_keynames])
    races <- colnames(freq_table)
    r_key <- lapply(seq.int(ncol(key_mat)),
                    function(x){
                      x_ind <- key_mat[,x] %in% obs_names
                      if(all(!x_ind)){
                        stop(paste0("Not enough keywords found for race",
                                 races[x],
                                 "; increase max_keywords or revise voter.file"))
                      }
                      return(key_mat[x_ind,x])
                    })
    return(r_key)
  } else {
    stop("'mutual.inf' is the only method currently implemented.")
  }
}
