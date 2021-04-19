.find_keynames <- function(names, 
                          freq_table,
                          method, 
                          n_keynames)
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
