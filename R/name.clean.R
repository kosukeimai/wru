#' Name cleaning and matching function.
#'
#' \code{name.clean} cleans surnames in user-input dataset and merges in racial 
#'  distributions from the Census Surname List and Census Spanish Surname List.
#'
#' This function allows users to match surnames in their dataset with the U.S. 
#'  Census 2000 Surname List to obtain Pr(Race | Surname) for each of the 
#'  five major racial groups. The function matches user-input surnames with 
#'  Census surnames as follows (each step only applies to surnames not matched 
#'  in previous steps): 
#'  1) match raw surnames with Census data; 
#'  2) remove any spaces and search again; 
#'  3) split apart double-barreled surnames into two names and match on first; 
#'  4) split apart double-barreled surnames into two names and match on second; 
#'  5) for any remaining names, impute probabilities from overall U.S. population.
#'  Note: Any name appearing only on the Spanish Surname List is assigned a 
#'  probability of 1 for Hispanics/Latinos and 0 for all other racial groups.
#'
#' @param voters An object of class \code{data.frame}. Must contain a field 
#'  named 'surname'.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns that 
#'  specify the part of the name matched with Census data (\code{\var{surname.match}}), 
#'  and the probabilities Pr(Race | Surname) for each racial group 
#'  (\code{\var{p_whi}} for Whites, \code{\var{p_bla}} for Blacks, 
#'  \code{\var{p_his}} for Hispanics/Latinos, \code{\var{p_asi}} for Asians, and 
#'  \code{\var{p_oth}} for Others).
#'
#' @import devtools
#'
#' @examples
#' data(voters)
#' name.clean(voters)
#'
#' @export
name.clean <- function(voters) {

  if ("surname" %in% names(voters) == F) {
    stop('Data does not contain surname field.')
  }
  names.all$surname <- as.character(names.all$surname)
  
  p_eth <- c("p_whi", "p_bla", "p_his", "p_asi", "p_oth")
  
  ## Convert Surnames to Upper Case 
  df1 <- voters
  df1$surname.upper <- df1$surname.match <- toupper(as.character(df1$surname))

  ## Merge Surname Priors (No Cleaning Yet)
  df2 <- merge(df1[names(df1) %in% p_eth == F], names.all[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)
  
  ## Remove Spaces and Merge Again
  df2$surname.match <- gsub(" ","", df2$surname.upper)
  df3 <- merge(df2[names(df2) %in% p_eth == F], names.all[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)
  
  ## Names with Hyphens or Spaces, e.g. Double-Barreled Names
  df3$nomatch <- 0
  if (nrow(df3[df3$surname.upper %in% names.all$surname == F, ]) > 0) {
    df3[df3$surname.upper %in% names.all$surname == F, ]$nomatch <- 1
  }
  df3$surname1 <- NA
  df3$surname2 <- NA
  df3[df3$nomatch == 1, ]$surname1[grep("-", df3[df3$nomatch == 1, ]$surname.upper)] <- sapply(strsplit(grep("-", df3$surname.upper, value = T), "-"), "[", 1)
  df3[df3$nomatch == 1, ]$surname2[grep("-", df3[df3$nomatch == 1, ]$surname.upper)] <- sapply(strsplit(grep("-", df3$surname.upper, value = T), "-"), "[", 2)  
  df3[df3$nomatch == 1, ]$surname1[grep(" ", df3[df3$nomatch == 1, ]$surname.upper)] <- sapply(strsplit(grep(" ", df3$surname.upper, value = T), " "), "[", 1)
  df3[df3$nomatch == 1, ]$surname2[grep(" ", df3[df3$nomatch == 1, ]$surname.upper)] <- sapply(strsplit(grep(" ", df3$surname.upper, value = T), " "), "[", 2)

  ## Use first half of name to merge in priors
  df3[df3$nomatch == 1, ]$surname.match <- as.character(df3[df3$nomatch == 1 , ]$surname1)
  df3[df3$nomatch == 1, ] <- merge(df3[df3$nomatch == 1, names(df3) %in% p_eth == F], names.all[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)[names(df3)]
  df3$nomatch <- 0
  if (nrow(df3[df3$surname.match %in% names.all$surname == F, ]) > 0) {
    df3[df3$surname.match %in% names.all$surname == F, ]$nomatch <- 1
  }
  
  ## Use second half of name to merge in priors for rest
  df3[df3$nomatch == 1, ]$surname.match <- as.character(df3[df3$nomatch == 1 , ]$surname2)
  df3[df3$nomatch == 1, ] <- merge(df3[df3$nomatch == 1, names(df3) %in% p_eth == F], names.all[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)[names(df3)]
  df3$nomatch <- 0
  if (nrow(df3[df3$surname.match %in% names.all$surname == F, ]) > 0) {
    df3[df3$surname.match %in% names.all$surname == F, ]$nomatch <- 1
  }

  ## Impute priors for names not on Census 2000 surname list or Spanish surname list
  if (nrow(df3[df3$nomatch == 1, ]) > 0) {
    df3[df3$nomatch == 1, ]$p_whi <- .621 #.705
    df3[df3$nomatch == 1, ]$p_bla <- .132 #.113
    df3[df3$nomatch == 1, ]$p_his <- .174 #.111
    df3[df3$nomatch == 1, ]$p_asi <- .054 #.070
    df3[df3$nomatch == 1, ]$p_oth <- .019 #(neg)
  }
  
  return(df3[c(names(voters), "surname.match", p_eth)])
}
