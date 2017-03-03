#' Surname probability merging function.
#'
#' \code{merge_surnames} merges surnames in user-input dataset with corresponding 
#'  race/ethnicity probabilities from U.S. Census Surname List and Spanish Surname List.
#'
#' This function allows users to match surnames in their dataset with the U.S. 
#'  Census Surname List (from 2000 or 2010) and Spanish Surname List to obtain 
#'  Pr(Race | Surname) for each of the five major racial groups.
#'  
#'  By default, the function matches surnames to the Census list as follows 
#'  (each step only applies to surnames not matched in previous steps): 
#'  1) Search raw surnames in Census surname list; 
#'  2) Remove any punctuation and search again; 
#'  3) Remove any spaces and search again; 
#'  4) Remove suffixes (e.g., Jr) and search again; 
#'  5) Split double-barreled surnames into two parts and search first part of name; 
#'  6) Split double-barreled surnames into two parts and search second part of name; 
#'  7) For any remaining names, impute probabilities using distribution 
#'  for all names not appearing on Census list.
#'  
#'  Note: Any name appearing only on the Spanish Surname List is assigned a 
#'  probability of 1 for Hispanics/Latinos and 0 for all other racial groups.
#'
#' @param voter.file An object of class \code{data.frame}. Must contain a field 
#'  named 'surname' containing list of surnames to be merged with Census lists.
#' @param surname.year An object of class \code{numeric} indicating which year 
#'  Census Surname List is from. Accepted values are \code{2010} and \code{2000}. 
#'  Default is \code{2010}.
#' @param clean.surname A \code{TRUE}/\code{FALSE} object. If \code{TRUE}, 
#' \code{clean.surname} function will be run to clean raw surnames in 
#' \code{\var{voter.file}} before matching them with Census lists, 
#' in order to increase the chance of finding a match. 
#' See \code{clean.surname} documentation for details.
#' Default is \code{TRUE}.
#' @param impute.missing A \code{TRUE}/\code{FALSE} object. If \code{TRUE}, 
#' race/ethnicity probabilities will be imputed for unmatched names using  
#' race/ethnicity distribution for all other names (i.e., not on Census List).
#' Default is \code{TRUE}.
#' @return Output will be an object of class \code{data.frame}. It will 
#'  consist of the original user-input data with additional columns that 
#'  specify the part of the name matched with Census data (\code{\var{surname.match}}), 
#'  and the probabilities Pr(Race | Surname) for each racial group 
#'  (\code{\var{p_whi}} for White, \code{\var{p_bla}} for Black, 
#'  \code{\var{p_his}} for Hispanic/Latino, 
#'  \code{\var{p_asi}} for Asian and Pacific Islander, and 
#'  \code{\var{p_oth}} for Other/Mixed).
#'
#' @import devtools
#'
#' @examples
#' data(voters)
#' merge_surnames(voters)
#'
#' @export
merge_surnames <- function(voter.file, surname.year = 2010, clean.surname = T, impute.missing = T) {

  if ("surname" %in% names(voter.file) == F) {
    stop('Data does not contain surname field.')
  }
  
  ## Census Surname List
  if (surname.year == 2000) {
    surnames <- surnames2000
  }
  surnames$surname <- as.character(surnames$surname)
  
  p_eth <- c("p_whi", "p_bla", "p_his", "p_asi", "p_oth")
  
  ## Convert Surnames in Voter File to Upper Case 
  df <- voter.file
  df$caseid <- 1:nrow(df)
  df$surname.match <- df$surname.upper <- toupper(as.character(df$surname))

  ## Merge Surnames with Census List (No Cleaning Yet)
  df <- merge(df[names(df) %in% p_eth == F], surnames[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)
  df[df$surname.upper %in% surnames$surname == F, ]$surname.match <- ""
  
  df1 <- df[df$surname.upper %in% surnames$surname, ] #Matched surnames
  df2 <- df[df$surname.upper %in% surnames$surname == F, ] #Unmatched surnames
  
  ## Clean Surnames (if Specified by User)
  if (clean.surname) {
    
    ## Remove All Punctuation and Try Merge Again
    df2$surname.match <- gsub("[^[:alnum:] ]", "", df2$surname.upper)
    df2 <- merge(df2[names(df2) %in% p_eth == F], surnames[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)
    if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {
      df1 <- rbind(df1, df2[df2$surname.match %in% surnames$surname, ])
      df2 <- df2[df2$surname.match %in% surnames$surname == F, ]
      if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {df2$surname.match <- ""}
    }

    ## Remove All Spaces and Try Merge Again
    df2$surname.match <- gsub(" ", "", df2$surname.match)
    df2 <- merge(df2[names(df2) %in% p_eth == F], surnames[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)
    if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {
      df1 <- rbind(df1, df2[df2$surname.match %in% surnames$surname, ])
      df2 <- df2[df2$surname.match %in% surnames$surname == F, ]
      if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {df2$surname.match <- ""}
    }

    ## Remove Jr/Sr/III Suffixes
    suffix <- c("JUNIOR", "SENIOR", "THIRD", "III", "JR", " II", " J R", " S R", " IV")
    for (i in 1:length(suffix)) {
      df2$surname.match <- ifelse(substr(df2$surname.match, nchar(df2$surname.match) - (nchar(suffix)[i] - 1), nchar(df2$surname.match)) == suffix[i], 
                                  substr(df2$surname.match, 1, nchar(df2$surname.match) - nchar(suffix)[i]), 
                                  df2$surname.match)
    }
    df2$surname.match <- ifelse(nchar(df2$surname.match) >= 7, 
                                ifelse(substr(df2$surname.match, nchar(df2$surname.match) - 1, nchar(df2$surname.match)) == "SR", 
                                       substr(df2$surname.match, 1, nchar(df2$surname.match) - 2), 
                                       df2$surname.match), 
                                df2$surname.match) #Remove "SR" only if name has at least 7 characters
    df2 <- merge(df2[names(df2) %in% p_eth == F], surnames[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)
    if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {
      df1 <- rbind(df1, df2[df2$surname.match %in% surnames$surname, ])
      df2 <- df2[df2$surname.match %in% surnames$surname == F, ]
      if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {df2$surname.match <- ""}
    }

    ## Names with Hyphens or Spaces, e.g. Double-Barreled Names
    df2$surname2 <- df2$surname1 <- NA
    df2$surname1[grep("-", df2$surname.upper)] <- sapply(strsplit(grep("-", df2$surname.upper, value = T), "-"), "[", 1)
    df2$surname2[grep("-", df2$surname.upper)] <- sapply(strsplit(grep("-", df2$surname.upper, value = T), "-"), "[", 2)  
    df2$surname1[grep(" ", df2$surname.upper)] <- sapply(strsplit(grep(" ", df2$surname.upper, value = T), " "), "[", 1)
    df2$surname2[grep(" ", df2$surname.upper)] <- sapply(strsplit(grep(" ", df2$surname.upper, value = T), " "), "[", 2)
    
    ## Use first half of name to merge in priors
    df2$surname.match <- as.character(df2$surname1)
    df2 <- merge(df2[names(df2) %in% c(p_eth) == F], surnames[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)[names(df2)]
    if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {
      df1 <- rbind(df1, df2[df2$surname.match %in% surnames$surname, names(df2) %in% names(df1)])
      df2 <- df2[df2$surname.match %in% surnames$surname == F, ]
      if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {df2$surname.match <- ""}
    }
    
    ## Use second half of name to merge in priors for rest
    df2$surname.match <- as.character(df2$surname2)
    df2 <- merge(df2[names(df2) %in% c(p_eth, "surname1", "surname2") == F], surnames[c("surname", p_eth)], by.x = "surname.match", by.y = "surname", all.x = TRUE)[names(df2) %in% c("surname1", "surname2") == F]
    if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {
      df1 <- rbind(df1, df2[df2$surname.match %in% surnames$surname, names(df2) %in% names(df1)])
      df2 <- df2[df2$surname.match %in% surnames$surname == F, ]
      if (nrow(df2[df2$surname.match %in% surnames$surname, ]) > 0) {df2$surname.match <- ""}
    }
  }

  ## Impute priors for names not on Census lists
  if (impute.missing) {
    if (nrow(df2) > 0) {
      df2$surname.match <- ""
      df2$p_whi <- .6665; df2$p_bla <- .0853; df2$p_his <- .1367; df2$p_asi <- .0797; df2$p_oth <- .0318
      warning(paste("Probabilities were imputed for", nrow(df2), ifelse(nrow(df2) == 1, "surname", "surnames"), "that could not be matched to Census list."))
    }
  } else warning(paste(nrow(df2), ifelse(nrow(df2) == 1, "surname was", "surnames were"), "not matched."))
  
  df <- rbind(df1, df2)
  return(df[order(df$caseid), c(names(voter.file), "surname.match", p_eth)])
}
