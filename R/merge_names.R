#' Surname probability merging function.
#'
#' \code{merge_names} merges names in a user-input dataset with corresponding
#'  race/ethnicity probabilities derived from both the U.S. Census Surname List
#'  and Spanish Surname List and voter files from states in the Southern U.S.
#'
#' This function allows users to match names in their dataset with database entries
#'  estimating P(name | ethnicity) for each of the five major racial groups for each
#'  name. The database probabilities are derived from both the U.S. Census Surname List
#'  and Spanish Surname List and voter files from states in the Southern U.S.
#'
#'  By default, the function matches names as follows:
#'  1) Search raw surnames in the database;
#'  2) Remove any punctuation and search again;
#'  3) Remove any spaces and search again;
#'  4) Remove suffixes (e.g., "Jr") and search again (last names only)
#'  5) Split double-barreled names into two parts and search first part of name;
#'  6) Split double-barreled names into two parts and search second part of name;
#'
#'  Each step only applies to names not matched in a previous step.
#'  Steps 2 through 6 are not applied if \code{clean.surname} is FALSE.
#'
#'  Note: Any name appearing only on the Spanish Surname List is assigned a
#'  probability of 1 for Hispanics/Latinos and 0 for all other racial groups.
#'
#' @param voter.file An object of class \code{data.frame}.  Must contain a row for each individual being predicted,
#' as well as a field named \code{\var{last}} containing each individual's surname.
#' If first name is also being used for prediction, the file must also contain a field
#' named \code{\var{first}}. If middle name is also being used for prediction, the field
#' must also contain a field named \code{\var{middle}}.
#' @param namesToUse A character vector identifying which names to use for the prediction.
#' The default value is \code{"last"}, indicating that only the last name will be used.
#' Other options are \code{"last, first"}, indicating that both last and first names will be
#' used, and \code{"last, first, middle"}, indicating that last, first, and middle names will all
#' be used.
#' @param table.surnames An object of class \code{data.frame} provided by the 
#' users as an alternative surname dictionary. It will consist of a list of 
#' U.S. surnames, along with the associated probabilities P(name | ethnicity) 
#' for ethnicities: white, Black, Hispanic, Asian, and other. Default is \code{NULL}.
#' (\code{\var{last_name}} for U.S. surnames, \code{\var{p_whi_last}} for White,
#' \code{\var{p_bla_last}} for Black, \code{\var{p_his_last}} for Hispanic,
#' \code{\var{p_asi_last}} for Asian, \code{\var{p_oth_last}} for other).
#' @param table.first See \code{\var{table.surnames}}.
#' @param table.middle See \code{\var{table.surnames}}.
#' @param clean.names A \code{TRUE}/\code{FALSE} object. If \code{TRUE},
#' any surnames in \code{\var{voter.file}} that cannot initially be matched
#' to the database will be cleaned, according to U.S. Census specifications,
#' in order to increase the chance of finding a match. Default is \code{TRUE}.
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
#' @import stringr
#'
#' @examples
#' data(voters)
#' merge_names(voters)
#'
#' @export
merge_names <- function(voter.file, namesToUse, table.surnames=NULL, table.first=NULL, table.middle=NULL, clean.names = TRUE, impute.missing=TRUE) {
  
  # check the names
  if(namesToUse == 'last') {
    if(!("last" %in% names(voter.file)))
      stop("Voter data frame needs to have a column named 'last'.")
    
  } else if(namesToUse == 'last, first') {
    if(!("last" %in% names(voter.file)) || !("first" %in% names(voter.file)))
      stop("Voter data frame needs to have a column named 'last' and a column called 'first'.")
    
  } else if(namesToUse == 'last, first, middle') {
    if(!("last" %in% names(voter.file)) || !("first" %in% names(voter.file))
       || !("middle" %in% names(voter.file)))
      stop("Voter data frame needs to have a column named 'last', a column called 'first', and a column called 'middle'.")
  }
  
  # read in the name files and cast NA to the null string
  # firstNameDict[is.na(firstNameDict$first_name),]$first_name <- ''
  # middleNameDict[is.na(middleNameDict$middle_name),]$middle_name <- ''
  # lastNameDict[is.na(lastNameDict$last_name),]$last_name <- ''
  
  p_eth <- c("p_whi", "p_bla", "p_his", "p_asi", "p_oth")
  if(is.null(table.surnames)){
    lastNameDict<- wruData::last
  } else {
    lastNameDict <- table.surnames
    names(lastNameDict) <- names(wruData::last)
  }
  if(is.null(table.first)){
    firstNameDict<- wruData::first
  } else {
    firstNameDict <- table.first
    names(firstNameDict) <- names(wruData::first)
  }
  if(is.null(table.middle)){
    middleNameDict<- wruData::mid
  } else {
    middleNameDict <- table.middle
    names(middleNameDict) <- names(wruData::mid)
  }
  
  nameDict <- list('first' =firstNameDict,
                   'middle' = middleNameDict,
                   'last' = lastNameDict)
  ## Convert names in voter file to upper case
  df <- voter.file
  df$caseid <- 1:nrow(df)
  
  df$lastname.match <- df$lastname.upper <- toupper(as.character(df$last))
  if(grepl('first', namesToUse))
    df$firstname.match <- df$firstname.upper <- toupper(as.character(df$first))
  if(grepl('middle', namesToUse)) {
    df$middlename.match <- df$middlename.upper <- toupper(as.character(df$middle))
    df$middlename.match[is.na(df$middlename.match)] <- ''
  }
  
  ## Merge Surnames with Census List (No Cleaning Yet)
  df <- merge(df, lastNameDict, by.x = "lastname.match", by.y = "last_name", all.x = TRUE)
  if(grepl('first', namesToUse))
    df <- merge(df, firstNameDict, by.x = "firstname.match", by.y = "first_name", all.x = TRUE)
  if(grepl('middle', namesToUse)) {
    df <- merge(df, middleNameDict, by.x = "middlename.match", by.y = "middle_name", all.x = TRUE)
  }
  
  if(namesToUse == 'last' && sum(!(df$lastname.upper %in% lastNameDict$last_name)) == 0)
    return(df[order(df$caseid), c(names(voter.file), "lastname.match", paste0(p_eth, "_last"))])
  if(namesToUse == 'last, first' && sum(!(df$lastname.match %in% lastNameDict$last_name)) == 0 &&
     sum(!(df$firstname.upper %in% firstNameDict$first_name)) == 0)
    return(df[order(df$caseid), c(names(voter.file), "lastname.match", "firstname.match", paste0(p_eth, "_last"), paste0(p_eth, "_first"))])
  if(namesToUse == 'last, first, middle' && sum(!(df$lastname.match %in% lastNameDict$last_name)) == 0 &&
     sum(!(df$firstname.upper %in% firstNameDict$first_name)) == 0 && sum(!(df$middlename.upper %in% middleNameDict$middle_name)) == 0)
    return(df[order(df$caseid), c(names(voter.file), "lastname.match", "firstname.match", "middlename.match", paste0(p_eth, "_last"), paste0(p_eth, "_first"), paste0(p_eth, "_middle"))])
  
  ## Clean names (if specified by user)
  if(clean.names) {
    
    for(nameType in str_split(namesToUse, ', ')[[1]]) {
      
      df1 <- df[!is.na(df[,paste('p_whi_', nameType, sep = '')]), ] #Matched names
      df2 <- df[is.na(df[,paste('p_whi_', nameType, sep = '')]), ] #Unmatched names
      
      ## Remove All Punctuation and Try Merge Again
      if(nrow(df2) > 0) {
        df2[,paste(nameType, "name.match", sep = "")] <- gsub("[^[:alnum:] ]", "", df2[,paste(nameType, "name.upper", sep = "")])
        
        df2 <- merge(df2[,!grepl(paste('_', nameType, sep = ''), names(df2))], nameDict[[nameType]], all.x = TRUE,
                     by.x = paste(nameType, "name.match", sep = ""), by.y = paste(nameType, "name", sep = '_'))
        df2 <- df2[,names(df1)] # reorder the columns
        
        if (sum(!is.na(df2[,paste('p_whi_', nameType, sep = ''),])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[,paste('p_whi_', nameType, sep = ''),]), ])
          df2 <- df2[is.na(df2[,paste('p_whi_', nameType, sep = '')]), ]
        }
      }
      
      ## Remove All Spaces and Try Merge Again
      if(nrow(df2) > 0) {
        df2[,paste(nameType, "name.match", sep = "")] <- gsub(" ", "", df2[,paste(nameType, "name.match", sep = "")])
        df2 <- merge(df2[,!grepl(paste('_', nameType, sep = ''), names(df2))], nameDict[[nameType]], all.x = TRUE,
                     by.x = paste(nameType, "name.match", sep = ""), by.y = paste(nameType, "name", sep = '_'))
        df2 <- df2[,names(df1)] # reorder the columns
        
        if (sum(!is.na(df2[,paste('p_whi_', nameType, sep = ''),])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[,paste('p_whi_', nameType, sep = ''),]), ])
          df2 <- df2[is.na(df2[,paste('p_whi_', nameType, sep = '')]), ]
        }
      }
      
      # Edits specific to common issues with last names
      if(nameType == 'last' & nrow(df2) > 0) {
        
        ## Remove Jr/Sr/III Suffixes for last names
        suffix <- c("JUNIOR", "SENIOR", "THIRD", "III", "JR", " II", " J R", " S R", " IV")
        for (i in 1:length(suffix)) {
          df2$lastname.match <- ifelse(substr(df2$lastname.match, nchar(df2$lastname.match) - (nchar(suffix)[i] - 1), nchar(df2$lastname.match)) == suffix[i],
                                       substr(df2$lastname.match, 1, nchar(df2$lastname.match) - nchar(suffix)[i]),
                                       df2$lastname.match)
        }
        df2$lastname.match <- ifelse(nchar(df2$lastname.match) >= 7,
                                     ifelse(substr(df2$lastname.match, nchar(df2$lastname.match) - 1, nchar(df2$lastname.match)) == "SR",
                                            substr(df2$lastname.match, 1, nchar(df2$lastname.match) - 2),
                                            df2$lastname.match),
                                     df2$lastname.match) #Remove "SR" only if name has at least 7 characters
        
        df2 <- merge(df2[,!grepl(paste('_', nameType, sep = ''), names(df2))], lastNameDict, by.x = "lastname.match", by.y = "last_name", all.x = TRUE)
        df2 <- df2[,names(df1)] # reorder the columns
        
        if (sum(!is.na(df2[,paste('p_whi_', nameType, sep = ''),])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[,paste('p_whi_', nameType, sep = ''),]), ])
          df2 <- df2[is.na(df2[,paste('p_whi_', nameType, sep = '')]), ]
        }
      }
      
      
      ## Names with Hyphens or Spaces, e.g. Double-Barreled Names
      if(nrow(df2) > 0) {
        df2$name2 <- df2$name1 <- NA
        df2$name1[grep("-", df2[,paste(nameType, "name.upper", sep = "")])] <- sapply(strsplit(grep("-", df2[,paste(nameType, "name.upper", sep = "")], value = T), "-"), "[", 1)
        df2$name2[grep("-", df2[,paste(nameType, "name.upper", sep = "")])] <- sapply(strsplit(grep("-", df2[,paste(nameType, "name.upper", sep = "")], value = T), "-"), "[", 2)
        df2$name1[grep(" ", df2[,paste(nameType, "name.upper", sep = "")])] <- sapply(strsplit(grep(" ", df2[,paste(nameType, "name.upper", sep = "")], value = T), " "), "[", 1)
        df2$name2[grep(" ", df2[,paste(nameType, "name.upper", sep = "")])] <- sapply(strsplit(grep(" ", df2[,paste(nameType, "name.upper", sep = "")], value = T), " "), "[", 2)
        
        ## Use first half of name to merge in priors
        df2[,paste(nameType, "name.match", sep = "")]  <- as.character(df2$name1)
        df2 <- merge(df2[,!grepl(paste('_', nameType, sep = ''), names(df2))], nameDict[[nameType]], all.x = TRUE,
                     by.x = paste(nameType, "name.match", sep = ""), by.y = paste(nameType, "name", sep = '_'))
        df2 <- df2[,c(names(df1), "name1", "name2")] # reorder the columns
        
        if (sum(!is.na(df2[,paste('p_whi_', nameType, sep = ''),])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[,paste('p_whi_', nameType, sep = '')]), !(names(df2) %in% c("name1", "name2"))])
          df2 <- df2[is.na(df2[,paste('p_whi_', nameType, sep = '')]), ]
        }
      }
      
      ## Use second half of name to merge in priors for rest
      if(nrow(df2) > 0) {
        df2[,paste(nameType, "name.match", sep = "")]  <- as.character(df2$name2)
        df2 <- merge(df2[,!grepl(paste('_', nameType, sep = ''), names(df2))], nameDict[[nameType]], all.x = TRUE,
                     by.x = paste(nameType, "name.match", sep = ""), by.y = paste(nameType, "name", sep = '_'))
        df2 <- df2[,c(names(df1), "name1", "name2")] # reorder the columns
        
        if (sum(!is.na(df2[,paste('p_whi_', nameType, sep = ''),])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[,paste('p_whi_', nameType, sep = '')]), !(names(df2) %in% c("name1", "name2"))])
          df2 <- df2[is.na(df2[,paste('p_whi_', nameType, sep = '')]), ]
        }
      }
      
      if(nrow(df2) > 0)
        df <- rbind(df1, df2[, !(names(df2) %in% c("name1", "name2"))])
      else
        df <- df1
      df <- df[order(df$caseid),]
    }
  }
  
  
  ## For unmatched names, just fill with an 1
  require(dplyr)
  warning(paste(paste(sum(is.na(df$p_whi_last)), " (", round(100*mean(is.na(df$p_whi_last)), 1), "%) individuals' last names were not matched.", sep = "")))
  if(grepl('first', namesToUse)) {
    warning(paste(paste(sum(is.na(df$p_whi_first)), " (", round(100*mean(is.na(df$p_whi_first)), 1), "%) individuals' first names were not matched.", sep = "")))
  }
  if(grepl('middle', namesToUse)) {
    warning(paste(paste(sum(is.na(df$p_whi_middle)), " (", round(100*mean(is.na(df$p_whi_middle)), 1), "%) individuals' middle names were not matched.", sep = "")))
  }
  
  if(impute.missing){
    inputer <- c(p_whi = .6665,p_bla = .0853, p_his = .1367, p_asi = .0797,p_oth = .0318)
    for(i in grep("p_", names(df), value=TRUE)) {
      #df[,i] <- coalesce(df[,i], 1)
      df[is.na(df[,i]),i] <- inputer[i]
    }
  }
  
  # return the data
  if(namesToUse == 'last')
    return(df[order(df$caseid), c(names(voter.file), "lastname.match", paste(p_eth, "last", sep = "_"))])
  else if(namesToUse == 'last, first')
    return(df[order(df$caseid), c(names(voter.file), "lastname.match", "firstname.match",
                                  paste(p_eth, "last", sep = "_"), paste(p_eth, "first", sep = "_"))])
  else if(namesToUse == 'last, first, middle')
    return(df[order(df$caseid), c(names(voter.file), "lastname.match", "firstname.match", "middlename.match",
                                  paste(p_eth, "last", sep = "_"), paste(p_eth, "first", sep = "_"), paste(p_eth, "middle", sep = "_"))])
  
}