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
#' @param name_source One of \code{"mixed"}, \code{"census_only"}, or \code{"vf_only"};
#'  see [predict_race()].
#' @param year Census vintage, \code{"2020"} (default) or \code{"2010"}.
#' @param table.surnames An object of class \code{data.frame} provided by the
#' users as an alternative surname dictionary. It will consist of a list of
#' U.S. surnames, along with the associated probabilities P(name | ethnicity)
#' for ethnicities: white, Black, Hispanic, Asian, and other. Default is \code{NULL}.
#' (\code{\var{last_name}} for U.S. surnames, \code{\var{p_whi_last}} for White,
#' \code{\var{p_bla_last}} for Black, \code{\var{p_his_last}} for Hispanic,
#' \code{\var{p_asi_last}} for Asian, \code{\var{p_oth_last}} for other).
#' @param table.first See \code{\var{table.surnames}}.
#' @param table.middle See \code{\var{table.surnames}}.
#' @param impute.missing See \code{predict_race}.
#' @param model See \code{predict_race}.
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
#' @importFrom dplyr coalesce
#' @examples
#' data(voters)
#' \dontrun{try(merge_names(voters, namesToUse = "surname", name_source = "mixed"))}
#' @param return.unmatched A \code{TRUE}/\code{FALSE} object. If \code{TRUE},
#' boolean columns (\code{last_matched}, and \code{first_matched} /
#' \code{middle_matched} when those names are used) are appended, reporting
#' whether each name was found in the dictionary. The flags are computed
#' before imputation, so they remain \code{FALSE} for names that were only
#' filled in by imputation. Default is \code{FALSE}.
#' @keywords internal
merge_names <- function(voter.file, namesToUse, name_source = "mixed", year = "2020", table.surnames = NULL, table.first = NULL, table.middle = NULL, clean.names = TRUE, impute.missing = FALSE, model = "BISG", return.unmatched = FALSE) {

  # check the names
  if (namesToUse == "surname") {
    if (!("surname" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named 'surname'.")
    }
  } else if (namesToUse == "surname, first") {
    if (!("surname" %in% names(voter.file)) || !("first" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named 'surname' and a column called 'first'.")
    }
  } else if (namesToUse == "surname, first, middle") {
    if (!("surname" %in% names(voter.file)) || !("first" %in% names(voter.file)) ||
      !("middle" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named 'surname', a column called 'first', and a column called 'middle'.")
    }
  }

  nameDict <- load_name_dictionaries(
    namesToUse = namesToUse,
    name_source = name_source,
    year = year,
    table.surnames = table.surnames,
    table.first = table.first,
    table.middle = table.middle
  )
  lastNameDict <- nameDict[["last"]]
  firstNameDict <- nameDict[["first"]]
  middleNameDict <- nameDict[["middle"]]

  p_eth <- c("c_whi", "c_bla", "c_his", "c_asi", "c_oth")

  ## Convert names in voter file to upper case
  df <- voter.file

  df$lastname.match <- df$lastname.upper <- toupper(as.character(df$surname))
  if (grepl("first", namesToUse)) {
    df$firstname.match <- df$firstname.upper <- toupper(as.character(df$first))
  }
  if (grepl("middle", namesToUse)) {
    df$middlename.match <- df$middlename.upper <- toupper(as.character(df$middle))
    df$middlename.match[is.na(df$middlename.match)] <- ""
  }

  ## Merge Surnames with Census List (No Cleaning Yet)
  df <- merge(df, lastNameDict, by.x = "lastname.match", by.y = "last_name", all.x = TRUE, sort = FALSE)
  if (grepl("first", namesToUse)) {
    df <- merge(df, firstNameDict, by.x = "firstname.match", by.y = "first_name", all.x = TRUE, sort = FALSE)
  }
  if (grepl("middle", namesToUse)) {
    df <- merge(df, middleNameDict, by.x = "middlename.match", by.y = "middle_name", all.x = TRUE, sort = FALSE)
  }

  ## Match-flag columns appended on the way out (when return.unmatched). The
  ## all-matched case needs no early return: the cleaning cascade below simply
  ## runs as a no-op and falls through to the same final return.
  flag_cols <- if (return.unmatched) {
    c("last_matched",
      if (grepl("first", namesToUse)) "first_matched",
      if (grepl("middle", namesToUse)) "middle_matched")
  } else character(0)

  ## Clean names (if specified by user)
  if (clean.names) {
    for (nameType in strsplit(namesToUse, ", ")[[1]]) {
      if(nameType=="surname"){
        nameType <- "last"
      }
      df1 <- df[!is.na(df[, paste("c_whi_", nameType, sep = "")]), ] # Matched names
      df2 <- df[is.na(df[, paste("c_whi_", nameType, sep = "")]), ] # Unmatched names

      ## Remove All Punctuation and Try Merge Again
      if (nrow(df2) > 0) {
        df2[, paste(nameType, "name.match", sep = "")] <- gsub("[^[:alnum:] ]", "", df2[, paste(nameType, "name.upper", sep = "")])

        df2 <- merge(df2[, !grepl(paste("_", nameType, sep = ""), names(df2))], nameDict[[nameType]],
          all.x = TRUE,
          by.x = paste(nameType, "name.match", sep = ""), by.y = paste(nameType, "name", sep = "_"),
          sort = FALSE
        )
        df2 <- df2[, names(df1)] # reorder the columns

        if (sum(!is.na(df2[, paste("c_whi_", nameType, sep = ""), ])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[, paste("c_whi_", nameType, sep = ""), ]), ])
          df2 <- df2[is.na(df2[, paste("c_whi_", nameType, sep = "")]), ]
        }
      }

      ## Remove All Spaces and Try Merge Again
      if (nrow(df2) > 0) {
        df2[, paste(nameType, "name.match", sep = "")] <- gsub(" ", "", df2[, paste(nameType, "name.match", sep = "")])
        df2 <- merge(df2[, !grepl(paste("_", nameType, sep = ""), names(df2))], nameDict[[nameType]],
          all.x = TRUE,
          by.x = paste(nameType, "name.match", sep = ""), by.y = paste(nameType, "name", sep = "_"),
          sort = FALSE
        )
        df2 <- df2[, names(df1)] # reorder the columns

        if (sum(!is.na(df2[, paste("c_whi_", nameType, sep = ""), ])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[, paste("c_whi_", nameType, sep = ""), ]), ])
          df2 <- df2[is.na(df2[, paste("c_whi_", nameType, sep = "")]), ]
        }
      }

      # Edits specific to common issues with last names
      if (nameType == "last" & nrow(df2) > 0) {

        ## Remove Jr/Sr/III Suffixes for last names
        suffix <- c("JUNIOR", "SENIOR", "THIRD", "III", "JR", " II", " J R", " S R", " IV")
        for (i in 1:length(suffix)) {
          df2$lastname.match <- ifelse(substr(df2$lastname.match, nchar(df2$lastname.match) - (nchar(suffix)[i] - 1), nchar(df2$lastname.match)) == suffix[i],
            substr(df2$lastname.match, 1, nchar(df2$lastname.match) - nchar(suffix)[i]),
            df2$lastname.match
          )
        }
        df2$lastname.match <- ifelse(nchar(df2$lastname.match) >= 7,
          ifelse(substr(df2$lastname.match, nchar(df2$lastname.match) - 1, nchar(df2$lastname.match)) == "SR",
            substr(df2$lastname.match, 1, nchar(df2$lastname.match) - 2),
            df2$lastname.match
          ),
          df2$lastname.match
        ) # Remove "SR" only if name has at least 7 characters

        df2 <- merge(
          df2[, !grepl(paste("_", nameType, sep = ""), names(df2))], 
          lastNameDict, by.x = "lastname.match", by.y = "last_name", 
          all.x = TRUE, sort = FALSE)
        df2 <- df2[, names(df1)] # reorder the columns

        if (sum(!is.na(df2[, paste("c_whi_", nameType, sep = ""), ])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[, paste("c_whi_", nameType, sep = ""), ]), ])
          df2 <- df2[is.na(df2[, paste("c_whi_", nameType, sep = "")]), ]
        }
      }


      ## Names with Hyphens or Spaces, e.g. Double-Barreled Names
      if (nrow(df2) > 0) {
        df2$name2 <- df2$name1 <- NA
        df2$name1[grep("-", df2[, paste(nameType, "name.upper", sep = "")])] <- sapply(strsplit(grep("-", df2[, paste(nameType, "name.upper", sep = "")], value = T), "-"), "[", 1)
        df2$name2[grep("-", df2[, paste(nameType, "name.upper", sep = "")])] <- sapply(strsplit(grep("-", df2[, paste(nameType, "name.upper", sep = "")], value = T), "-"), "[", 2)
        df2$name1[grep(" ", df2[, paste(nameType, "name.upper", sep = "")])] <- sapply(strsplit(grep(" ", df2[, paste(nameType, "name.upper", sep = "")], value = T), " "), "[", 1)
        df2$name2[grep(" ", df2[, paste(nameType, "name.upper", sep = "")])] <- sapply(strsplit(grep(" ", df2[, paste(nameType, "name.upper", sep = "")], value = T), " "), "[", 2)

        ## Use first half of name to merge in priors
        df2[, paste(nameType, "name.match", sep = "")] <- as.character(df2$name1)
        df2 <- merge(df2[, !grepl(paste("_", nameType, sep = ""), names(df2))], nameDict[[nameType]],
          all.x = TRUE,
          by.x = paste(nameType, "name.match", sep = ""), by.y = paste(nameType, "name", sep = "_"),
          sort = FALSE
        )
        df2 <- df2[, c(names(df1), "name1", "name2")] # reorder the columns

        if (sum(!is.na(df2[, paste("c_whi_", nameType, sep = ""), ])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[, paste("c_whi_", nameType, sep = "")]), !(names(df2) %in% c("name1", "name2"))])
          df2 <- df2[is.na(df2[, paste("c_whi_", nameType, sep = "")]), ]
        }
      }

      ## Use second half of name to merge in priors for rest
      if (nrow(df2) > 0) {
        df2[, paste(nameType, "name.match", sep = "")] <- as.character(df2$name2)
        df2 <- merge(df2[, !grepl(paste("_", nameType, sep = ""), names(df2))], nameDict[[nameType]],
          all.x = TRUE,
          by.x = paste(nameType, "name.match", sep = ""), by.y = paste(nameType, "name", sep = "_"),
          sort = FALSE
        )
        df2 <- df2[, c(names(df1), "name1", "name2")] # reorder the columns

        if (sum(!is.na(df2[, paste("c_whi_", nameType, sep = ""), ])) > 0) {
          df1 <- rbind(df1, df2[!is.na(df2[, paste("c_whi_", nameType, sep = "")]), !(names(df2) %in% c("name1", "name2"))])
          df2 <- df2[is.na(df2[, paste("c_whi_", nameType, sep = "")]), ]
        }
      }

      if (nrow(df2) > 0) {
        df <- rbind(df1, df2[, !(names(df2) %in% c("name1", "name2"))])
      } else {
        df <- df1
      }
    }
  }

  name_types <- c("last",
                  if (grepl("first", namesToUse)) "first",
                  if (grepl("middle", namesToUse)) "middle")

  ## Undo the cascade's "NA" coercion: an unmatched non-hyphenated name ends up
  ## with a match key of "NA", which then merges against the genuine surname "Na"
  ## (~97% Asian in the voter-file dictionary). Treat those rows as unmatched so
  ## they are imputed (or left NA), not assigned "Na" probabilities (#162).
  for (nt in name_types) {
    bad <- toupper(df[[paste0(nt, "name.match")]]) == "NA" &
           toupper(df[[paste0(nt, "name.upper")]]) != "NA"
    bad[is.na(bad)] <- FALSE
    if (any(bad)) df[bad, paste0(p_eth, "_", nt)] <- NA
  }

  ## For unmatched names, just fill with an column mean if impute is true, or with constant if false
  c_miss_last <- mean(is.na(df$c_whi_last))
  if (c_miss_last > 0) {
    message(paste(paste(sum(is.na(df$c_whi_last)), " (", round(100 * mean(is.na(df$c_whi_last)), 1), "%) individuals' last names were not matched.", sep = "")))
  }
  if (grepl("first", namesToUse)) {
    c_miss_first <- mean(is.na(df$c_whi_first))
    if (c_miss_first > 0) {
      message(paste(paste(sum(is.na(df$c_whi_first)), " (", round(100 * mean(is.na(df$c_whi_first)), 1), "%) individuals' first names were not matched.", sep = "")))
    }
  }
  if (grepl("middle", namesToUse)) {
    c_miss_mid <- mean(is.na(df$c_whi_middle))
    if (c_miss_mid > 0) {
      message(paste(paste(sum(is.na(df$c_whi_middle)), " (", round(100 * mean(is.na(df$c_whi_middle)), 1), "%) individuals' middle names were not matched.", sep = "")))
    }
  }

  ## Record match flags before imputation so imputed rows still read as
  ## unmatched. After the cascade and the "NA" cleanup above, a non-NA
  ## probability means the name was matched (#105).
  if (return.unmatched) {
    for (nt in name_types) df[[paste0(nt, "_matched")]] <- !is.na(df[[paste0("c_whi_", nt)]])
  }

  ## When impute.missing is TRUE, fill unmatched names with the column mean.
  ## When FALSE, leave them as NA so callers can identify unmatched names (#162).
  if (impute.missing) {
    impute.vec <- colMeans(df[, grep("^c_", names(df), value = TRUE)], na.rm = TRUE)
    for (i in grep("^c_", names(df), value = TRUE)) {
      df[, i] <- dplyr::coalesce(df[, i], impute.vec[i])
    }
  }

  # return the data
  if (namesToUse == "surname") {
    return(df[, c(names(voter.file), "lastname.match", paste(p_eth, "last", sep = "_"), flag_cols)])
  } else if (namesToUse == "surname, first") {
    return(df[, c(
      names(voter.file), "lastname.match", "firstname.match",
      paste(p_eth, "last", sep = "_"), paste(p_eth, "first", sep = "_"), flag_cols
    )])
  } else if (namesToUse == "surname, first, middle") {
    return(df[, c(
      names(voter.file), "lastname.match", "firstname.match", "middlename.match",
      paste(p_eth, "last", sep = "_"), paste(p_eth, "first", sep = "_"), paste(p_eth, "middle", sep = "_"), flag_cols
    )])
  }
}


#' Preflight for name data
#'
#' Checks if namedata is available in the current working directory, if not
#' downloads it from github using piggyback. By default, wru will download the
#' data to a temporary directory that lasts as long as your session does.
#' However, you may wish to set the \code{wru_data_wd} option to save the 
#' downloaded data to your current working directory for more permanence. 
#'
#' @importFrom piggyback pb_download
wru_data_preflight <- function() {
  dest <- ifelse(getOption("wru_data_wd", default = FALSE), getwd(), tempdir())
  tryCatch(
    # Oddity of conditions for .token. Ignores token if is ""
    piggyback::pb_download(repo = "kosukeimai/wru", dest = dest, .token = "", tag = "v4.0.0"), 
    error = function(e) message("There was an error retrieving data: ", e$message)
  )
}
