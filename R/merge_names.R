# ------------------------------
# Helper: resolve cache location
# ------------------------------
wru_cache_dir <- function() {
  opt <- getOption("wru_data_wd", default = FALSE)
  if (isTRUE(opt)) {
    # TRUE means: use current working directory
    return(getwd())
  }
  if (is.character(opt) && length(opt) == 1 && !is.na(opt) && nzchar(opt)) {
    dir.create(path.expand(opt), recursive = TRUE, showWarnings = FALSE)
    return(path.expand(opt))
  }
  # default: tempdir for this session
  tempdir()
}

#' Preflight for name data (persistent, pinned, and verified)
#' Downloads 4 name dictionaries to a persistent directory if missing.
#' Set options(wru_data_wd = TRUE) to cache in getwd(); otherwise uses tempdir().
wru_data_preflight <- function() {
  dest <- ifelse(getOption("wru_data_wd", default = FALSE), getwd(), tempdir())
  files_needed <- c(
    "wru-data-first_c.rds",
    "wru-data-mid_c.rds",
    "wru-data-census_last_c.rds",
    "wru-data-last_c.rds"
  )
  missing <- files_needed[!file.exists(file.path(dest, files_needed))]
  if (length(missing) == 0L) return(invisible(TRUE))
  
  # Use the release that actually has these assets (matches CRAN code).
  tag_to_use <- "v2.0.0"
  
  # Try download; piggyback ignores .token when "" (same as CRAN code).
  tryCatch(
    {
      piggyback::pb_download(
        file = missing,
        repo = "kosukeimai/wru",
        tag  = tag_to_use,
        dest = dest,
        overwrite = TRUE,
        .token = ""
      )
    },
    error = function(e) {
      stop(
        sprintf(
          "wru_data_preflight() failed to fetch: %s\nReason: %s",
          paste(missing, collapse = ", "),
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  
  # Verify they arrived; if not, guide the user.
  still_missing <- files_needed[!file.exists(file.path(dest, files_needed))]
  if (length(still_missing) > 0L) {
    stop(
      paste0(
        "wru_data_preflight(): some files are still missing after download: ",
        paste(still_missing, collapse = ", "),
        "\nCheck network/GitHub access or download these assets from the ",
        "wru v2.0.0 release and place them in: ", normalizePath(dest),
        "\nThen re-run."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Surname probability merging function.
#'
#' \code{merge_names} merges names in a user-input dataset with corresponding
#' race/ethnicity probabilities derived from both the U.S. Census Surname List
#' and Spanish Surname List and voter files from states in the Southern U.S.
#'
#' @param voter.file data.frame with at least a \code{surname} column; \code{first} and \code{middle} if used
#' @param namesToUse "surname", "surname, first", or "surname, first, middle"
#' @param census.surname logical; if TRUE, use Census/Spanish surname list
#' @param table.surnames Optional custom last-name dictionary (schema like shipped RDS)
#' @param table.first   Optional custom first-name dictionary
#' @param table.middle  Optional custom middle-name dictionary
#' @param clean.names   logical; try cleaning/fallback passes
#' @param impute.missing logical; impute c_* NA with column means (else 1)
#' @param model         kept for API parity
#' @param return.unmatched logical; add *_matched boolean columns
#' @return data.frame with probabilities (and flags if requested)
#' @keywords internal
merge_names <- function(
    voter.file,
    namesToUse,
    census.surname,
    table.surnames = NULL,
    table.first = NULL,
    table.middle = NULL,
    clean.names = TRUE,
    impute.missing = FALSE,
    model = "BISG",
    return.unmatched = TRUE
) {
  # --- arg checks ------------------------------------------------------------
  if (namesToUse == "surname") {
    if (!("surname" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named 'surname'.")
    }
  } else if (namesToUse == "surname, first") {
    if (!all(c("surname","first") %in% names(voter.file))) {
      stop("Voter data frame needs 'surname' and 'first'.")
    }
  } else if (namesToUse == "surname, first, middle") {
    if (!all(c("surname","first","middle") %in% names(voter.file))) {
      stop("Voter data frame needs 'surname', 'first', and 'middle'.")
    }
  }
  
  # --- ensure data present (downloads only if missing) -----------------------
  wru_data_preflight()
  
  path   <- wru_cache_dir()
  first_c <- readRDS(file.path(path, "wru-data-first_c.rds"))
  mid_c   <- readRDS(file.path(path, "wru-data-mid_c.rds"))
  last_c  <- readRDS(file.path(
    path,
    if (isTRUE(census.surname)) "wru-data-census_last_c.rds" else "wru-data-last_c.rds"
  ))
  
  if (!is.data.frame(first_c) || !is.data.frame(mid_c) || !is.data.frame(last_c)) {
    stop("wru cached name dictionaries are not data.frames; clear cache and re-run wru_data_preflight().")
  }
  
  p_eth <- c("c_whi", "c_bla", "c_his", "c_asi", "c_oth")
  
  # optional custom dictionaries; align names to shipped schema
  if (is.null(table.surnames)) {
    lastNameDict <- last_c
  } else {
    lastNameDict <- table.surnames
    names(lastNameDict) <- names(last_c)
    lastNameDict[is.na(lastNameDict)] <- 0
  }
  if (is.null(table.first)) {
    firstNameDict <- first_c
  } else {
    firstNameDict <- table.first
    names(firstNameDict) <- names(first_c)
    firstNameDict[is.na(firstNameDict)] <- 0
  }
  if (is.null(table.middle)) {
    middleNameDict <- mid_c
  } else {
    middleNameDict <- table.middle
    names(middleNameDict) <- names(mid_c)
    middleNameDict[is.na(middleNameDict)] <- 0
  }
  
  nameDict <- list(first = firstNameDict, middle = middleNameDict, last = lastNameDict)
  
  # --- initial normalization & merges ---------------------------------------
  df <- voter.file
  
  df$lastname.match <- df$lastname.upper <- toupper(as.character(df$surname))
  if (grepl("first", namesToUse)) {
    df$firstname.match <- df$firstname.upper <- toupper(as.character(df$first))
  }
  if (grepl("middle", namesToUse)) {
    df$middlename.match <- df$middlename.upper <- toupper(as.character(df$middle))
    df$middlename.match[is.na(df$middlename.match)] <- ""
  }
  
  # first pass merges (raw)
  df <- merge(df, lastNameDict, by.x = "lastname.match", by.y = "last_name", all.x = TRUE, sort = FALSE)
  if (grepl("first", namesToUse)) {
    df <- merge(df, firstNameDict, by.x = "firstname.match", by.y = "first_name", all.x = TRUE, sort = FALSE)
  }
  if (grepl("middle", namesToUse)) {
    df <- merge(df, middleNameDict, by.x = "middlename.match", by.y = "middle_name", all.x = TRUE, sort = FALSE)
  }
  
  # compute flags now so early-returns also include them
  if (isTRUE(return.unmatched)) {
    if ("c_whi_last"   %in% names(df)) df$last_matched   <- !is.na(df$c_whi_last)
    if ("c_whi_first"  %in% names(df)) df$first_matched  <- !is.na(df$c_whi_first)
    if ("c_whi_middle" %in% names(df)) df$middle_matched <- !is.na(df$c_whi_middle)
  }
  
  # unified early-return helper
  fast_return <- function() {
    prob_cols <- c(paste0(p_eth, "_last"))
    name_cols <- c("lastname.match")
    flag_cols <- character(0)
    if (isTRUE(return.unmatched) && "last_matched" %in% names(df)) flag_cols <- c(flag_cols, "last_matched")
    if (grepl("first", namesToUse)) {
      prob_cols <- c(prob_cols, paste0(p_eth, "_first"))
      name_cols <- c(name_cols, "firstname.match")
      if (isTRUE(return.unmatched) && "first_matched" %in% names(df)) flag_cols <- c(flag_cols, "first_matched")
    }
    if (grepl("middle", namesToUse)) {
      prob_cols <- c(prob_cols, paste0(p_eth, "_middle"))
      name_cols <- c(name_cols, "middlename.match")
      if (isTRUE(return.unmatched) && "middle_matched" %in% names(df)) flag_cols <- c(flag_cols, "middle_matched")
    }
    cols <- c(names(voter.file), name_cols, flag_cols, prob_cols)
    df[, cols, drop = FALSE]
  }
  
  if (namesToUse == "surname" &&
      sum(!(df$lastname.upper %in% lastNameDict$last_name)) == 0) {
    return(fast_return())
  }
  if (namesToUse == "surname, first" &&
      sum(!(df$lastname.match   %in% lastNameDict$last_name))  == 0 &&
      sum(!(df$firstname.upper %in% firstNameDict$first_name)) == 0) {
    return(fast_return())
  }
  if (namesToUse == "surname, first, middle" &&
      sum(!(df$lastname.match    %in% lastNameDict$last_name))   == 0 &&
      sum(!(df$firstname.upper   %in% firstNameDict$first_name))  == 0 &&
      sum(!(df$middlename.upper  %in% middleNameDict$middle_name))== 0) {
    return(fast_return())
  }
  
  # --- cleaning / fallback passes (dimension-safe) --------------------------
  if (clean.names) {
    for (nameType0 in strsplit(namesToUse, ", ")[[1]]) {
      nameType <- if (nameType0 == "surname") "last" else nameType0
      
      col_prob  <- paste0("c_whi_", nameType)
      col_match <- paste0(nameType, "name.match")
      col_upper <- paste0(nameType, "name.upper")
      
      df1 <- df[!is.na(df[, col_prob]), , drop = FALSE]
      df2 <- df[ is.na(df[, col_prob]), , drop = FALSE]
      
      ## 1) strip punctuation
      if (nrow(df2) > 0) {
        df2[, col_match] <- gsub("[^[:alnum:] ]", "", df2[, col_upper])
        df2 <- merge(
          df2[, !grepl(paste0("_", nameType), names(df2))],
          nameDict[[nameType]],
          all.x = TRUE,
          by.x = col_match, by.y = paste0(nameType, "_name"),
          sort = FALSE
        )
        df2 <- df2[, names(df1), drop = FALSE]
        
        keep <- !is.na(df2[, col_prob])
        if (any(keep)) {
          df1 <- rbind(df1, df2[keep, , drop = FALSE])
          df2 <- df2[!keep, , drop = FALSE]
        }
      }
      
      ## 2) strip spaces
      if (nrow(df2) > 0) {
        df2[, col_match] <- gsub(" ", "", df2[, col_match])
        df2 <- merge(
          df2[, !grepl(paste0("_", nameType), names(df2))],
          nameDict[[nameType]],
          all.x = TRUE,
          by.x = col_match, by.y = paste0(nameType, "_name"),
          sort = FALSE
        )
        df2 <- df2[, names(df1), drop = FALSE]
        
        keep <- !is.na(df2[, col_prob])
        if (any(keep)) {
          df1 <- rbind(df1, df2[keep, , drop = FALSE])
          df2 <- df2[!keep, , drop = FALSE]
        }
      }
      
      ## 3) last-name suffix cleanup
      if (nameType == "last" && nrow(df2) > 0) {
        suffix <- c("JUNIOR","SENIOR","THIRD","III","JR"," II"," J R"," S R"," IV")
        for (suf in suffix) {
          df2$lastname.match <- ifelse(
            substr(df2$lastname.match,
                   nchar(df2$lastname.match) - (nchar(suf) - 1),
                   nchar(df2$lastname.match)) == suf,
            substr(df2$lastname.match, 1, nchar(df2$lastname.match) - nchar(suf)),
            df2$lastname.match
          )
        }
        df2$lastname.match <- ifelse(
          nchar(df2$lastname.match) >= 7 &
            substr(df2$lastname.match, nchar(df2$lastname.match) - 1, nchar(df2$lastname.match)) == "SR",
          substr(df2$lastname.match, 1, nchar(df2$lastname.match) - 2),
          df2$lastname.match
        )
        
        df2 <- merge(
          df2[, !grepl(paste0("_", nameType), names(df2))],
          lastNameDict,
          by.x = "lastname.match", by.y = "last_name",
          all.x = TRUE, sort = FALSE
        )
        df2 <- df2[, names(df1), drop = FALSE]
        
        keep <- !is.na(df2[, col_prob])
        if (any(keep)) {
          df1 <- rbind(df1, df2[keep, , drop = FALSE])
          df2 <- df2[!keep, , drop = FALSE]
        }
      }
      
      ## 4) double-barreled (first half then second half)
      if (nrow(df2) > 0) {
        df2$name2 <- df2$name1 <- NA
        
        # hyphen split
        idx <- grep("-", df2[, col_upper])
        if (length(idx)) {
          parts <- strsplit(df2[idx, col_upper], "-")
          df2$name1[idx] <- vapply(parts, `[`, character(1), 1)
          df2$name2[idx] <- vapply(parts, `[`, character(1), 2)
        }
        # space split (fill only NAs left by hyphen step)
        idx <- grep(" ", df2[, col_upper])
        if (length(idx)) {
          parts <- strsplit(df2[idx, col_upper], " ")
          fill1 <- vapply(parts, `[`, character(1), 1)
          fill2 <- vapply(parts, `[`, character(1), 2)
          df2$name1[idx] <- ifelse(is.na(df2$name1[idx]), fill1, df2$name1[idx])
          df2$name2[idx] <- ifelse(is.na(df2$name2[idx]), fill2, df2$name2[idx])
        }
        
        # first half
        df2[, col_match] <- as.character(df2$name1)
        df2 <- merge(
          df2[, !grepl(paste0("_", nameType), names(df2))],
          nameDict[[nameType]],
          all.x = TRUE,
          by.x = col_match, by.y = paste0(nameType, "_name"),
          sort = FALSE
        )
        df2 <- df2[, c(names(df1), "name1", "name2"), drop = FALSE]
        
        keep <- !is.na(df2[, col_prob])
        if (any(keep)) {
          df1 <- rbind(df1, df2[keep, !(names(df2) %in% c("name1","name2")), drop = FALSE])
          df2 <- df2[!keep, , drop = FALSE]
        }
      }
      
      if (nrow(df2) > 0) {
        # second half
        df2[, col_match] <- as.character(df2$name2)
        df2 <- merge(
          df2[, !grepl(paste0("_", nameType), names(df2))],
          nameDict[[nameType]],
          all.x = TRUE,
          by.x = col_match, by.y = paste0(nameType, "_name"),
          sort = FALSE
        )
        df2 <- df2[, c(names(df1), "name1", "name2"), drop = FALSE]
        
        keep <- !is.na(df2[, col_prob])
        if (any(keep)) {
          df1 <- rbind(df1, df2[keep, !(names(df2) %in% c("name1","name2")), drop = FALSE])
          df2 <- df2[!keep, , drop = FALSE]
        }
      }
      
      # stitch back
      if (nrow(df2) > 0) {
        df <- rbind(df1, df2[, !(names(df2) %in% c("name1","name2")), drop = FALSE])
      } else {
        df <- df1
      }
    }
  }
  
  # --- reporting -------------------------------------------------------------
  c_miss_last <- mean(is.na(df$c_whi_last))
  if (c_miss_last > 0) {
    message(paste0(sum(is.na(df$c_whi_last)), " (", round(100 * c_miss_last, 1), "%) individuals' last names were not matched."))
  }
  if (grepl("first", namesToUse)) {
    c_miss_first <- mean(is.na(df$c_whi_first))
    if (c_miss_first > 0) {
      message(paste0(sum(is.na(df$c_whi_first)), " (", round(100 * c_miss_first, 1), "%) individuals' first names were not matched."))
    }
  }
  if (grepl("middle", namesToUse)) {
    c_miss_mid <- mean(is.na(df$c_whi_middle))
    if (c_miss_mid > 0) {
      message(paste0(sum(is.na(df$c_whi_middle)), " (", round(100 * c_miss_mid, 1), "%) individuals' middle names were not matched."))
    }
  }
  
  # flags BEFORE imputation
  if (isTRUE(return.unmatched)) {
    if ("c_whi_last"   %in% names(df)) df$last_matched   <- !is.na(df$c_whi_last)
    if ("c_whi_first"  %in% names(df)) df$first_matched  <- !is.na(df$c_whi_first)
    if ("c_whi_middle" %in% names(df)) df$middle_matched <- !is.na(df$c_whi_middle)
  }
  
  # --- imputation ------------------------------------------------------------
  if (impute.missing) {
    impute.vec <- colMeans(df[, grep("^c_", names(df), value = TRUE), drop = FALSE], na.rm = TRUE)
    for (i in grep("^c_", names(df), value = TRUE)) {
      df[, i] <- dplyr::coalesce(df[, i], impute.vec[i])
    }
  } else {
    for (i in grep("^c_", names(df), value = TRUE)) {
      df[, i] <- dplyr::coalesce(df[, i], 1)
    }
  }
  
  # --- unified return --------------------------------------------------------
  prob_cols <- c(paste0(p_eth, "_last"))
  name_cols <- c("lastname.match")
  flag_cols <- character(0)
  if (isTRUE(return.unmatched) && "last_matched" %in% names(df)) flag_cols <- c("last_matched", flag_cols)
  
  if (grepl("first",  namesToUse)) {
    prob_cols <- c(prob_cols, paste0(p_eth, "_first"))
    name_cols <- c(name_cols, "firstname.match")
    if (isTRUE(return.unmatched) && "first_matched" %in% names(df))  flag_cols <- c(flag_cols, "first_matched")
  }
  if (grepl("middle", namesToUse)) {
    prob_cols <- c(prob_cols, paste0(p_eth, "_middle"))
    name_cols <- c(name_cols, "middlename.match")
    if (isTRUE(return.unmatched) && "middle_matched" %in% names(df)) flag_cols <- c(flag_cols, "middle_matched")
  }
  
  cols <- c(names(voter.file), name_cols, flag_cols, prob_cols)
  df[, cols, drop = FALSE]
}

