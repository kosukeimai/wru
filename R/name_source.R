#' Load the first, middle, and last name dictionaries
#'
#' Downloads (via [wru_data_preflight()]) and reads the name dictionaries used by
#' [merge_names()], applying any user-supplied dictionary overrides. Separated from
#' the merge logic so the latter can be tested without network access.
#'
#' @param census.surname If TRUE, load the Census-derived surname dictionary;
#'   otherwise the augmented surname dictionary.
#' @param table.surnames,table.first,table.middle Optional user-supplied dictionaries.
#' @return A named list with elements \code{first}, \code{middle}, and \code{last}.
#' @keywords internal
load_name_dictionaries <- function(census.surname,
                                   table.surnames = NULL,
                                   table.first = NULL,
                                   table.middle = NULL) {
  wru_data_preflight()
  path <- ifelse(getOption("wru_data_wd", default = FALSE), getwd(), tempdir())

  first_c <- readRDS(paste0(path, "/wru-data-first_c.rds"))
  mid_c <- readRDS(paste0(path, "/wru-data-mid_c.rds"))
  if (census.surname) {
    last_c <- readRDS(paste0(path, "/wru-data-census_last_c.rds"))
  } else {
    last_c <- readRDS(paste0(path, "/wru-data-last_c.rds"))
  }

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
    firstNameDict[is.na(firstNameDict)] <- 0
    names(firstNameDict) <- names(first_c)
  }
  if (is.null(table.middle)) {
    middleNameDict <- mid_c
  } else {
    middleNameDict <- table.middle
    middleNameDict[is.na(middleNameDict)] <- 0
    names(middleNameDict) <- names(mid_c)
  }

  list(first = firstNameDict, middle = middleNameDict, last = lastNameDict)
}

#' Select and stack name dictionaries by source
#'
#' Combines a Census-derived dictionary and a voter-file dictionary according to
#' \code{name_source}. For \code{"mixed"}, the two are unioned by \code{key} with
#' Census probabilities taking precedence on overlapping names and the voter-file
#' dictionary supplying the remaining names. A \code{source} column records the
#' provenance (\code{"census"} or \code{"vf"}) of each row.
#'
#' @param census A Census-derived dictionary (\code{key} + \code{c_*} columns), or NULL.
#' @param vf A voter-file dictionary with the same schema, or NULL.
#' @param name_source One of \code{"mixed"}, \code{"census_only"}, \code{"vf_only"}.
#' @param key The join column, e.g. \code{"last_name"} or \code{"first_name"}.
#' @return A data frame with \code{key}, the \code{c_*} columns, and a \code{source} column.
#' @keywords internal
stack_name_dictionary <- function(census = NULL, vf = NULL, name_source, key) {
  if (name_source == "census_only") {
    census$source <- "census"
    rownames(census) <- NULL
    return(census)
  }
  if (name_source == "vf_only") {
    vf$source <- "vf"
    rownames(vf) <- NULL
    return(vf)
  }
  if (name_source == "mixed") {
    census$source <- "census"
    vf$source <- "vf"
    vf_only <- vf[!(vf[[key]] %in% census[[key]]), names(census), drop = FALSE]
    out <- rbind(census, vf_only)
    rownames(out) <- NULL
    return(out)
  }
  stop("Unknown name_source: ", name_source)
}
