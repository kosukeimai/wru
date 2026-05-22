#' Read raw name dictionaries from disk
#'
#' Downloads (via [wru_data_preflight()]) and reads the raw first, middle, and last
#' name dictionaries. The Census first-name dictionary exists only for 2020; for
#' other years (or until it is published) it is returned as \code{NULL}. This is the
#' single I/O seam so dictionary-selection logic can be tested without network access.
#'
#' @param year Census vintage, \code{"2020"} (default) or \code{"2010"}.
#' @return A named list: \code{census_last}, \code{last}, \code{first}, \code{mid},
#'   and \code{census_first} (NULL when unavailable).
#' @keywords internal
read_name_dictionaries <- function(year = "2020") {
  wru_data_preflight()
  path <- ifelse(getOption("wru_data_wd", default = FALSE), getwd(), tempdir())
  rd <- function(f) readRDS(file.path(path, f))
  cf_path <- file.path(path, "wru-data-census_first_c.rds")
  list(
    census_last  = rd("wru-data-census_last_c.rds"),
    last         = rd("wru-data-last_c.rds"),
    first        = rd("wru-data-first_c.rds"),
    mid          = rd("wru-data-mid_c.rds"),
    census_first = if (identical(as.character(year), "2020") && file.exists(cf_path)) {
      readRDS(cf_path)
    } else {
      NULL
    }
  )
}

.apply_dict_override <- function(tbl, base) {
  names(tbl) <- names(base)
  tbl[is.na(tbl)] <- 0
  tbl
}

.drop_source_col <- function(d) {
  if (!is.null(d)) d[["source"]] <- NULL
  d
}

#' Load and assemble the name dictionaries for merge_names()
#'
#' Selects and stacks the first, middle, and last dictionaries according to
#' \code{name_source} and \code{year}. When \code{name_source} is \code{NULL} the
#' legacy \code{census.surname} behavior is used (Census-or-augmented surnames,
#' voter-file first/middle). \code{name_source = "census_only"} has no Census
#' middle-name dictionary and errors if middle names are requested.
#'
#' @param namesToUse Which names are in play, e.g. \code{"surname, first"}.
#' @param name_source One of \code{"mixed"}, \code{"census_only"}, \code{"vf_only"},
#'   or \code{NULL} for the legacy \code{census.surname} path.
#' @param year Census vintage passed to [read_name_dictionaries()].
#' @param census.surname Legacy flag, used only when \code{name_source} is \code{NULL}.
#' @param table.surnames,table.first,table.middle Optional user dictionaries.
#' @return A named list with elements \code{first}, \code{middle}, and \code{last}.
#' @keywords internal
load_name_dictionaries <- function(namesToUse, name_source = NULL, year = "2020",
                                   census.surname = TRUE,
                                   table.surnames = NULL, table.first = NULL,
                                   table.middle = NULL) {
  if (!is.null(name_source) && name_source == "census_only" && grepl("middle", namesToUse)) {
    stop("name_source = \"census_only\" has no Census middle-name dictionary; ",
         "use \"mixed\" or \"vf_only\", or drop middle names.")
  }

  raw <- read_name_dictionaries(year)

  if (is.null(name_source)) {
    lastNameDict <- if (census.surname) raw$census_last else raw$last
  } else {
    lastNameDict <- .drop_source_col(
      stack_name_dictionary(raw$census_last, raw$last, name_source, "last_name")
    )
  }
  if (!is.null(table.surnames)) {
    lastNameDict <- .apply_dict_override(table.surnames, raw$last)
  }

  firstNameDict <- NULL
  if (grepl("first", namesToUse)) {
    firstNameDict <- if (is.null(name_source)) {
      raw$first
    } else {
      .drop_source_col(
        stack_name_dictionary(raw$census_first, raw$first, name_source, "first_name")
      )
    }
    if (!is.null(table.first)) {
      firstNameDict <- .apply_dict_override(table.first, raw$first)
    }
  }

  middleNameDict <- NULL
  if (grepl("middle", namesToUse)) {
    middleNameDict <- raw$mid
    if (!is.null(table.middle)) {
      middleNameDict <- .apply_dict_override(table.middle, raw$mid)
    }
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
    if (is.null(census)) {
      vf$source <- "vf"
      rownames(vf) <- NULL
      return(vf)
    }
    if (is.null(vf)) {
      census$source <- "census"
      rownames(census) <- NULL
      return(census)
    }
    census$source <- "census"
    vf$source <- "vf"
    vf_only <- vf[!(vf[[key]] %in% census[[key]]), names(census), drop = FALSE]
    out <- rbind(census, vf_only)
    rownames(out) <- NULL
    return(out)
  }
  stop("Unknown name_source: ", name_source)
}
