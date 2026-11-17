#' Pre-process vector of names to match census style. Internal function
#'
#' @param voter_names Character vector to be pre-processed.
#' @param target_names Character vector of census names to be matched.
#'
#' @return A character vector of pre-processed named
#' @keywords internal
#'

.name_preproc <- function(voter_names, target_names){
  orig_names <- toupper(as.character(voter_names))
  post_names <- orig_names
  ## 1) Raw match
  match_tmp <- post_names %in% target_names
  ## 2) remove punctuation
  post_names[!match_tmp] <- gsub("[^[:alnum:] ]", "",
                                 post_names[!match_tmp])
  match_tmp <- post_names %in% target_names
  ## 3) Remove spaces
  post_names[!match_tmp] <- gsub(" ", "",
                                 post_names[!match_tmp])
  match_tmp <- post_names %in% target_names
  ## 4) Remove suffixes
  suffix <- c("JUNIOR", "SENIOR", "THIRD", "III", "JR", " II", " J R", " S R", " IV")
  for (i in seq_along(suffix)) {
    post_names[!match_tmp] <- ifelse(substr(post_names[!match_tmp],
                                       nchar(post_names[!match_tmp]) - (nchar(suffix)[i] - 1),
                                       nchar(post_names[!match_tmp])) == suffix[i], 
                                substr(post_names[!match_tmp], 1, nchar(post_names[!match_tmp]) - nchar(suffix)[i]), 
                                post_names[!match_tmp])
  }
  post_names[!match_tmp] <- ifelse(nchar(post_names[!match_tmp]) >= 7, 
                                   ifelse(substr(post_names[!match_tmp], 
                                                 nchar(post_names[!match_tmp]) - 1,
                                                 nchar(post_names[!match_tmp])) == "SR", 
                                          substr(post_names[!match_tmp], 1, nchar(post_names[!match_tmp]) - 2), 
                                          post_names[!match_tmp]), 
                                   post_names[!match_tmp]) #Remove "SR" only if name has at least 7 characters
  match_tmp <- post_names %in% target_names
  ## 5) Split double-barreled names, match on first part
  has_delim <- !match_tmp & !is.na(orig_names) & grepl("[-, ]", orig_names)
  if (any(has_delim)) {
    post_names_tmp <- post_names
    post_names_tmp[has_delim] <- vapply(
      strsplit(orig_names[has_delim], "[-, ]"),
      `[`, character(1), 1L
    )
    match_tmp_2 <- post_names_tmp %in% target_names
    post_names[match_tmp_2 & !match_tmp] <- post_names_tmp[match_tmp_2 & !match_tmp]
    match_tmp <- post_names %in% target_names
  }
  ## 6) Split double-barreled names, match on second part
  has_delim <- !match_tmp & !is.na(orig_names) & grepl("[-, ]", orig_names)
  if (any(has_delim)) {
    post_names_tmp <- post_names
    post_names_tmp[has_delim] <- vapply(
      strsplit(orig_names[has_delim], "[-, ]"),
      `[`, character(1), 2L
    )
    match_tmp_2 <- post_names_tmp %in% target_names
    post_names[match_tmp_2 & !match_tmp] <- post_names_tmp[match_tmp_2 & !match_tmp]
  }
  return(unlist(post_names))
}