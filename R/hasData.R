hasData <- function(has_data = .pkgenv$has_data) {
  if (!has_data) {
    msg <- paste("To use this function, you must have the",
                 "`wru-data` package installed. To install that ",
                 "package, run `install.packages('wru-data',",
                 "repos='https://geanders.github.io/drat/', type='source')")
    msg <- paste(strwrap(msg), collapse="\n")
    stop(msg)
  }
}