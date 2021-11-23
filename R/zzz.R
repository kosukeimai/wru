.pkgenv <- new.env(parent=emptyenv()) 
.onLoad <- function(libname, pkgname) {
  has_data <- requireNamespace("wruData", quietly = TRUE)
  .pkgenv[["has_data"]] <- has_data 
}
.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$has_data) { 
    msg <- paste("To use this package, you must install the",
                 "wruData package. To install that ",
                 "package, run `install.packages('wruData',",
                 "repos='https://solivella.github.io/wruData/', type='source')`.")
    msg <- paste(strwrap(msg), collapse="\n")
    packageStartupMessage(msg)
  }
}

.hasData <- function(has_data = .pkgenv$has_data) { #7
  if (!has_data) {
    msg <- paste("To use this function, you must install the",
                 "wruData package. To install that ",
                 "package, run `install.packages('wruData',",
                 "repos='https://solivella.github.io/wruData/', type='source')`.")
    msg <- paste(strwrap(msg), collapse="\n")
    stop(msg)
  } 
}

.onUnload <- function (libpath) {
  library.dynam.unload("wru", libpath)
}