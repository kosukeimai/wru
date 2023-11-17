.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\nPlease cite as:\n\n",
    format(citation("wru"), style = "text"),
    "\n"
  )
}
