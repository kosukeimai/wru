.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "Please cite as:", "\n\n",
    format(utils::citation("wru"), style = "text"), "\n\n",
    "Note that wru 2.0.0 uses 2020 census data by default.", "\n",
    'Use the argument `year = "2010"`, to replicate analyses produced with earlier package versions.',
    "\n"
  )
}
