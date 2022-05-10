.onUnload <- function (libpath) {
  library.dynam.unload("wru", libpath)
}