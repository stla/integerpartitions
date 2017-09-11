.onLoad <- function(libname, pkgname) {
  library.dynam("Partitions", pkgname, libname, now=TRUE)
  .C("HsStart")
  invisible()
}

.onUnLoad <- function(libpath) {
  library.dynam.unload("Partitions", libpath)
  invisible()
}
