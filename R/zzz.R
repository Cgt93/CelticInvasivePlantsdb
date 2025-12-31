
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to the Celtic Invasive Plants database. Version", packageVersion("CelticInvasivePlantsdb"), "!"))
}

