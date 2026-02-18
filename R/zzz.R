
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to the Celtic Invasive Plants database. Version", packageVersion("CelticInvasivePlantsdb"), "!"))
}


.onLoad <- function(libname, pkgname) {
  file_path <- system.file("extdata", "Grids_CIP_2025.gpkg", package = pkgname)
  if (!file.exists(file_path)) {
    stop(paste("No se pudo encontrar el archivo", file_path))
  }
  message(paste("El archivo existe:", file_path))
}


.onLoad <- function(libname, pkgname) {
  if (!file.exists(system.file("extdata", "Grids_CIP_2025.gpkg", package = pkgname))) {
    git2r::lfs_pull(repo = ".")
  }
  # resto del código
}
