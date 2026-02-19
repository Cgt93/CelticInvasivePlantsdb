if (!file.exists(system.file("extdata", "Grids_CIP_2025.gpkg", package = "CelticInvasivePlantsdb"))) {
  url <- "https://zenodo.org/records/18630660/files/Grids_CIP_2025.gpkg?download=1"
  destino <- system.file("extdata", "Grids_CIP_2025.gpkg", package = "CelticInvasivePlantsdb")
  message("Downoloading grid map...")
  download.file(url, destino, mode = "wb")
  message("Grid map successfully downloaded")
}
