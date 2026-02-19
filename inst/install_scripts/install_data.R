#inst/instakl_scripts/install_data.R
library(remotes)

Downloading_maps <- function() {
  url <- "https://zenodo.org/records/18630660/files/Grids_CIP_2025.gpkg?download=1"
  destino <- system.file("extdata", "Grids_CIP_2025.gpkg", package = "CelticInvasivePlantsdb")
  
  if (!file.exists(destino)) {
    message("Downoloading grid map...")
    download.file(url, destino, mode = "wb")
    message("Grid map successfully downloaded")
  }
  
  sf::st_read(destino)
}
