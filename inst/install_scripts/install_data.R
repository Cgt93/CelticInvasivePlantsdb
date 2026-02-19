#inst/instakl_scripts/install_data.R
library(remotes)

data_file = "Grids_CIP_2025.gpkg"
data_path = file.path("inst/extdata", data_file)

if (!file.exists(data_path)) {
  url = "https:
  download.file(url, data_path, mode = "wb")
}
