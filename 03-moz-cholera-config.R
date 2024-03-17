# for R shiny to run
tool_name <- "moz-cholera"


# project data directories
data_dir <- here("data")
output_dir <- here("output")
rmd_dir <- here("notebook")
img_dir <- here("images")
plot_dir <- here("plots")
rda_dir <- here("rdas")
dataset_file <- list.files(here("data"), pattern = "^[a-zA-Z0-9_].*")
dataset_path <- here("data", dataset_file)

# Regex pattern
pattern <- "^([^<]+)\\s*\\(.*" # to extract and clean district names

# Export directory
export_dir <- output_dir

# Fonts
font_name <- "Univers LT Std"
font_add(font_name, regular = UNICEF_FONTS[font_name]) 
showtext_auto()

if (!file.exists(paste(rda_dir, "dataload.RData", sep = "/"))) {
  # maps data
  gis_moz_file <- list.files(here("_gis", "moz", "geopackages"), pattern = "^[a-zA-Z0-9_].*")
  gis_moz_path <- here("_gis", "moz", "geopackages", gis_moz_file)
  gis_moz_db <- gis_moz_path[grep(paste0("*.database.*"), gis_moz_path)]
  
  gis_africa_path <- here("_gis", "africa", "geopackages")
  gis_africa_file <- list.files(gis_africa_path, pattern = "^[a-zA-Z0-9_].*")
  gis_africa_db <- here(gis_africa_path, gis_africa_file)
  
  gis_mwi_path <- here("_gis", "mwi", "shapefiles", "admin")
  gis_zwe_path <- here("_gis", "zwe", "shapefiles", "admin")
  gis_zmb_path <- here("_gis", "zmb", "shapefiles", "admin")
  
  admin_path <- here("_data","moz", "cods")
  admin_file <- list.files(admin_path, pattern = "^[a-zA-Z0-9_].*")
  admin_dataset_path <- here(admin_path, admin_file)
}
