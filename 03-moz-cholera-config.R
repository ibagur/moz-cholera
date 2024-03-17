# for R shiny to run
tool_name <- "moz-cholera"

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

# project data directories
data_dir <- here("R", tool_name, "data")
output_dir <- here("R", tool_name, "output")
rmd_dir <- here("R", tool_name, "notebook")
img_dir <- here("R", tool_name, "images")
plot_dir <- here("R", tool_name, "plots")
rda_dir <- here("R", tool_name, "rdas")
dataset_file <- list.files(here("R", tool_name, "data"), pattern = "^[a-zA-Z0-9_].*")
dataset_path <- here("R", tool_name, "data", dataset_file)

# Regex pattern
pattern <- "^([^<]+)\\s*\\(.*" # to extract and clean district names

# Export directory
export_dir <- "/Users/inigo/Library/CloudStorage/OneDrive-UNICEF/07 products/cholera/UNICEF cholera update/export"

# Fonts
font_name <- "Univers LT Std"
font_add(font_name, regular = UNICEF_FONTS[font_name]) 
showtext_auto()