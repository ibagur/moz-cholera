list_of_packages <- package_list <- c("here", "tidyverse", "openxlsx", "sf", "tmap", "leaflegend",  "plotly", "rsconnect", "extrafont", "fuzzyjoin", "showtext", "webshot2", "ggrepel", "reactable", "reactablefmtr")

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)



# load here library to handle relative paths from project ROOT
library(here)

source(here("R", "_common", "required.R"))
source(here("R", "_common", "required-plot.R"))
source(here("R", "_common", "utils.R"))
source(here("R", "_common", "config.R"))
#source(here("R", "_common", "constants.R"))
#source(here("R", "_common", "pcode-functions.R"))
#source(here("R", "_common", "functions.R"))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", "00-init.R"))

if(tool_name != "_template") {
  source(here("R", tool_name, paste0("01-", tool_name, "-required.R")))
  source(here("R", tool_name, paste0("02-", tool_name, "-functions.R")))
  source(here("R", tool_name, paste0("03-", tool_name, "-config.R")))
  source(here("R", tool_name, paste0("04-", tool_name, "-dataload.R")))
  source(here("R", tool_name, paste0("05-", tool_name, "-process.R")))
  #source(here("R", tool_name, paste0("06-", tool_name, "-analysis.R")))
  source(here("R", tool_name, paste0("07-", tool_name, "-plot.R")))
  #source(here("R", tool_name, paste0("08-", tool_name, "-output.R")))
}