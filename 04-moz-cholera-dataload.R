# IMPORT DATA -------

if (!file.exists(paste(rda_dir, "dataload.RData", sep = "/"))) {

  admin1_data <- read.xlsx(admin_dataset_path, sheet = 2, check.names = TRUE)
  admin2_data <- read.xlsx(admin_dataset_path, sheet = 3, check.names = TRUE)

  ## data for maps -----
  cholera_data_path <- dataset_path[grep("*.Overview.*", dataset_path)]
  cholera_data <- read.xlsx(cholera_data_path, sheet = 1, check.names = T, startRow = 9) %>% 
    rename_with(~c("province", "district", "status", "population", "zq", "zar", "cases_june_2023", "deaths_june_2023", "cases_sep_2023", "deaths_sep_2023", "awd_flag", "cholera_cases_flag", "cholera_declared_flag", "no_active_flag")
                , .cols = 2:15) %>% 
    filter(!grepl("Currently", X1)) %>% 
    select(-X1)
  
  ## summary data -----
  
  #summary_data_path <- dataset_path[grep(paste0("*.summary.*"), dataset_path)]
  
  summary_data_path <- dataset_path[grep(paste0("*.resumo.*"), dataset_path)]
  
  # Use map-reduce to read each file and reduce to combine all dataframes
  summary_data <- summary_data_path %>% 
    map(function(path){
      sheet_names <- getSheetNames(path)
      week_sheet <- sheet_names[startsWith(sheet_names, "Week")]
      read.xlsx(path, sheet = week_sheet, check.names = F, startRow = 2, skipEmptyCols = FALSE) %>% 
        select(2:10) %>% 
        rename_with(~c("province", "district"), .cols=1:2) %>% 
        fill(province, .direction = "down")
    }) %>% 
    reduce(bind_rows) %>% 
    filter(!startsWith(district, "Total")) %>% 
    distinct()  # Remove duplicate rows
    # mutate(district = ifelse(grepl("\\*$", district), 
    #                          substr(district, 1, nchar(district) - 1), 
    #                          district))
  
  ## Last bulletin data -----
  
  #bulletin_data_path <- dataset_path[grep("*.DVS.*", dataset_path)]
  #bulletin_data_path <- max(summary_data_path)
  
  bulletin_data_path <- data.frame(summary_data_path) %>%
    mutate(date_string = gsub(".*_([0-9]{2}\\.[0-9]{2}\\.[0-9]{2}) .*", "\\1", summary_data_path)) %>% 
    mutate(date = dmy(date_string)) %>% 
    arrange(desc(date)) %>%
    slice(1)
  
  # Use regular expression to extract the date part
  #date_str <- sub(".*_(\\d{4})(\\d{2})(\\d{2}).xlsx$", "\\1-\\2-\\3", bulletin_data_path)
  # Convert the extracted string to a Date object
  #bulletin_date <- as.Date(date_str, format="%Y-%m-%d")
  bulletin_date <- bulletin_data_path$date
    
  bulletin_data <- read.xlsx(bulletin_data_path$summary_data_path, sheet = 1, check.names = T, startRow = 3, cols = 2:8) %>% 
    select(X1, X2, Casos, Óbitos) %>% 
    rename_with(~c("province", "district", "cases_now", "deaths_now"), .cols = everything()) %>% 
    fill(province, .direction = "down") %>%  # to fill with the last non-NA value above.
    filter(!grepl("total", district, ignore.case = TRUE)) # remove Total rows
  
  
  # get active cases data and health system burden
  bulletin_active_data <- read.xlsx(bulletin_data_path$summary_data_path, sheet = 4, check.names = T, startRow = 3, cols = 2:19) %>% 
    select(X1, X2, Casos, X13:X15,X17:18) %>% 
    rename_with(~c("province", "district", "cases_now", "hospitalized", "capacity","occupancy_rate","population", "overall_incidence_rate"), .cols = everything()) %>% 
    fill(province, .direction = "down") %>%  # to fill with the last non-NA value above.
    filter(!grepl("total", district, ignore.case = TRUE)) %>% 
    distinct()  %>% # Remove duplicate rows
    filter(!grepl("surto|fim", district, ignore.case = TRUE))
  
  ## WASH data -----
  
  sanitation_data <- dataset_path[grep(paste0("*.sanitation.*"), dataset_path, ignore.case = T)] %>% 
    read.xlsx(sheet = 5, check.names = T) 
    
  water_data <- dataset_path[grep(paste0("*.water.*"), dataset_path, ignore.case = T)] %>% 
    read.xlsx(sheet = 1, check.names = T) 
  
  
  ## MINISTRY SUMMARY data -----
  
  ministry_data <- dataset_path[grep(paste0("*.ministro.*"), dataset_path, ignore.case = T)] %>% 
    read.xlsx(sheet = 4, check.names = T, startRow = 2) %>% 
    rename_with(~c("province", "district"), .cols = X1:X2)
  
  ## Partners presence data
  
  partners_data <- dataset_path[grep(paste0("*.partnership.*"), dataset_path, ignore.case = T)] %>% 
    read.xlsx(sheet = "Partners", check.names = T) %>% 
    select(-last_col()) %>% 
    rename_with(~c("province", "district"), .cols = c(1,3)) %>% 
    fill(province, .direction = "down")
  
  ## Save all dataframes
  save(
    admin1_data, 
    admin2_data, 
    cholera_data, 
    summary_data, 
    bulletin_data_path, 
    bulletin_date, 
    bulletin_data, 
    bulletin_active_data, 
    sanitation_data, 
    water_data, 
    ministry_data, 
    partners_data, 
    file = paste(rda_dir, "dataload.RData", sep = "/"))
  
} else {
  
  load(file = paste(rda_dir, "dataload.RData", sep = "/"), verbose = F)

  summary_data_path <- dataset_path[grep(paste0("*.resumo.*"), dataset_path)]
  
  # Use map-reduce to read each file and reduce to combine all dataframes
  if (length(summary_data_path) > 0 ) {
    
    summary_data <- summary_data_path %>% 
      map(function(path){
        sheet_names <- getSheetNames(path)
        week_sheet <- sheet_names[startsWith(sheet_names, "Week")]
        read.xlsx(path, sheet = week_sheet, check.names = F, startRow = 2, skipEmptyCols = FALSE) %>% 
          select(2:10) %>% 
          rename_with(~c("province", "district"), .cols=1:2) %>% 
          fill(province, .direction = "down")
      }) %>% 
      reduce(bind_rows) %>% 
      filter(!startsWith(district, "Total")) %>% 
      bind_rows(summary_data) %>% 
      distinct()  # Remove duplicate rows
  
    bulletin_data_path <- data.frame(summary_data_path) %>%
      mutate(date_string = gsub(".*_([0-9]{2}\\.[0-9]{2}\\.[0-9]{2}) .*", "\\1", summary_data_path)) %>% 
      mutate(date = dmy(date_string)) %>% 
      arrange(desc(date)) %>%
      slice(1)
    
    # get latest bulletin date
    bulletin_date <- bulletin_data_path$date
    
    # get bulletin data
    bulletin_data <- read.xlsx(bulletin_data_path$summary_data_path, sheet = 1, check.names = T, startRow = 3, cols = 2:8) %>% 
      select(X1, X2, Casos, Óbitos) %>% 
      rename_with(~c("province", "district", "cases_now", "deaths_now"), .cols = everything()) %>% 
      fill(province, .direction = "down") %>%  # to fill with the last non-NA value above.
      filter(!grepl("total", district, ignore.case = TRUE)) # remove Total rows
    
    # get active cases data and health system burden
    bulletin_active_data <- read.xlsx(bulletin_data_path$summary_data_path, sheet = 4, check.names = T, startRow = 3, cols = 2:19) %>% 
      select(X1, X2, Casos, X13:X15,X17:18) %>% 
      rename_with(~c("province", "district", "cases_now", "hospitalized", "capacity","occupancy_rate","population", "overall_incidence_rate"), .cols = everything()) %>% 
      fill(province, .direction = "down") %>%  # to fill with the last non-NA value above.
      filter(!grepl("total", district, ignore.case = TRUE)) %>% 
      distinct()  %>% # Remove duplicate rows
      filter(!grepl("surto|fim", district, ignore.case = TRUE))
    
    # move all existing bulletin files
    current_path <- summary_data_path
    new_path <- str_replace(current_path, 'data/', 'data/_bak/_')
    file_move_result <- file.rename(summary_data_path, new_path)
  }
  
  partners_data <- dataset_path[grep(paste0("*.partnership.*"), dataset_path, ignore.case = T)] %>% 
    read.xlsx(sheet = "Partners", check.names = T) %>% 
    select(-last_col()) %>% 
    rename_with(~c("province", "district"), .cols = c(1,3)) %>% 
    fill(province, .direction = "down")
  
  ## Save all dataframes
  save(
    admin1_data, 
    admin2_data, 
    cholera_data, 
    summary_data, 
    bulletin_data_path, 
    bulletin_date, 
    bulletin_data, 
    bulletin_active_data, 
    sanitation_data, 
    water_data, 
    ministry_data, 
    partners_data, 
    file = paste(rda_dir, "dataload.RData", sep = "/"))
  
}

# IMPORT GIS DATA -----

## Basemap layers -----
if (!file.exists(paste(rda_dir, "dataload.RData", sep = "/"))) {
  # get layers
  db_moz_layers <- st_layers(gis_moz_db)
  db_moz_layers$name[db_moz_layers$name == "admin2"]
  
  # get adm2 layer
  adm1_map <- st_read(gis_moz_db , layer = db_moz_layers$name[2],  quiet = TRUE)
  adm2_map <- st_read(gis_moz_db , layer = db_moz_layers$name[3],  quiet = TRUE)
  
  # get ESA boundaries layers
  db_africa_layers <- st_layers(gis_africa_db)
  
  # get MWI boundaries layers
  db_mwi_layers <- st_layers(gis_mwi_path)
  
  # get ZWE boundaries layers
  db_zwe_layers <- st_layers(gis_zwe_path)
  
  # get ZMB boundaries layers
  db_zmb_layers <- st_layers(gis_zmb_path)
  
  # get ESA_region layer
  africa_adm0_map <- st_read(gis_africa_db, layer = "afr_g2014_2013_0",  quiet = TRUE) %>%
    filter(ISO3 %in% c("TZA", "ZAF", "ZWE", "ZMB", "SWZ", "MWI")) %>%
    mutate(adm0_name_wrap = str_to_upper(str_wrap(ADM0_NAME, width = 6))) %>%
    mutate(adm0_name_wrap = str_to_upper(ADM0_NAME)) %>%
    mutate(adm0_name_wrap = case_when(
      ADM0_NAME == "Swaziland" ~ "ESWATINI",
      ADM0_NAME == "Tanzania" ~ "UNITED REPUBLIC OF TANZANIA",
      .default = adm0_name_wrap))
  
  # get MWI adm2 layer
  mwi_adm2_map <- st_read(gis_mwi_path, layer = "mwi_admbnda_adm2_nso_hotosm_20230405",  quiet = TRUE) %>% 
    filter(ADM2_EN %in% c("Mangochi", "Machinga", "Phalombe", "Mulanje", "Thyolo", "Chikwawa", "Mwanza", "Neno", "Ntcheu", "Dedza", "Lilongwe", "Mchinji", "Nsanje","Zomba"))
  
  # get ZWE adm2 layer
  zwe_adm2_map <- st_read(gis_zwe_path, layer = "zwe_admbnda_adm2_zimstat_ocha_20180911",  quiet = TRUE) %>% 
    filter(ADM2_EN %in% c("Mbire", "Centenary/ Muzarabani", "Mount Darwin", "Rushinga", "Mudzi", "Nyanga", "Mutasa", "Mutare", "Mutare Urban", "Chimanimani", "Chipinge", "Chiredzi"))
  
  # get ZMB adm2 layer
  zmb_adm2_map <- st_read(gis_zmb_path, layer = "zmb_admbnda_adm2_dmmu_20201124",  quiet = TRUE) %>% 
    filter(ADM2_EN %in% c("Luangwa", "Rufunsa", "Nyimba", "Petauke", "Sinda", "Katete", "Chadiza", "Vubwi"))
  
  ## Save all GIS common data
  save(
    adm1_map, 
    adm2_map, 
    africa_adm0_map, 
    mwi_adm2_map, 
    zwe_adm2_map, 
    zmb_adm2_map, 
    file = paste(rda_dir, "gis_dataload.RData", sep = "/"))
  
} else {
  
  load(file = paste(rda_dir, "gis_dataload.RData", sep = "/"), verbose = F)
  
}
