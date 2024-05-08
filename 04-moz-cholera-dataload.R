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
  
  summary_data_path <- dataset_path[grep(paste0("*.DVS.*"), dataset_path)]
  
  # Multi-line comment explaining the entire code:
  # This code reads multiple Excel files from a list of file paths (summary_data_path).
  # For each file, it finds the sheet name that starts with "Week" and reads the data from that sheet,
  # skipping the first row and any empty columns.
  # It then selects columns 2 to 10, renames the first two columns to "province" and "district",
  # and fills down the "province" column to propagate values to missing cells.
  # The resulting data frames from each file are combined into a single data frame using bind_rows().
  # Finally, the code filters out rows where the "district" column starts with "Total" and removes any duplicate rows. 
  summary_data <- summary_data_path %>%
    map(function(path) {
      # Get the names of all sheets in the current Excel file
      sheet_names <- getSheetNames(path)
      # Find the sheet name that starts with "Week" (assuming it's the desired sheet)
      week_sheet <- sheet_names[startsWith(sheet_names, "Week")]
      # Read the data from the "Week" sheet, starting from the 2nd row and skipping empty columns
      read.xlsx(path, sheet = week_sheet, check.names = F, startRow = 2, skipEmptyCols = FALSE) %>%
        # Select columns 2 to 10
        select(2:10) %>%
        # Rename the first two columns to "province" and "district"
        rename_with(~c("province", "district"), .cols = 1:2) %>%
        # Fill down the "province" column to propagate values to missing cells
        fill(province, .direction = "down")
    }) %>%
    # Combine all the data frames from each file into a single data frame
    reduce(bind_rows) %>%
    # Filter out rows where the "district" column starts with "Total"
    filter(!startsWith(district, "Total")) %>%
    # Remove any duplicate rows
    distinct()
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
  load(file = paste(rda_dir, "aux_dataload.RData", sep = "/"), verbose = F)
  
  summary_data_path <- dataset_path[grep(paste0("*.DVS.*"), dataset_path)]
  extension <- tolower(tools::file_ext(summary_data_path))
  
  # Use map-reduce to read each file and reduce to combine all dataframes
  if (length(summary_data_path) > 0 ) {
    
    # check if file is PDF or Excel (assuming a single file)
    if(all(extension == "pdf")) {
      
      # Extract text from the PDF file
      txt <- pdf_text(summary_data_path)
      
      # Select pages 3 and 4 from the extracted text / or only page 3 if a 'activos' pdf is uploaded
      if (grepl("activos", summary_data_path)) {
        pdf_tbl <- txt[3]
        activos_pdf_flag = TRUE
      } else {
        pdf_tbl <- paste(txt[3], txt[4])
        activos_pdf_flag = FALSE
      }
      
      # Split the selected text into rows
      tmp <- strsplit(pdf_tbl, "\n")
      
      # Remove extra spaces and replace commas with periods in the rows
      district_tbl <- tmp[[1]] %>% 
        str_squish() %>% 
        str_replace_all(., ",", ".")
      
      # This code performs data cleaning and transformation on the `district_tbl` data frame.
      # It extracts the location information, matches it with administrative data, and
      # restructures the data frame for further analysis. The main steps include:
      # 1. Extracting location information and separating it from numeric data
      # 2. Cleaning and standardizing location names
      # 3. Handling cases where location data is spread across multiple rows
      # 4. Performing fuzzy matching of location names with administrative data
      # 5. Filtering and selecting relevant columns
      # 6. Joining with administrative data to add province information
      # 7. Renaming columns and converting data types
      # 8. Reordering columns
      
      df_match <- data.frame(district_tbl) %>%
        # Extract the location information from the 'district_tbl' column and remove it from the original column
        mutate(location_raw = str_extract(district_tbl, "^[^0-9]+"),
               district_tbl = str_replace(district_tbl, "^[^0-9]+", "")) %>%
        # Separate the 'district_tbl' column into 13 columns named 'X1' to 'X13'
        separate(district_tbl, into = paste0("X", 1:13), sep = "\\s+", remove = TRUE, fill = "right") %>% 
        # Trim whitespace from the 'location_raw' column
        mutate(location_raw = str_trim(location_raw)) %>% 
        # Remove specific patterns and words from the 'location_raw' column and store in 'location_tmp' # nolint
        mutate(location_tmp = str_replace(location_raw, "\\*|[Pp]rov[\\s]*[íi]ncia", "")) %>% 
        # Filter out rows where both 'location_raw' and 'X2' are missing
        filter(!(is.na(location_raw) & is.na(X2))) %>% 
        # Filter out rows containing the word "total" (case-insensitive) in 'location_raw'
        filter(!grepl("total", location_raw, ignore.case = TRUE)) %>% 
        # Replace specific city names with a standardized format in 'location_tmp'
        mutate(location_tmp = case_when(
          grepl("Matola", location_tmp, ignore.case=T) ~ str_replace_all(location_tmp, "(Matola)", "Cidade Da \\1"),
          grepl("Chimoio|Pemba|Lichinga|Quelimane|Xai-Xai", location_tmp, ignore.case=T) ~ str_replace_all(location_tmp, "(Chimoio|Pemba|Lichinga|Quelimane|Xai-Xai)", "Cidade De \\1"),
          .default = location_tmp
        )) %>%   
        # Handle cases where location data is spread across two consecutive rows
        mutate(location_tmp = case_when(
          !is.na(X13) & is.na(location_tmp) & is.na(lag(X13)) ~ lag(location_tmp),
          TRUE ~ location_tmp
        )) %>% 
        mutate(location_raw = case_when(
          !is.na(X13) & is.na(location_raw) & is.na(lag(X13)) ~ lag(location_raw),
          TRUE ~ location_raw
        )) %>% 
        # Handle situation with Maputo City an pdf row spread on two rows
        mutate(
          HasDecimal = grepl("\\d+\\.\\d+", X5) # Identify rows with decimals in X5
        ) %>%
        mutate(
          X13 = if_else(HasDecimal, "0.0", as.character(X13)), # Update X13 where HasDecimal is TRUE
          across(
            .cols = X1:X5,
            .fns = ~if_else(HasDecimal, lead(., default = last(.)), .),
            .names = "{.col}" # Preserve original column names
          )
        ) %>%
        mutate (X13 = if_else(grepl("ambique", location_tmp), "0.0", X13)) %>% 
        select(-HasDecimal) %>%         
        # Filter out rows where 'X13' is missing
        filter(!is.na(X13)) %>% 
        filter(!is.na(location_raw)) %>% 
        # Apply fuzzy matching functions to find the best match for 'location_tmp' in 'admin2_data$ADM2_PT'
        mutate(
          best_seq_match = map_chr(location_tmp, ~best_seq_match(.x, admin2_data$ADM2_PT)[1]),
          seq_ratio = as.double(map(location_tmp, ~best_seq_match(.x, admin2_data$ADM2_PT)[2])),
        ) %>% 
        # Assign the best match to 'location_match' if the match ratio is above 0.5, otherwise assign NA
        mutate(location_match = ifelse(seq_ratio > 0.47, best_seq_match, NA)) %>%
        # Assign 'location_match' to the 'district' column
        mutate(district = location_match) %>% 
        # Add an asterisk to the 'district_raw' column if 'location_raw' contains an asterisk
        mutate(district_raw = ifelse(grepl("\\*", location_raw), paste0(district, "*"), district)) %>%
        #mutate(active_flag = ifelse(grepl("\\*", location_raw), FALSE, TRUE)) %>% # add ACTIVE FLAG
        # Remove unnecessary columns
        select(-starts_with("location"), -contains("seq"), -c(X12)) %>% 
        # Reorder columns
        select(district, district_raw, everything()) %>% 
        # Join with 'admin2_data' to get the corresponding province information
        left_join(admin2_data %>% select(ADM1_PT, ADM2_PT), by = c("district" = "ADM2_PT")) %>% 
        # Rename 'ADM1_PT' to 'province'
        rename(province = ADM1_PT) %>% 
        # Convert columns 'X1' to 'X11' to integer type
        mutate(across(c(X1:X11), as.integer), across(c(X13), as.double), ) %>% 
        # Reorder columns with 'province' first
        select(province, everything())
      
      # Get date from pdf file name
      bulletin_data_path <- data.frame(summary_data_path ) %>%
        # Extract the date string from each PDF file name using a regular expression
        mutate(date_string = gsub(".*([0-9]{2}[\\.\\-][0-9]{2}[\\.\\-][0-9]{2,4}).*", "\\1", summary_data_path)) %>% 
        # Convert the extracted date strings to date objects
        mutate(date = dmy(date_string)) %>% 
        # Arrange the dates in descending order
        arrange(desc(date)) %>%
        # Select the first row, which corresponds to the latest bulletin date
        slice(1)
      
      # Calculate the difference in days between the latest bulletin date and the Excel base date ("1899-12-30")
      # and store the result as a character value in the 'bulletin_date' variable
      bulletin_date_excel <- convert_date_to_days(bulletin_data_path$date)
      bulletin_date <- bulletin_data_path$date
      
      # rename column with Excel date to align with expected input in Dataload script
      summary_data <- df_match %>% 
        select(province, district = district_raw, X3) %>% 
        rename(!!bulletin_date_excel := X3) %>% 
        bind_rows(summary_data) %>% 
        distinct()  # Remove duplicate rows
      
      # create bulletin_data from PDF
      bulletin_data <- df_match %>% 
        select(province, district = district_raw, cases_now = X3, deaths_now = X5)
      
      # Create temporary dataset to hold hospital data
      buletin_active_data_tmp <- bulletin_active_data %>% 
        mutate(district = case_when(
          grepl("Matola", district, ignore.case=T) ~ str_replace_all(district, "(Matola)", "Cidade Da \\1"),
          grepl("Chimoio|Pemba|Lichinga|Quelimane|Xai-Xai", district, ignore.case=T) ~ str_replace_all(district, "(Chimoio|Pemba|Lichinga|Quelimane|Xai-Xai)", "Cidade De \\1"),
          .default = district
        ))
      
      # create bulletin_activa_data from pdf
      bulletin_active_data <- df_match %>% 
        select(province, district = district_raw, cases_now = X3, hospitalized = X11, overall_incidence_rate = X13) %>% 
        stringdist_left_join(buletin_active_data_tmp %>% select(province, district, capacity, population),
                             by = c("province", "district"),
                             method="lv", # Levenhstein distance
                             max_dist = 1) %>% 
        select(-ends_with(".y")) %>% 
        rename(province = province.x, district = district.x) %>% 
        mutate(occupancy_rate = hospitalized / capacity) %>% 
        select(province, district, cases_now, hospitalized, capacity, occupancy_rate, population, overall_incidence_rate) %>% 
        distinct()  %>% # Remove duplicate rows
        filter(!grepl("surto|fim", district, ignore.case = TRUE))
      
    } else { # In case the files are excel by default
      
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
      
    }
    
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
