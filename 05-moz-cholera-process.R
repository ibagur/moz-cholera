## create empty object list containers
output <- list()

# Daily summary data processing -----

# District level
district_daily_cumul_tbl <- summary_data %>% 
  mutate(district_raw = district) %>% 
  mutate(district = case_when(
    grepl("Beira|Matola", district, ignore.case=T) ~ str_replace_all(district, ".*(Beira|Matola).*", "Cidade Da \\1"),
    grepl("Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai", district, ignore.case=T) ~ str_replace_all(district, ".*(Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai).*", "Cidade De \\1"),
    .default = district
  )) %>% 
  mutate(district = sub(pattern, "\\1", district)) %>% 
  stringdist_left_join(admin2_data %>% select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE),
                       by = c("district"="ADM2_PT"),
                       method="lv", # Jaro-Winkler distance
                       max_dist = 1) %>%
  filter(!(ADM2_PCODE %in% c("MZ0909") & ADM2_PT != district_raw)) %>% 
  select(-c(district, province, district_raw)) %>% 
  select(ADM1_PT, ADM2_PT, everything()) %>% 
  #mutate(across(everything(), ~ replace(., . == 0, NA))) %>% # Zero -> NA
  pivot_longer(
    cols = c((which(names(.) == "ADM2_PT") + 1):(which(names(.) == "ADM1_PCODE") - 1)), # select only date columns
    names_to = "day",
    values_to = "cases", 
    values_drop_na = T
  ) %>% 
  mutate(date = as.Date(as.numeric(day), origin = "1899-12-30")) %>% 
  mutate(week = format(date, "%Y-%W")) %>% 
  distinct(ADM2_PCODE, cases, date, .keep_all = TRUE) %>% 
  group_by(date) %>% 
  filter(!all(cases == 0)) %>%
  ungroup() %>% 
  select(week, ADM1_PT, ADM2_PT, ADM1_PCODE, ADM2_PCODE, date, cases) %>% 
  group_by(week, ADM1_PT, ADM2_PT) %>% 
  mutate(cumul_district_cases = cumsum(cases)) %>% # get cumulative district cases
  arrange(date, ADM1_PT, ADM2_PT) %>% 
  group_by(week) %>%
  mutate(n_days = n_distinct(date))

# Province level
province_daily_cumul_tbl <- district_daily_cumul_tbl %>% 
  group_by(week, ADM1_PT, date) %>% 
  summarize(ADM1_PCODE = first(ADM1_PCODE), cases = sum(cases, na.rm = T), n_days = first(n_days)) %>% 
  group_by(week, ADM1_PT) %>% 
  mutate(cumul_province_cases = cumsum(cases)) %>% # get cumulative district cases
  arrange(week, ADM1_PT) %>% 
  mutate(daily_change = c(NA, diff(cases))) %>% 
  mutate(cumul_daily_change = c(NA, diff(cumul_province_cases))) %>% 
  select(week, ADM1_PT, ADM1_PCODE, date, everything())

# National level
national_daily_cumul_tbl <- province_daily_cumul_tbl %>%
  group_by(date) %>%
  summarize(week = first(week), total_cases = sum(cases, na.rm = TRUE), n_days = first(n_days)) %>% 
  select(week, date, total_cases) %>% 
  group_by(week) %>% 
  mutate(total_week = sum(total_cases)) %>%  
  arrange(date) %>% 
  ungroup()
 

# Weekly summary data processing -----

# District level
district_weekly_cumul_tbl <- district_daily_cumul_tbl %>% 
  group_by(week, ADM1_PT, ADM2_PT) %>% 
  summarize(ADM1_PCODE = first(ADM1_PCODE), ADM2_PCODE = first(ADM2_PCODE), total_cases = max(cumul_district_cases), n_days = first(n_days)) %>% 
  arrange(ADM1_PT, ADM2_PT, week) %>%
  group_by(ADM1_PT, ADM2_PT) %>%
  mutate(
    relative_variation = (total_cases - lag(total_cases)) / lag(total_cases)
  ) %>% 
  mutate(
    relative_variation = replace(relative_variation, is.na(relative_variation) | is.nan(relative_variation), 0),
    relative_variation = replace(relative_variation, is.infinite(relative_variation), 1)
  ) %>% 
  group_by(week, ADM1_PT) %>%
  mutate(total_week_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(relative_variation_weighted = relative_variation * total_cases/total_week_cases)
  
# Province level
province_weekly_cumul_tbl <- province_daily_cumul_tbl %>% 
  group_by(week, ADM1_PT) %>% 
  summarize(ADM1_PCODE = first(ADM1_PCODE), total_cases = max(cumul_province_cases), n_days = first(n_days)) %>% 
  arrange(ADM1_PT, week) %>%
  group_by(ADM1_PT) %>%
  mutate(
    relative_variation = (total_cases - lag(total_cases)) / lag(total_cases)
  ) %>% 
  group_by(week) %>%
  mutate(total_week_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(relative_variation_weighted = relative_variation * total_cases/total_week_cases)


# MINISTRY data processing (patch) -----

district_weekly_ministry_tbl <- ministry_data %>% 
  mutate(district_raw = district) %>% 
  mutate(district = case_when(
    grepl("Beira|Matola", district, ignore.case=T) ~ str_replace_all(district, ".*(Beira|Matola).*", "Cidade Da \\1"),
    grepl("Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai", district, ignore.case=T) ~ str_replace_all(district, ".*(Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai).*", "Cidade De \\1"),
    .default = district
  )) %>% 
  mutate(district = sub(pattern, "\\1", district)) %>% 
  stringdist_left_join(admin2_data %>% select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE),
                       by = c("district"="ADM2_PT"),
                       method="lv", # Jaro-Winkler distance
                       max_dist = 1) %>%
  filter(!(ADM2_PCODE %in% c("MZ0909") & ADM2_PT != district_raw)) %>% 
  select(-c(district, province, district_raw)) %>% 
  select(ADM1_PT, ADM2_PT, everything()) %>% 
  pivot_longer(
    cols = c((which(names(.) == "ADM2_PT") + 1):(which(names(.) == "ADM1_PCODE") - 1)), # select only date columns
    names_to = "time_data",
    values_to = "total_cases", 
    values_drop_na = T
  ) %>% 
  mutate(
    week_number = as.integer(str_remove(time_data, "S")), # Remove 'S' and convert to integer
    year = ifelse(week_number >= 40, 2023, 2024), # Determine the year
    week = paste(year, sprintf("%02d", week_number), sep="-") # Combine into year-week format
  ) %>%
  select(-c(week_number, year, time_data)) %>% # Optional: Remove the intermediate columns
  arrange(ADM1_PT, ADM2_PT, week) %>% 
  mutate(
    relative_variation = (total_cases - lag(total_cases)) / lag(total_cases)
  ) %>% 
  mutate(
    relative_variation = replace(relative_variation, is.na(relative_variation) | is.nan(relative_variation), 0),
    relative_variation = replace(relative_variation, is.infinite(relative_variation), 1)
  ) %>% 
  group_by(week, ADM1_PT) %>%
  mutate(total_week_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(relative_variation_weighted = relative_variation * total_cases/total_week_cases) %>% 
  select(week, everything()) %>% 
  mutate(n_days = NA)

province_weekly_ministry_tbl <- district_weekly_ministry_tbl %>% 
  group_by(week, ADM1_PT, ADM1_PCODE) %>% 
  summarise(total_cases = sum(total_cases, na.rm = TRUE)) %>% 
  arrange(ADM1_PT, week) %>%
  group_by(ADM1_PT) %>%
  mutate(
    relative_variation = (total_cases - lag(total_cases)) / lag(total_cases)
  ) %>% 
  group_by(week) %>%
  mutate(total_week_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(relative_variation_weighted = relative_variation * total_cases/total_week_cases)  

# Patch data -----

district_weekly_tbl <- bind_rows(district_weekly_cumul_tbl %>% filter(week != "2024-03"), district_weekly_ministry_tbl %>% filter(week == "2024-03")) %>%  # to include missing 3rd week
  mutate(n_days = case_when(
    week == "2024-03" ~ 7,
    .default = n_days  
  )) %>% 
  arrange(ADM1_PT, ADM2_PT, week) %>% 
  mutate(
    relative_variation = (total_cases - lag(total_cases)) / lag(total_cases)
  ) %>% 
  mutate(
    relative_variation = replace(relative_variation, is.na(relative_variation) | is.nan(relative_variation), 0),
    relative_variation = replace(relative_variation, is.infinite(relative_variation), 1)
  ) %>% 
  group_by(week, ADM1_PT) %>%
  mutate(total_week_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(relative_variation_weighted = relative_variation * total_cases/total_week_cases) %>% 
  select(week, everything()) %>% 
  filter(!is.na(ADM1_PT)) %>% 
  group_by(ADM1_PT) %>% 
  mutate(n_weeks = n_distinct(week)) %>% # to properly count the total number of weeks!
  ungroup() %>% 
  mutate(condition_flag = if_else(n_weeks ==1, 1, 0)) %>% # if Province appears only 1 week
  filter(condition_flag == 0) %>% 
  select(-condition_flag)
  # below how to spot a new province getting cases that was not before
  #mutate(duplication_count = if_else((condition_flag == 1) & (week == max(week)), 2, 1)) %>%
  # uncount(duplication_count, .id = "new") %>% 
  # mutate(week = if_else(new == 2, sort(unique(district_weekly_tbl$week))[length(sort(unique(district_weekly_tbl$week)))-1], week)) %>% 
  # mutate(across(c(total_cases, total_week_cases, relative_variation, relative_variation_weighted), ~ if_else(new == 2, 0, .))) %>% 
  # select(-c(condition_flag, new))
  

province_weekly_tbl <- district_weekly_tbl %>% 
  group_by(week, ADM1_PT, ADM1_PCODE) %>% 
  summarise(total_cases = sum(total_cases, na.rm = TRUE), n_days = first(n_days)) %>% 
  arrange(ADM1_PT, week) %>%
  group_by(ADM1_PT) %>%
  mutate(
    relative_variation = (total_cases - lag(total_cases)) / lag(total_cases)
  ) %>% 
  group_by(week) %>%
  mutate(total_week_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(relative_variation_weighted = relative_variation * total_cases/total_week_cases)  


# WASH water data processing -----

# District level
water_district_tbl <- water_data %>% 
  rename_with(~c("province", "district", "pop_2022", "pop_surf_2022"), .cols=1:4) %>% 
  mutate(pop_surf_ratio = pop_surf_2022/pop_2022) %>% 
  select(-X.) %>% 
  mutate(province = if_else(grepl("Cidade", province, ignore.case=T), "Maputo City", province)) %>% 
  mutate(district = str_to_title(str_trim(district))) %>% 
  mutate(district_raw = district) %>% 
  mutate(district = stri_trans_general(str = district, id = "Latin-ASCII")) %>% 
  mutate(district = case_when(
    province == "Maputo City" ~ "Cidade De Maputo",
    grepl("Beira|Matola", district, ignore.case=T) ~ str_replace_all(district, ".*(Beira|Matola).*", "Cidade Da \\1"),
    grepl("Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai", district, ignore.case=T) ~ str_replace_all(district, ".*(Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai).*", "Cidade De \\1"),
    grepl("Maxixe", district, ignore.case=T) ~ "Maxixe",
    grepl("Porto", district, ignore.case=T) ~ "Nacala", 
    .default = district
  )) %>% 
  mutate(district = sub(pattern, "\\1", district)) 

# consolidate only City Maputo rows
summed_row <- water_district_tbl %>%
  filter(district == "Cidade De Maputo") %>%
  summarise(across(c(province:district), first), across(where(is.numeric), ~ sum(.x, na.rm = T))) %>%
  mutate(pop_surf_ratio = pop_surf_2022/pop_2022)
  
# Bind with main dataset after filtering out the City Maputo original rows
water_district_tbl <- bind_rows(water_district_tbl %>% filter(province != "Maputo City"), summed_row) %>% 
  stringdist_left_join(admin2_data %>% select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE),
                       by = c("district"="ADM2_PT"),
                       method="lv", # Jaro-Winkler distance
                       max_dist = 1) %>%
  select(-c(province, district)) %>% 
  select(ADM1_PT:ADM2_PCODE, everything()) %>% 
  arrange(ADM1_PT, ADM2_PT) %>% 
  filter(!(ADM2_PCODE %in% c("MZ0909", "MZ1008") & ADM2_PT != district_raw))

# WASH sanitation data processing -----

# District level
sanitation_district_tbl <- sanitation_data %>% 
  select(-c(DISTRICT_EXCEL, POP_2017:X.POP_OD_2022, LEVEL)) %>% 
  rename_with(~c("pop_2022", "pop_od_2022"), .cols=3:4) %>% 
  mutate(across(.cols=c(3:4), round)) %>% 
  mutate(pop_od_ratio = pop_od_2022/pop_2022) %>% 
  mutate(district_raw = ADM2_PT) %>% 
  stringdist_left_join(admin2_data %>% select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE),
                       by = c("ADM2_PT"),
                       method="lv", # Jaro-Winkler distance
                       max_dist = 1) %>% 
  filter(!(ADM2_PCODE %in% c("MZ0909", "MZ1008") & ADM2_PT.x != ADM2_PT.y)) %>% 
  select(-c(PROVINCE, ADM2_PT.y)) %>% 
  select(ADM1_PT, ADM1_PCODE, ADM2_PT = ADM2_PT.x, ADM2_PCODE, everything())


# Process last bulletin data -----

# District level
bulletin_data_adm2 <- bulletin_data %>% 
  mutate(district_raw = district) %>% 
  mutate(district = case_when(
    grepl("Beira|Matola", district, ignore.case=T) ~ str_replace_all(district, ".*(Beira|Matola).*", "Cidade Da \\1"),
    grepl("Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai", district, ignore.case=T) ~ str_replace_all(district, ".*(Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai).*", "Cidade De \\1"),
    grepl("Zumb", district, ignore.case=T) ~ "Zumbu", 
    .default = district
  )) %>% 
  mutate(district = str_to_title(str_trim(district))) %>% 
  mutate(district = sub(pattern, "\\1", district)) %>% 
  mutate(province = if_else(grepl("Maputo Prov", province, ignore.case = T), "Maputo", province)) %>% # detect Maputo Provincia
  stringdist_left_join(admin1_data %>% select(ADM1_PT, ADM1_PCODE),
                       by = c("province"="ADM1_PT"),
                       method="lv", # Levenhstein distance
                       max_dist = 2) %>%
  select(ADM1_PT, district, everything()) %>%
  stringdist_left_join(admin2_data %>% select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE, AREA_SQKM),
                       by = c("province"="ADM1_PT", "district"="ADM2_PT"),
                       method="lv", # Jaro-Winkler distance
                       max_dist = 1) %>% # select also the ADM1_PT to fix bad province names
  mutate(ADM1_PCODE = ADM1_PCODE.y, ADM1_PT = ADM1_PT.y) %>% # to fix bad province names
  select(-c(ADM1_PT.x, ADM1_PCODE.x)) %>% 
  filter(str_detect(ADM2_PCODE, paste0("^", ADM1_PCODE))) %>%  # to filter out Machanga 
  group_by(ADM2_PT) %>%
  select(-district, -province) %>% 
  select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE, everything()) %>% 
  ungroup() %>% 
  mutate(date = bulletin_date) %>% 
  mutate (week = format(date, "%Y-%W")) %>% 
  select(week, date, everything())


# Combined bulletin - wash - sanitation ----

# District level
cholera_wash_sanitation_district_tbl <- bulletin_data_adm2 %>% 
  left_join(water_district_tbl %>% select(ADM2_PCODE, pop_2022a = pop_2022, pop_surf_2022a = pop_surf_2022, pop_surf_ratio),
            by = c("ADM2_PCODE")) %>% 
  left_join(sanitation_district_tbl %>% select(ADM2_PCODE, pop_2022b = pop_2022, pop_od_2022b = pop_od_2022, pop_od_ratio),
            by = c("ADM2_PCODE")) %>% 
  mutate(incidence = cases_now * 1e5 / pop_2022a) %>% 
  relocate(AREA_SQKM, .after = last_col()) %>% 
  arrange(ADM1_PT, ADM2_PT)

# Province level
cholera_wash_sanitation_province_tbl <- cholera_wash_sanitation_district_tbl %>% 
  group_by(ADM1_PT) %>% 
  summarize(across(c(cases_now, deaths_now, pop_2022a, pop_surf_2022a, pop_2022b, pop_od_2022b), ~ sum(.x, na.rm = T)), ADM1_PCODE = first(ADM1_PCODE)) %>% 
  mutate(pop_surf_ratio = pop_surf_2022a/pop_2022a, pop_od_ratio = pop_od_2022b/pop_2022b) %>% 
  select(ADM1_PT, ADM1_PCODE, everything()) %>% 
  arrange(ADM1_PT, ADM1_PCODE) 

# Process last bulletin active cases data -----

# District level
bulletin_active_data_adm2_tbl <- bulletin_active_data %>% 
  mutate(district_raw = district) %>% 
  mutate(district = case_when(
    grepl("Zumb", district, ignore.case=T) ~ "Zumbu", 
    grepl("Beira|Matola", district, ignore.case=T) ~ str_replace_all(district, ".*(Beira|Matola).*", "Cidade Da \\1"),
    grepl("Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai", district, ignore.case=T) ~ str_replace_all(district, ".*(Chimoio|Inhambane|Lichinga|Maputo|Nampula|Pemba|Quelimane|Tete|Xai-Xai).*", "Cidade De \\1"),
    .default = district
  )) %>% 
  mutate(district = str_to_title(str_trim(district))) %>% 
  mutate(district = sub(pattern, "\\1", district)) %>% 
  stringdist_left_join(admin1_data %>% select(ADM1_PT, ADM1_PCODE),
                       by = c("province"="ADM1_PT"),
                       method="lv", # Levenhstein distance
                       max_dist = 2) %>%
  select(ADM1_PT, district, everything()) %>%
  stringdist_left_join(admin2_data %>% select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE, AREA_SQKM),
                       by = c("province"="ADM1_PT", "district"="ADM2_PT"),
                       method="lv", # Levenhstein distance
                       max_dist = 1) %>%
  mutate(ADM1_PCODE = ADM1_PCODE.y, ADM1_PT = ADM1_PT.y) %>% # to fix bad province names
  select(-c(ADM1_PT.x, ADM1_PCODE.x)) %>%
  filter(str_detect(ADM2_PCODE, paste0("^", ADM1_PCODE))) %>%  # to filter out Machanga 
  group_by(ADM2_PT) %>%
  select(-district, -province) %>% 
  select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE, everything()) %>% 
  ungroup() %>% 
  mutate(date = bulletin_date) %>% 
  mutate (week = format(date, "%Y-%W")) %>% 
  select(week, date, everything()) %>% 
  left_join(district_pop_capacity_tbl %>% select(ADM2_PCODE, capacity, population), by = c("ADM2_PCODE")) %>% 
  rename(capacity = capacity.y, population = population.y) %>% 
  select(-c(capacity.x, population.x, ADM1_PT.y, ADM1_PCODE.y)) %>% 
  mutate(occupancy_rate = hospitalized / capacity) %>% 
  relocate(capacity, .after = hospitalized) %>% 
  relocate(population, .after = occupancy_rate) %>% 
  filter(occupancy_rate > 0)
  
# Partnership data -----

## District wide table (exclude Radio mozambique)
partners_district_wide_tbl <- partners_data %>% 
  select(-starts_with("Radio")) %>% # Exclude Radio Mozambique
  mutate(district = case_when(
    province == "Maputo City" ~ "Cidade De Maputo",
    .default = district
  )) %>% 
  mutate(across(6:(ncol(.)), ~ ifelse(!is.na(.), ., 0))) %>% 
  select(-cholera_status) %>% 
  select(ADM1_PT=province, ADM1_PCODE, ADM2_PT=district, ADM2_PCODE, everything())
  

## District wide table 
# partners_district_wide_tbl <- partners_data %>% 
#   mutate(district = str_to_title(str_trim(district))) %>% 
#   mutate(district_raw = district) %>% 
#   mutate(district = stri_trans_general(str = district, id = "Latin-ASCII")) %>% 
#   mutate(province = case_when(
#     grepl("Cidade", province) ~ "Maputo City",
#     .default = province
#   )) %>% 
#   mutate(district = case_when(
#     province == "Maputo City" ~ "Cidade De Maputo",
#     grepl("Lichinga|Chimoio|Nampula|Quelimane|Xai-Xai", district, ignore.case=T) ~ paste0("Cidade De ", district),
#     grepl("Maxixe", district, ignore.case=T) ~ "Maxixe",
#     grepl("Porto", district, ignore.case=T) ~ "Nacala", 
#     .default = district
#   )) %>% 
#   mutate(district = sub(pattern, "\\1", district)) %>%
#   mutate(across(3:(ncol(.)-1), ~ ifelse(!is.na(.), 1, 0))) %>% #TODO change to TRUE/FALSE for grid plot?
#   stringdist_left_join(admin2_data %>% select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE),
#                        by = c("district"="ADM2_PT"),
#                        method="lv", # Jaro-Winkler distance
#                        max_dist = 1) %>%
#   filter(!(ADM2_PCODE %in% c("MZ0909", "MZ1008", "MZ0408") & ADM2_PT != district_raw)) %>% 
#   select(-c(province, district)) %>% 
#   select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE, everything())


## Partner presence district long table (only presence=1)
partners_district_long_tbl <- partners_district_wide_tbl %>% 
  pivot_longer(
    #cols = c((which(names(.) == "ADM2_PCODE") + 1):(which(names(.) == "district_raw") - 1)), # select only date columns
    cols = c((which(names(.) == "ADM2_PCODE") + 1):last_col()),
    names_to = "partner",
    values_to = "presence"
  ) %>% 
  #filter(presence >= 1) %>% # all partners
  filter(presence == 1) %>% # only if UNICEF partner
  #mutate(presence=TRUE) %>% #TODO remove if changed before
  mutate(partner = remove_dots(partner)) %>% 
  mutate(partner = case_when(
    grepl("Radio", partner) ~ "Radio Moçambique",
    grepl("ICS", partner) ~ "ICS", # combine ICS activities for the total count
    .default = partner
  )) %>% 
  ungroup()

## Partner presence district long table (presence 1 and non-presence = 0)
partners_district_long_full_tbl <- partners_district_wide_tbl %>% 
  pivot_longer(
    #cols = c((which(names(.) == "ADM2_PCODE") + 1):(which(names(.) == "district_raw") - 1)), # select only date columns
    cols = c((which(names(.) == "ADM2_PCODE") + 1):last_col()),
    names_to = "partner",
    values_to = "presence"
  ) %>% 
  #filter(presence != 2) %>% # remove non UNICEF partners
  mutate(partner = remove_dots(partner)) %>% 
  mutate(partner = case_when(
    grepl("Radio", partner) ~ "Radio Moçambique",
    .default = partner
  )) %>% 
  ungroup()

# Partners count per district and province (exc. Radio Moz)

partners_count_district_tbl <- partners_district_long_tbl %>%
  group_by(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE) %>%
  summarise(
    total_partners = n(),
    total_gov = sum(grepl("ICS|DROP|SPS", partner), na.rm = TRUE),
    total_ngo = total_partners - total_gov,
    #total_unicef = sum(presence == 1, na.rm = TRUE),
    #total_non_unicef = sum(presence == 2, na.rm = TRUE)
  ) %>% 
  mutate(
    total_partners = na_if(total_partners, 0),
    total_gov = na_if(total_gov, 0),
    total_ngo = na_if(total_ngo, 0)    
    # total_unicef = na_if(total_unicef, 0),
    # total_non_unicef = na_if(total_non_unicef, 0)
  )
  
partners_count_province_tbl <- partners_district_long_tbl %>% 
  group_by(ADM1_PT, ADM1_PCODE) %>% 
  summarise(
    total_partners = n_distinct(partner)
    )
  
# Map data processing -----

cholera_data_district <- cholera_data %>%
  mutate(district = stri_trans_general(str = district, id = "Latin-ASCII")) %>% 
  mutate(province = case_when(
    status == "City Dist." ~ "Maputo City",
    .default = province
  )) %>% 
  mutate(district = case_when(
    status == "City Dist." ~ "Cidade De Maputo",
    grepl("Porto", district, ignore.case=T) ~ "Nacala", 
    grepl("Chimbonila", district, ignore.case=T) ~ "Chimbonila", 
    status == "City" & !(district %in% c("Ilha de Mocambique", "Nacala", "Maxixe")) ~ paste0("Cidade Da ", district), 
    .default = district
  )) %>% 
  mutate(district = sub(pattern, "\\1", district))

# consolidate only City Maputo rows
summed_row <- cholera_data_district %>%
  filter(district == "Cidade De Maputo") %>%
  summarise(across(c(province:status), first), across(where(is.numeric),  ~ sum(.x, na.rm = T))) %>%
  mutate(district = "Cidade De Maputo") %>% 
  mutate(across(ends_with("flag"), ~ ifelse(. == 0, NA, 1)))

# Bind with main dataset after filtering out the City Maputo original rows
cholera_data_district <- bind_rows(cholera_data_district %>% filter(status != "City Dist."), summed_row)

# Now, all numeric columns in 'df' will have values set to NA if they were 0, and 1 otherwise.

cholera_data_adm2_prev <- cholera_data_district %>% 
  stringdist_left_join(admin1_data %>% select(ADM1_PT, ADM1_PCODE), 
                       by = c("province"="ADM1_PT"), 
                       method="lv", # Levenhstein distance
                       max_dist = 2) %>% 
  select(ADM1_PT, district, everything()) %>% 
  stringdist_left_join(admin2_data %>% select(ADM2_PT, ADM2_PCODE, AREA_SQKM),
                       by = c("district"="ADM2_PT"),
                       method="jw", # Jaro-Winkler distance
                       max_dist = 99,
                       distance_col='dist',
                       p=0.1) %>%
  group_by(ADM2_PT) %>%
  slice_min(order_by=dist, n=1) %>%
  select(ADM1_PT, district, ADM2_PT, dist, everything()) %>% 
  filter(dist < 0.15) %>% 
  filter(!(ADM2_PT %in% c("Maquival", "Lago Niassa", "Cidade De Maputo"))) %>% 
  select(-district, -province, -dist)

# Join last bulletin data with prev cholera dataset
cholera_data_adm2 <- cholera_data_adm2_prev %>% 
  left_join(bulletin_data_adm2 %>% select(ADM1_PCODE, ADM2_PCODE, cases_now, deaths_now, district_raw), by = c("ADM1_PCODE", "ADM2_PCODE")) %>% 
  relocate(ends_with("now"), .after = "deaths_sep_2023") %>% 
  mutate(cases_now = cases_now) %>% 
  mutate(deaths_now = deaths_now) %>% 
  mutate(cholera_declared_flag = case_when (
    !is.na(cases_now) ~ 1,
    #.default = cholera_declared_flag
    .default = NA
  )
  ) %>% # check if cases declared now, preserve also original clasification
  mutate(cholera_ended_flag = if_else(grepl("\\*", district_raw), 1, NA)) %>% # check end cholera 
  mutate(cholera_declared_flag = case_when(
    cholera_ended_flag == 1 ~ NA,
    .default = cholera_declared_flag
  )) %>% # update cholera declared
  #select(-district_raw) %>% 
  ungroup() %>% 
  mutate(fill_color = "white") %>% 
  mutate(fill_color = if_else(!is.na(cases_sep_2023), UNICEF_PALETTE_YELLOWS[3], fill_color)) %>% #yellow
  mutate(fill_color = if_else(!is.na(cholera_ended_flag), UNICEF_PALETTE_YELLOWS[3], fill_color)) %>% #yellow
  #mutate(fill_color = if_else(!is.na(awd_flag), UNICEF_PALETTE[5], fill_color)) %>% #orange !WARNING removed AWD
  mutate(fill_color = ifelse(!is.na(cholera_declared_flag), UNICEF_PALETTE[6], fill_color)) %>% #red
  # mutate(fill_color = case_when(
  #   cholera_declared_flag == 1 ~ UNICEF_PALETTE[6], #red
  #   !is.na(cases_sep_2023) ~ UNICEF_PALETTE_YELLOWS[3], #yellow
  #   awd_flag == 1 ~ UNICEF_PALETTE[5], #orange
  #   .default = "white" 
  #   #.default = NA
  # )) %>% 
  mutate(adm2_pt_filtered = case_when(
    fill_color != "white" ~ ADM2_PT,
    #!is.na(fill_color) ~ adm2_pt,
    #!is.na(zq) | !is.na(zar) ~ ADM2_PT,
    .default = NA
  )) %>% 
  left_join(partners_count_district_tbl,  by=c("ADM2_PCODE")) %>% 
  rename(ADM1_PCODE = ADM1_PCODE.x, ADM1_PT = ADM1_PT.x, ADM2_PT = ADM2_PT.x) %>% 
  select(-c(ends_with(c(".x",".y")))) %>% 
  # mutate(total_partners_lbl = case_when(
  #   fill_color == "white" & !is.na(total_partners) ~ NA,
  #   .default = total_partners
  # ))
  mutate(total_partners_lbl = total_partners) %>% # add all partners in all districts
  mutate(total_gov_lbl = total_gov) %>% # add unicef partners in all districts
  mutate(total_ngo_lbl = total_ngo) %>% # add unicef partners in all districts
  # mutate(total_unicef_lbl = total_unicef) %>% # add unicef partners in all districts
  # mutate(total_non_unicef_lbl = total_non_unicef) %>% # add unicef partners in all districts
  mutate(mod_factor = if_else(is.na(total_ngo), 0, 1)) %>% 
  mutate(mod_factor = if_else(is.na(total_gov), 0, mod_factor)) %>% 
  # mutate(mod_factor = if_else(is.na(total_non_unicef_lbl), 0, 1)) %>% 
  # mutate(mod_factor = if_else(is.na(total_unicef_lbl), 0, mod_factor)) %>% 
  #mutate(mod_factor = if_else(is.na(total_non_unicef_lbl), 0, 1)) %>% 
  # mutate(total_partners_lbl = case_when(
  #   !is.na(zq) | !is.na(zar) ~ total_partners,
  #   .default = total_partners
  # ))
  mutate(hotspot = case_when(
    (!is.na(zq) | !is.na(zar)) ~ "yes",
    .default = NA
  )) %>% 
  select(ADM1_PT, ADM1_PCODE, ADM2_PT, ADM2_PCODE, everything()) %>% 
  arrange(ADM1_PCODE, ADM2_PCODE)

if (activos_pdf_flag) {
  
  # Load saved cholera_data_adm2_bak
  load(file = paste(rda_dir, "cholera_data_adm2_bak.RData", sep = "/"), verbose = F)
  
  # Join by adm2_pcode
  # Update cholera_ended_flag: cholera_declared_flag_bak == 1 & is.na(cholera_declared_flag) -> cholera_ended_flag = 1
  cholera_data_adm2 <- cholera_data_adm2 %>% 
    left_join(cholera_data_adm2_bak, by = "ADM2_PCODE") %>% 
    mutate(cholera_ended_flag.x = if_else((cholera_declared_flag.y == 1 & is.na(cases_now)), 1, cholera_ended_flag.x)) %>% 
    rename_with(~ gsub("\\.x$", "", .x), ends_with(".x")) %>% 
    mutate(fill_color = "white") %>% 
    mutate(fill_color = if_else(!is.na(cases_sep_2023), UNICEF_PALETTE_YELLOWS[3], fill_color)) %>% #yellow
    mutate(fill_color = if_else(!is.na(cholera_ended_flag), UNICEF_PALETTE_YELLOWS[3], fill_color)) %>% #yellow
    mutate(fill_color = ifelse(!is.na(cholera_declared_flag), UNICEF_PALETTE[6], fill_color)) %>% #red
    select(-ends_with(".y"))
  
} else {
  
  cholera_data_adm2_bak <- cholera_data_adm2 %>% select(ADM2_PCODE, cholera_declared_flag, cholera_ended_flag, fill_color)
  # save to RData
  save(
    cholera_data_adm2_bak,
    file = paste(rda_dir, "cholera_data_adm2_bak.RData", sep = "/"))
}

# EXPORT post-processing -----

## Partnership data for Excel tool -----

## District wide table for export 
partners_district_wide_export_tbl <- partners_district_wide_tbl %>% 
  rename_with(.,~ remove_dots(.)) %>% 
  mutate(across(everything(), ~ replace(., . == 0, NA))) %>%
  left_join(cholera_data_adm2 %>% select(ADM2_PCODE, fill_color), by = c("ADM2_PCODE")) %>% 
  mutate(cholera_status = case_when(
    fill_color == UNICEF_PALETTE_YELLOWS[3] ~ "recent",
    fill_color == UNICEF_PALETTE[6] ~ "current",
    .default = ""
  )) %>% 
  select(-fill_color) %>% 
  select(Province=ADM1_PT, ADM1_PCODE, District=ADM2_PT,ADM2_PCODE, cholera_status, everything())

## Daily historical data -----

# Long table
district_daily_export_tbl <- district_daily_cumul_tbl %>% 
  select(date, week, province = ADM1_PT, ADM1_PCODE, district = ADM2_PT, ADM2_PCODE, cases) %>% 
  arrange(date, ADM1_PCODE, ADM2_PCODE) %>% 
  filter(cases != 0) %>% 
  filter(!is.na(province)) %>% 
  ungroup()

# Wide table
district_daily_export_wide_tbl <- district_daily_export_tbl %>% 
  select(date, province, ADM1_PCODE, district, ADM2_PCODE, cases) %>% 
  pivot_wider(names_from = date, values_from = cases, values_fill = 0, names_sort = TRUE) %>%
  left_join(cholera_data_adm2 %>% select(ADM2_PCODE, cholera_declared_flag), by = "ADM2_PCODE") %>% 
  mutate(active_cholera = if_else(!is.na(cholera_declared_flag), "Yes", "")) %>% 
  select(-cholera_declared_flag) %>% 
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x")) %>% 
  select(-ends_with(".y")) %>% 
  select(province, ADM1_PCODE, district, ADM2_PCODE, active_cholera , everything()) %>% 
  arrange(ADM1_PCODE, ADM2_PCODE)


# For all plots -----

# get latest date
max_date_1 <- format(max(province_daily_cumul_tbl$date), "%d-%m-%Y")
max_date_2 <- format(max(province_daily_cumul_tbl$date), "%Y%m%d")

# get latest week
data <- province_weekly_tbl %>% 
  filter(n_days == 7) %>%
  filter(!is.na(ADM1_PCODE)) %>% 
  filter(total_cases !=0)

max_week <- max(data$week)