# Export current cholera data -----

# # get table name
# name_tbl <- substitute(cholera_data_adm2)
# # get table content
# tmp <- eval(name_tbl)
# wb <- createWorkbook()
# addWorksheet(wb, "cholera map data", gridLines = FALSE)
# writeDataTable(wb, sheet = 1, tmp, tableStyle = "TableStyleMedium9",  withFilter = TRUE)
# 
# excel_file <- paste0("cholera_map_data_", max_date_2, ".xlsx")
# saveWorkbook(wb, file = paste(output_dir, excel_file, sep = "/"), overwrite = TRUE)
# 
# # get table name
# name_tbl <- substitute(bulletin_data)
# # get table content
# tmp <- eval(name_tbl)
# wb <- createWorkbook()
# addWorksheet(wb, "cholera cases", gridLines = FALSE)
# writeDataTable(wb, sheet = 1, tmp, tableStyle = "TableStyleMedium9",  withFilter = TRUE)
# 
# excel_file <- "bulletin_data.xlsx"
# saveWorkbook(wb, file = paste(output_dir, excel_file, sep = "/"), overwrite = TRUE)

# Export partnerships table -----

# get table name
name_tbl <- substitute(partners_district_wide_export_tbl)
# get table content
tmp <- eval(name_tbl)
wb <- createWorkbook()
addWorksheet(wb, "Partners", gridLines = FALSE)
writeDataTable(wb, sheet = 1, tmp, tableStyle = "TableStyleMedium9",  withFilter = TRUE)

excel_file <- "partners_district_data.xlsx"
saveWorkbook(wb, file = paste(output_dir, excel_file, sep = "/"), overwrite = TRUE)


# Export daily historical table -----

# Create a new workbook
wb <- createWorkbook()
addWorksheet(wb, "cholera_data")

# Write the district names first
writeDataTable(wb, "cholera_data", district_daily_export_wide_tbl[, 1:4], startRow = 2, tableStyle = "TableStyleMedium13",  withFilter = TRUE)

centerStyle <- createStyle(halign = "center", valign = "center", border = "LeftRight", fgFill = "grey77", borderColour = "black")

# Find unique weeks and their corresponding dates
weeks <- unique(district_daily_export_tbl$week)
dates_per_week <- lapply(weeks, function(wk) unique(district_daily_export_tbl$date[district_daily_export_tbl$week == wk]))

# Write and merge headers for each week
start_col <- 5 # Adjust based on your data structure
for (i in seq_along(weeks)) {
  week_dates <- dates_per_week[[i]]
  end_col <- start_col + length(week_dates) - 1
  
  writeData(wb, 1, x = weeks[i], startCol = start_col, startRow = 1)
  mergeCells(wb, sheet = 1, rows = 1, cols = c(start_col, end_col))
  addStyle(wb, sheet = 1, style = centerStyle, rows = 1, cols = c(start_col, end_col), gridExpand = TRUE, stack = TRUE)
  start_col <- end_col + 1
}

# Write the data
writeDataTable(wb, "cholera_data", district_daily_export_wide_tbl[,5:ncol(district_daily_export_wide_tbl)], startRow = 2, startCol = 5, rowNames = FALSE, tableStyle = "TableStyleMedium9",  withFilter = FALSE)

freezePane(wb, "cholera_data", firstActiveRow = 3, firstActiveCol = 5)

# get table name
name_tbl <- substitute(district_daily_export_wide_tbl)

excel_file <- paste0(name_tbl, "_", max_date_2, ".xlsx")
saveWorkbook(wb, file = paste(output_dir, excel_file, sep = "/"), overwrite = TRUE)

excel_file <- paste0(name_tbl, ".xlsx")
saveWorkbook(wb, file = paste(export_dir, excel_file, sep = "/"), overwrite = TRUE)