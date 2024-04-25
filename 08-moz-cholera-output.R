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
writeDataTable(wb, "cholera_data", district_daily_export_wide_tbl[, 1:5], startRow = 2, tableStyle = "TableStyleMedium13",  withFilter = TRUE)

centerStyle <- createStyle(halign = "center", valign = "center", border = "LeftRight", fgFill = "grey77", borderColour = "black")

# Find unique weeks and their corresponding dates
weeks <- unique(district_daily_export_tbl$week)
dates_per_week <- lapply(weeks, function(wk) unique(district_daily_export_tbl$date[district_daily_export_tbl$week == wk]))

# Write and merge headers for each week
start_col <- 6 # Adjust based on your data structure
for (i in seq_along(weeks)) {
  week_dates <- dates_per_week[[i]]
  end_col <- start_col + length(week_dates) - 1
  
  writeData(wb, 1, x = weeks[i], startCol = start_col, startRow = 1)
  mergeCells(wb, sheet = 1, rows = 1, cols = c(start_col, end_col))
  addStyle(wb, sheet = 1, style = centerStyle, rows = 1, cols = c(start_col, end_col), gridExpand = TRUE, stack = TRUE)
  start_col <- end_col + 1
}

# Write the data
writeDataTable(wb, "cholera_data", district_daily_export_wide_tbl[,6:ncol(district_daily_export_wide_tbl)], startRow = 2, startCol = 6, rowNames = FALSE, tableStyle = "TableStyleMedium9",  withFilter = FALSE)

freezePane(wb, "cholera_data", firstActiveRow = 3, firstActiveCol = 6)

# get table name
name_tbl <- substitute(district_daily_export_wide_tbl)

# excel_file <- paste0(name_tbl, "_", max_date_2, ".xlsx")
# saveWorkbook(wb, file = paste(output_dir, excel_file, sep = "/"), overwrite = TRUE)

excel_file <- paste0(name_tbl, ".xlsx")
saveWorkbook(wb, file = paste(output_dir, excel_file, sep = "/"), overwrite = TRUE)

# Export directory (only for local version)
export_dir <- "/Users/inigo/Library/CloudStorage/OneDrive-UNICEF/07 products/cholera/UNICEF cholera update/export"
saveWorkbook(wb, file = paste(export_dir, excel_file, sep = "/"), overwrite = TRUE)

# Update Azure Blobs -----
if (flag_azure == "TRUE") {
  
  local_dir <- paste(here("plots"), "*.png", sep = "/")
  storage_multiupload(container = cont, src = local_dir, dest = "plots")
  
  local_dir <- paste(here("rdas"), "*.RData", sep = "/")
  storage_multiupload(container = cont, src = local_dir, dest = "rdas")
  
}