
# Specify the path to your PDF file
pdf_path <- "/Users/inigo/Documents/curro/UNHCR Mozambique/Onedrive - UNHCR/08_tools/01_R/UNHCR_IM_Tools/R/moz-cholera/images/colera_table.pdf"

# Use tabulizer to extract tables from the PDF. Adjust area and columns parameters as needed.
tables <- extract_tables(pdf_path, pages = 1) # You might need to adjust the pages parameter

# Assuming the table of interest is the first one extracted and needs little to no preprocessing
df <- as.data.frame(tables[[1]])

# Check the first few rows to ensure it looks correct
head(df)

# Convert the data frame to an Excel file
excel_path <- gsub(".pdf", ".xlsx", pdf_path)
write.xlsx(df, excel_path)

# Print the path where the Excel file is saved
cat("Excel file saved at:", excel_path, "\n")