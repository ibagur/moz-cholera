# Mozambique Cholera Monitoring Dashboard

## Overview

This R-based system provides a comprehensive ETL (Extract, Transform, Load) pipeline and interactive dashboard for monitoring cholera outbreaks in Mozambique. The system processes epidemiological data, generates visualizations, and creates an interactive Shiny web application for real-time cholera surveillance.

## System Architecture

The system follows a modular approach with numbered scripts that execute in sequence:

### Core Workflow Scripts

| Script | Purpose | Description |
|--------|---------|-------------|
| `00-init.R` | Initialization | Sets up the tool name and handles file renaming from templates |
| `01-moz-cholera-required.R` | Dependencies | Loads all required R packages for mapping, visualization, and data processing |
| `02-moz-cholera-functions.R` | Custom Functions | Contains project-specific utility functions |
| `03-moz-cholera-config.R` | Configuration | Defines paths, Azure settings, fonts, and system parameters |
| `04-moz-cholera-dataload.R` | Data Loading | ETL process for importing and cleaning multiple data sources |
| `05-moz-cholera-process.R` | Data Processing | Transforms raw data into analysis-ready formats |
| `06-moz-cholera-analysis.R` | Data Analysis | Statistical analysis and calculations |
| `07-moz-cholera-plot.R` | Visualization | Creates maps, charts, and interactive plots |
| `08-moz-cholera-output.R` | Data Export | Exports processed data to Excel and updates Azure storage |
| `09-moz-cholera-render.R` | Report Generation | Renders reports and documentation |

### Main Entry Points

- **`moz-cholera.R`** - Main script that orchestrates the entire workflow
- **`app.R`** - Shiny web application for the interactive dashboard

## Data Sources

The system processes multiple data sources:

### Input Data Types
- **Bulletin Files** (PDF/Excel): Daily cholera surveillance reports from DVS
- **Partnership Files** (Excel): Information about partner organizations and coverage
- **Administrative Data**: District and province boundary information
- **WASH Data**: Water and sanitation infrastructure data
- **Ministry Data**: Official government cholera statistics

### Key Data Processing Features
- **Fuzzy matching** for district name standardization
- **Multi-format support** (PDF text extraction, Excel sheets)
- **Temporal data aggregation** (daily, weekly, bi-weekly summaries)
- **Geographic data integration** with administrative boundaries

## Dashboard Features

### Interactive Web Application (`app.R`)

The Shiny dashboard provides:

#### File Upload Interface
- Upload latest bulletin files (PDF/Excel)
- Upload partnership coverage files
- Real-time data processing with progress indicators

#### Visualization Tabs
1. **Preview 1**: Cholera outbreak maps and daily trends
2. **Preview 2**: District-level bi-weekly analysis
3. **Preview 3**: Health system capacity and occupancy rates

#### Download Capabilities
- Export processed data as Excel files
- Download high-resolution plots (PNG/PDF)
- Generate partnership coverage reports

### Azure Integration
- **Cloud Storage**: Automatic backup to Azure Blob Storage
- **Scalability**: Supports deployment on Azure App Service
- **Data Persistence**: RData files and plots stored in cloud

## Visualizations Generated

### Maps
- **Cholera Outbreak Maps**: District-level outbreak status with partner presence
- **Geographic Analysis**: Integration with neighboring countries (Malawi, Zimbabwe, Zambia)

### Time Series Analysis
- **Daily Trend Plots**: Province-level case progression
- **Weekly Bar Charts**: Comparative analysis across provinces
- **Bi-weekly District Analysis**: Detailed district-level trends

### Health System Monitoring
- **Hospital Occupancy Rates**: Capacity vs. current patients
- **Active Cases Distribution**: Geographic spread of active cases
- **Interactive Tables**: Detailed statistics with filtering capabilities

## Output Products

### Data Exports
- **Excel Files**: Structured data tables with date-specific exports
- **Partnership Reports**: District-level partner coverage analysis
- **Historical Data**: Complete time series datasets

### Visual Outputs
- **High-resolution Maps** (PNG/PDF)
- **Statistical Charts** (PNG/PDF)
- **Interactive HTML Tables**

## Technical Requirements

### R Packages
- **Spatial Analysis**: `sf`, `tmap`, `leaflet`
- **Data Processing**: `openxlsx`, `fuzzyjoin`, `stringi`
- **Visualization**: `ggplot2`, `plotly`, `reactable`
- **Web Framework**: `shiny`, `htmlwidgets`
- **Cloud Integration**: `AzureStor`

### System Dependencies
- **Python**: Required for fuzzy string matching (`reticulate`, `fuzzywuzzyR`)
- **Fonts**: UNICEF brand fonts for consistent styling
- **Geographic Data**: Mozambique administrative boundaries (GeoPackage format)

## Directory Structure

```
moz-cholera/
├── data/           # Input data files (bulletins, partnerships)
├── output/         # Exported Excel files and reports
├── plots/          # Generated visualizations (PNG/PDF)
├── rdas/           # R data objects for caching
├── images/         # Static images and assets
├── notebook/       # R Markdown reports
└── misc/           # Miscellaneous files and backups
```

## Usage Instructions

### Running the Complete Analysis
```r
# Execute the main workflow
source("moz-cholera.R")
```

### Launching the Dashboard
```r
# Start the Shiny application
shiny::runApp("app.R")
```

### Manual Step Execution
```r
# Run individual components
source("04-moz-cholera-dataload.R")  # Load data
source("05-moz-cholera-process.R")   # Process data
source("07-moz-cholera-plot.R")      # Generate plots
```

## Configuration

### Local Development
- Update paths in `03-moz-cholera-config.R`
- Ensure required GIS data is available
- Configure font paths for proper rendering

### Azure Deployment
- Set environment variable: `RUNNING_IN_AZURE = "TRUE"`
- Configure Azure storage credentials
- Update container and storage account settings

## Data Quality Features

### Automated Validation
- **District name matching** with fuzzy logic
- **Date format standardization** across sources
- **Missing data handling** with appropriate flagging

### Error Handling
- **File format detection** (PDF vs Excel)
- **Sheet name discovery** for Excel files
- **Graceful degradation** when data sources are unavailable

## Maintenance and Updates

### Regular Tasks
- Update administrative boundary data
- Refresh partner organization lists
- Monitor Azure storage usage
- Review and update district name matching rules

### Version Control
- Backup files stored in `_bak/` directories
- Date-stamped outputs for historical tracking
- Version-specific script backups (e.g., `app_20240307.R`)

## Support and Documentation

For technical issues or data questions, refer to:
- Individual script comments for detailed functionality
- Configuration files for system parameters
- Output directories for sample results

---

*This system supports UNICEF's cholera response efforts in Mozambique by providing timely, accurate, and actionable epidemiological intelligence.*
