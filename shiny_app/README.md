# Drone QA/QC Analysis Application

A LandScan offline Shiny application for analyzing multispectral drone data quality.

## Features

- **Data Import**: Easy folder selection and EXIF data extraction
- **Flight Analysis**: Direction analysis and pattern detection
- **Band Analysis**: Individual spectral band assessment
- **Quality Assessment**: Comprehensive quality metrics and go/no-go decisions
- **Interactive Maps**: Offline-capable mapping with multiple visualization options
- **Report Generation**: Export analysis results in various formats

## Installation

1. Ensure you have R installed (version 4.0 or higher)
2. Install required packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyFiles",
  "DT", "plotly", "leaflet", "leaflet.extras",
  "dplyr", "ggplot2", "tidyr", "zoo",
  "viridis", "RColorBrewer", "scales",
  "exifr", "htmlwidgets", "rmarkdown", "tinytex"

))
