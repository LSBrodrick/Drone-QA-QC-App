# launch_app.R
# Quick launcher for the Drone QA/QC Analysis application

# Check and install required packages
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyFiles",
  "DT", "plotly", "leaflet",
  "dplyr", "tidyr", "zoo", "purrr",
  "viridis", "RColorBrewer", "scales",
  "exifr", "htmlwidgets", "tiff",
  "rmarkdown", "tinytex", "kableExtra"
)

# Fast check using requireNamespace() instead of slow installed.packages()
install_if_missing <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, dependencies = TRUE, repos = "https://cloud.r-project.org")
  }
}

# Install missing packages
cat("Checking required packages...\n")
install_if_missing(required_packages)

# Load shiny
library(shiny)

# Launch the app
cat("Launching Drone QA/QC Analysis app...\n")
runApp("app.R", launch.browser = TRUE)
