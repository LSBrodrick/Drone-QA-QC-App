# Drone QA/QC Analysis Application

A LandScan offline Shiny application for analyzing multispectral drone data quality, with a focus on **sun sensor stability analysis based on flight direction**.

## For End Users (Standalone Build)

The standalone build includes everything needed to run the app -- no R installation required.

### Install from ZIP
1. Download `DroneQAQC.zip`
2. Extract to any folder (e.g., `C:\DroneQAQC`)
3. Double-click `DroneQAQC.bat` (or the Desktop shortcut if you ran `install_shortcuts.bat`)
4. The app opens in your default web browser

### Install from Installer (.exe)
1. Run `DroneQAQC_Setup.exe`
2. Follow the prompts (no admin required)
3. Launch from Start Menu or Desktop shortcut

### What's Included
- R runtime (portable, no system install)
- All required R packages
- ExifTool + Perl (for EXIF metadata extraction)
- Pandoc (for PDF report rendering)
- TinyTeX (LaTeX distribution for PDF generation)

Everything works offline.

---

## For Developers (From Source)

### Prerequisites
- R 4.0+ installed
- ExifTool with Perl (included in `exiftool/` folder)

### Run the App
```r
# Auto-installs missing packages on first run:
Rscript launch_app.R

# Or if packages are already installed:
Rscript -e "shiny::runApp()"
```

### Required R Packages (19)
```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyFiles",
  "DT", "plotly", "leaflet", "leaflet.extras",
  "dplyr", "tidyr", "zoo", "purrr",
  "viridis", "RColorBrewer", "scales",
  "exifr", "htmlwidgets",
  "rmarkdown", "tinytex", "kableExtra"
))
```

### PDF Reports (Optional)
PDF export requires TinyTeX:
```r
install.packages("tinytex")
tinytex::install_tinytex()
```

### Build Standalone Distribution
```bash
# Creates build/output/DroneQAQC/ and DroneQAQC.zip
Rscript build/build_standalone.R

# Build Windows installer (requires Inno Setup 6+):
# Open build/build_inno.iss in Inno Setup Compiler -> Build -> Compile
```

**Build prerequisites:**
- ExifTool at `shiny_app/exiftool/`
- TinyTeX installed on build machine (for bundling)
- Pandoc available (bundled with RStudio, or install separately)
- Internet connection (downloads R Portable + packages)

### ExifTool Configuration
ExifTool is bundled in the `exiftool/` directory. The path is configured in `R/config.R`:
```r
perl_path = file.path("exiftool", "exiftool_files", "perl.exe")
```
Set to `""` to use system default Perl.

---

## Key Features

### Sun Sensor Stability Analysis (Primary Feature)
- **Direction-based irradiance comparison**: Compares irradiance readings across different flight directions
- **Bidirectional analysis**: Compares opposite flight directions (North vs South, East vs West) to detect sun angle effects
- **Stability scoring**: Per-band stability score (0-100)
- **Pass/fail assessment**: Automatic quality determination based on configurable thresholds

### Interpreting Results
- **CV < 10%**: Excellent stability
- **CV 10-15%**: Acceptable stability
- **CV > 15%**: Review recommended
- **Direction difference > 10%**: Potential sun angle issue

### Other Features
- Data Import with EXIF extraction from DJI Mavic 3 Multispectral images
- Flight direction analysis and pattern detection
- Per-band spectral assessment with anomaly detection
- Quality assessment (lighting, exposure, GPS, sun sensor)
- Interactive maps with offline tile support
- PDF report generation
- Batch processing for multiple flights

## Configuration

Edit `R/config.R` to customize:
- Organization name and logo
- Quality thresholds for pass/fail decisions
- ExifTool perl path

## Troubleshooting

### "No images found"
- Ensure your folder contains .jpg, .jpeg, .tif, or .tiff images with EXIF metadata

### "Missing irradiance data"
- Ensure images are from a multispectral camera with DLS (Downwelling Light Sensor)

### "All bands show Unknown"
- Filenames must contain band identifiers: `_G`, `_R`, `_RE`, `_NIR`, `_W`

### ExifTool errors
- Verify `exiftool/exiftool_files/perl.exe` exists
- Or set `perl_path = ""` in `R/config.R` to use system Perl

### PDF report fails
- Ensure TinyTeX is installed: `tinytex::is_tinytex()` should return `TRUE`
- Ensure Pandoc is available: `rmarkdown::pandoc_available()` should return `TRUE`

## File Structure
```
shiny_app/
+-- app.R                              # Main application
+-- launch_app.R                       # Launch script (auto-installs packages)
+-- report_template.Rmd                # PDF report template
+-- R/
|   +-- analysis_functions_enhanced.R  # Core analysis functions
|   +-- config.R                       # Configuration settings
+-- exiftool/                          # Bundled ExifTool + Perl
+-- www/
|   +-- custom.css                     # Custom styling
+-- build/
    +-- build_standalone.R             # Standalone distribution builder
    +-- build_inno.iss                 # Inno Setup installer script
```

## Version History

- **v2.1**: Bundled ExifTool, TinyTeX, Pandoc for offline deployment
- **v2.0**: Added sun sensor stability analysis with bidirectional comparison
- **v1.0**: Initial release with basic QA/QC functionality
