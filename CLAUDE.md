# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This App Does

A Shiny dashboard for LandScan field technicians to QA/QC DJI Mavic 3 Multispectral drone imagery. Extracts EXIF metadata via ExifTool, analyzes flight patterns, spectral band quality, and — most critically — sun sensor stability across flight directions (N/S/E/W bidirectional comparison). Designed to run offline on field laptops. Must be designed so drone operators can easily download the app and get usable information in-field. Must be scalable for drone meta data other than the DJI Mavic 3 Multispectral.

## Running and Building

```bash
# Run in development (auto-installs missing packages):
Rscript launch_app.R

# Run directly (packages must already be installed):
Rscript -e "shiny::runApp()"

# Build standalone distributable (bundles R Portable + all packages):
Rscript build/build_standalone.R
# Output: build/output/DroneQAQC/ and build/output/DroneQAQC.zip

# Build Windows installer (requires Inno Setup 6+, run AFTER build_standalone.R):
# Open build/build_inno.iss in Inno Setup Compiler → Build → Compile
# Output: build/output/DroneQAQC_Setup.exe
```

**Caution:** `build/output/DroneQAQC/app/` contains a built copy of the app. When searching the codebase, ignore matches under `build/output/` — edit only the root `app.R` and `R/` files.

## Architecture

### Monolithic app.R (~1300 lines)

All UI and server logic lives in `app.R`. It sources two files at startup:
- `R/analysis_functions_enhanced.R` — all analysis/data processing functions
- `R/config.R` — configuration (falls back to inline defaults if missing)

At startup, app.R also locates Pandoc (RStudio env var → standalone bundle → system installs) and runs `validate_exiftool_setup()`; if ExifTool is broken, the Data Import tab shows a warning banner.

### UI Structure (sidebar tabs)

The dashboard has 7 tabs: Data Import, Batch Processing, Flight Analysis, Band Analysis, **Sun Sensor Analysis** (primary feature), Quality Assessment, and Interactive Maps. The sidebar also shows quick stats and a "Generate Report" button once data is loaded.

### Server Reactive Pipeline

Central state is in `reactiveValues` named `values`. When the user clicks "Load EXIF Data", processing runs in this exact sequence:

1. `exifr::read_exif()` with the `REQUIRED_EXIF_TAGS` whitelist (defined at top of analysis_functions_enhanced.R) → `values$exif_data`
2. `process_exif_data_enhanced()` → `values$processed_data` (normalizes DJI XMP field name variations)
3. `validate_exposure_settings()` → `values$exposure_validation`
4. `filter_quality_data_enhanced()` → returns a list; `$data` → `values$filtered_data` (removes non-multispectral/RGB images)
5. `analyze_flight_directions_enhanced()` → `values$direction_analysis` (also updates `filtered_data` with direction column)
6. `analyze_all_bands_enhanced()` → `values$band_results`
7. `analyze_sun_sensor_stability()` → `values$stability_analysis`
8. `assess_overall_quality()` → `values$quality_results`

`perform_advanced_qa()` → `values$advanced_qa` (altitude, gimbal, capture interval, band ratios, MAD outliers, coverage) is **lazy-loaded** the first time the Quality Assessment tab is visited, not during the load sequence.

All render functions (`output$*`) use `req(values$...)` to gate on data availability.

### analysis_functions_enhanced.R (~1800 lines)

Organized in sections:
- **Data Processing**: `process_exif_data_enhanced`, `filter_quality_data_enhanced`, `validate_exposure_settings` — EXIF field extraction with fallback column name lists for DJI XMP prefix variations
- **Flight Direction**: `categorize_flight_direction_enhanced` (yaw → 8 compass octants), `analyze_flight_directions_enhanced` (identifies main directions, filters by `min_direction_images`)
- **Sun Sensor**: `analyze_sun_sensor_stability` — CV per band per direction; bidirectional pairs (N-S, E-W) differenced; stability score 0-100
- **Band Analysis**: `analyze_single_band_enhanced`, `analyze_all_bands_enhanced` — per-band statistics, anomaly detection
- **Quality Assessment**: `assess_overall_quality`, `perform_advanced_qa` — lighting/exposure/GPS/sensor checks, altitude stability, gimbal nadir, capture regularity, band ratio consistency, MAD outlier detection, spatial coverage
- **Visualization**: `create_*` functions for plotly charts, leaflet maps, DT tables
- **Report**: `compile_report_data` assembles all results into a list for `report_template.Rmd`
- **Setup/Batch**: `validate_exiftool_setup`; `scan_flight_folders`, `process_single_flight` — process multiple flight folders

### Report Generation

Triggered via modal dialog → renders `report_template.Rmd` with `rmarkdown::render()` using xelatex. The template receives a `report_data` param (assembled by `compile_report_data()`). Includes mission info, exposure settings, QA pass/fail table, band stability scores, and optional outlier detection. PDF export requires `tinytex`.

### Batch Processing

The Batch Processing tab allows selecting a parent folder, scanning for sub-folders containing drone imagery (`scan_flight_folders`), and processing each independently (`process_single_flight`). Results can be exported as individual PDF reports.

## Key External Dependency: ExifTool

ExifTool (with Perl) is bundled in the repo at `exiftool/`. The perl path is set in `R/config.R` relative to the app working directory:
```r
perl_path = file.path("exiftool", "exiftool_files", "perl.exe")
```
Set to `""` to use system default. The build script copies ExifTool into the standalone bundle.

## Quality Thresholds (R/config.R)

| Threshold | Default | Meaning |
|---|---|---|
| `cv_threshold` | 15% | Max acceptable irradiance CV within a direction |
| `direction_diff_threshold` | 10% | Max acceptable difference between opposite directions |
| `stability_cv_threshold` | 15% | Overall sun sensor CV threshold |
| `min_direction_images` | 20 | Minimum images per direction to include in analysis |
| `min_direction_percent` | 10% | Minimum % of images for a direction to be "main" |
| `gps_jump_threshold` | 5 m | GPS anomaly detection distance |
| `lighting_deviation_threshold` | 20% | Lighting consistency check |
| `exposure_change_threshold` | 10% | Exposure consistency check |

## Package Dependencies

Core: `shiny`, `shinydashboard`, `shinyWidgets`, `shinyFiles`, `DT`, `plotly`, `leaflet`, `dplyr`, `tidyr`, `zoo`, `purrr`, `viridis`, `RColorBrewer`, `scales`, `exifr`, `htmlwidgets`, `rmarkdown`, `kableExtra`

Optional (PDF reports): `tinytex` + TinyTeX LaTeX distribution

## Band Name Derivation

Band names are derived from DJI Mavic 3M filenames by pattern matching suffixes: `_G`/`_GREEN` → Green, `_R`/`_RED` → Red, `_RE`/`_REDEDGE` → RedEdge, `_NIR` → NIR, `_W`/`_RGB` → RGB. The `derive_band_from_filename()` function handles this. If EXIF contains a `BandName` field, that takes priority.

## Code Style

Code like a professional who will QA their own code before delivering final product. Work like you are a professional of app design who also understands data science and physics of drone based sensors. Use comments sparingly. Only comment complex or non-obvious code.
