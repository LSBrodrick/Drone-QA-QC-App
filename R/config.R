# Configuration File

app_config <- list(
  organization_name = "LandScan",
  organization_logo = "landscan_logo.png",
  app_title = "Drone QA/QC Analysis",
  app_subtitle = "Professional Multispectral Analysis Tool",
  app_version = "1.0.0",

  # ExifTool perl path (set to "" to use system default)
  # Relative to app working directory; build script rewrites for standalone builds
  perl_path = file.path("exiftool", "exiftool_files", "perl.exe"),

  quality_thresholds = list(
    lighting_deviation_threshold = 20,
    exposure_change_threshold = 10,
    gps_jump_threshold = 5,
    cv_threshold = 15,
    min_direction_percent = 10,
    min_direction_images = 20,
    stability_cv_threshold = 15,
    direction_diff_threshold = 10
  )
)
