# ==============================================================================
# CONFIGURATION FILE
# Customize these settings for your organization
# ==============================================================================

app_config <- list(
  # Organization details
  organization_name = "LandScan",
  organization_logo = "landscan_logo.png",  # Place logo in www/ folder
  
  # App title
  app_title = "Drone QA/QC Analysis",
  app_subtitle = "Professional Multispectral Analysis Tool",
  
  # Color scheme (using hex color codes)
  colors = list(
    primary = "#27ae60",      # Main green
    primary_dark = "#1e4d2b", # Dark green
    primary_light = "#3a7850", # Light green
    secondary = "#2ecc71",    # Secondary green
    accent = "#3498db",       # Blue accent
    success = "#27ae60",      # Success green
    warning = "#f39c12",      # Warning orange
    danger = "#e74c3c",       # Danger red
    header_bg = "#2d5f3f",    # Header background
    header_logo_bg = "#1e4d2b" # Logo area background
  ),
  
  # Expected exposure settings
  exposure_settings = list(
    fstop = 5.6,
    rgb_shutter = 1/2000,
    green_shutter = 1/2500,
    red_edge_shutter = 1/1000,
    nir_shutter = 1/2500
  ),
  
  # Quality thresholds
  quality_thresholds = list(
    lighting_deviation_threshold = 20,  # % images with >10% deviation
    exposure_change_threshold = 10,     # % images with exposure changes
    gps_jump_threshold = 5,            # % images with GPS jumps
    cv_threshold = 15,                 # Coefficient of variation threshold
    min_direction_percent = 15,        # Minimum % for main direction
    min_direction_images = 30          # Minimum images for main direction
  ),
  
  # Feature flags
  features = list(
    show_logo = TRUE,
    show_organization_name = TRUE,
    enable_maps = TRUE,
    enable_reports = TRUE,
    enable_export = TRUE,
    enable_drag_drop = TRUE,
    validate_exposure = TRUE
  )
)