# ==============================================================================
# ENHANCED DRONE QA/QC ANALYSIS FUNCTIONS
# Based on the Mavic 3 Multispectral Flight QA/QC script
# ==============================================================================

# Libraries are loaded in app.R — no duplicate loading needed here

# ==============================================================================
# REQUIRED EXIF TAGS — limits ExifTool extraction for speed
# ==============================================================================
REQUIRED_EXIF_TAGS <- c(
  "FileName", "SourceFile",
  "Irradiance", "LightSensorStatus", "DLSStatus",
  "FlightYawDegree", "GimbalYawDegree",
  "FlightPitchDegree", "FlightRollDegree", "GimbalPitchDegree",
  "ExposureTime", "ShutterSpeed", "ShutterSpeedValue",
  "ISO", "ISOSpeed", "ISOSpeedRatings",
  "FNumber", "Aperture", "ApertureValue",
  "GPSLatitude", "GPSLongitude", "GPSDateStamp", "GPSTimeStamp",
  "RelativeAltitude", "GPSAltitude", "AbsoluteAltitude",
  "DateTimeOriginal", "CreateDate", "ModifyDate",
  "BandName",
  "Model", "Make", "CameraModelName",
  "FlightXSpeed", "FlightYSpeed", "SpeedX", "SpeedY",
  "ShutterCount", "ImageCount", "FileSequenceNumber"
)

# ==============================================================================
# 1. ENHANCED DATA PROCESSING
# ==============================================================================

#' Get first available column from a list of possible names
#' Returns the values from the first column that exists, or NA if none exist
get_first_available_column <- function(data, col_names) {
  for (col_name in col_names) {
    if (col_name %in% names(data)) {
      return(data[[col_name]])
    }
  }
  return(rep(NA, nrow(data)))
}

#' Derive band name from filename for DJI Mavic 3M (vectorized)
derive_band_from_filename <- function(filenames) {
  fnames <- toupper(basename(as.character(filenames)))
  dplyr::case_when(
    is.na(filenames) | filenames == "" ~ "Unknown",
    grepl("_MS_G\\.|_G\\.|_GREEN", fnames)          ~ "Green",
    grepl("_MS_R\\.|_R\\.|_RED\\.", fnames)          ~ "Red",
    grepl("_MS_RE\\.|_RE\\.|_REDEDGE", fnames)      ~ "RedEdge",
    grepl("_MS_NIR\\.|_NIR\\.", fnames)              ~ "NIR",
    grepl("_W\\.|_RGB\\.|DJI_", fnames)             ~ "RGB",
    TRUE                                             ~ "Unknown"
  )
}

#' Process EXIF data with enhanced field extraction
process_exif_data_enhanced <- function(exif_data) {
  
  # Extract columns with fallbacks
  irradiance_raw <- get_first_available_column(exif_data, c(
    "Irradiance", "XMP:Irradiance", "XMP-drone-dji:Irradiance", "Irradiance1"
  ))
  
  sun_sensor_raw <- get_first_available_column(exif_data, c(
    "LS_status", "XMP:LightSensorStatus", "XMP-drone-dji:LightSensorStatus", "DLSStatus"
  ))
  
  yaw_raw <- get_first_available_column(exif_data, c(
    "FlightYawDegree", "XMP:FlightYawDegree", "XMP-drone-dji:FlightYawDegree",
    "GimbalYawDegree", "XMP:GimbalYawDegree"
  ))
  
  pitch_raw <- get_first_available_column(exif_data, c(
    "FlightPitchDegree", "XMP:FlightPitchDegree", "XMP-drone-dji:FlightPitchDegree"
  ))
  
  roll_raw <- get_first_available_column(exif_data, c(
    "FlightRollDegree", "XMP:FlightRollDegree", "XMP-drone-dji:FlightRollDegree"
  ))
  
  gimbal_pitch_raw <- get_first_available_column(exif_data, c(
    "GimbalPitchDegree", "XMP:GimbalPitchDegree", "XMP-drone-dji:GimbalPitchDegree"
  ))
  
  exposure_raw <- get_first_available_column(exif_data, c(
    "ExposureTime", "ShutterSpeed", "ShutterSpeedValue"
  ))
  
  iso_raw <- get_first_available_column(exif_data, c(
    "ISO", "ISOSpeed", "ISOSpeedRatings"
  ))
  
  fnumber_raw <- get_first_available_column(exif_data, c(
    "FNumber", "Aperture", "ApertureValue"
  ))
  
  lat_raw <- get_first_available_column(exif_data, c(
    "GPSLatitude", "XMP:GPSLatitude", "Latitude"
  ))
  
  lon_raw <- get_first_available_column(exif_data, c(
    "GPSLongitude", "XMP:GPSLongitude", "Longitude"
  ))
  
  alt_raw <- get_first_available_column(exif_data, c(
    "RelativeAltitude", "XMP:RelativeAltitude", "GPSAltitude", "AbsoluteAltitude"
  ))
  
  timestamp_raw <- get_first_available_column(exif_data, c(
    "DateTimeOriginal", "CreateDate", "ModifyDate"
  ))
  
  band_raw <- get_first_available_column(exif_data, c(
    "BandName", "XMP:BandName", "XMP-drone-dji:BandName"
  ))
  
  filename_raw <- get_first_available_column(exif_data, c(
    "FileName", "SourceFile"
  ))

  sourcefile_raw <- get_first_available_column(exif_data, c("SourceFile"))

  # GPS clock is true UTC; DateTimeOriginal is the local camera clock.
  # Needed for solar position. Falls back to longitude-based offset below.
  gps_date_raw <- get_first_available_column(exif_data, c("GPSDateStamp"))
  gps_time_raw <- get_first_available_column(exif_data, c("GPSTimeStamp"))
  gps_utc <- as.POSIXct(paste(as.character(gps_date_raw), as.character(gps_time_raw)),
                        format = "%Y:%m:%d %H:%M:%S", tz = "UTC")

  # Camera model/make
  model_raw <- get_first_available_column(exif_data, c(
    "Model", "CameraModelName", "XMP:Model", "XMP-drone-dji:CameraModelName"
  ))

  make_raw <- get_first_available_column(exif_data, c(
    "Make", "XMP:Make", "XMP-drone-dji:Make"
  ))

  # Drone speed components (m/s in EXIF)
  speed_x_raw <- get_first_available_column(exif_data, c(
    "FlightXSpeed", "XMP:FlightXSpeed", "XMP-drone-dji:FlightXSpeed",
    "SpeedX", "XMP:SpeedX", "XMP-drone-dji:SpeedX"
  ))

  speed_y_raw <- get_first_available_column(exif_data, c(
    "FlightYSpeed", "XMP:FlightYSpeed", "XMP-drone-dji:FlightYSpeed",
    "SpeedY", "XMP:SpeedY", "XMP-drone-dji:SpeedY"
  ))

  # Shutter count
  shutter_count_raw <- get_first_available_column(exif_data, c(
    "ShutterCount", "ImageCount", "XMP:ShutterCount",
    "XMP-drone-dji:ShutterCount", "FileSequenceNumber"
  ))

  # Create processed data frame
  processed_data <- data.frame(
    Irradiance = as.numeric(irradiance_raw),
    sun_sensor_status = as.character(sun_sensor_raw),
    FlightYaw = as.numeric(yaw_raw),
    FlightPitch = as.numeric(pitch_raw),
    FlightRoll = as.numeric(roll_raw),
    GimbalPitch = as.numeric(gimbal_pitch_raw),
    exposure_time = as.numeric(exposure_raw),
    ISO = as.numeric(iso_raw),
    FNumber = as.numeric(fnumber_raw),
    Latitude = as.numeric(lat_raw),
    Longitude = as.numeric(lon_raw),
    Altitude = as.numeric(alt_raw),
    TimeStamp = as.POSIXct(timestamp_raw, format = "%Y:%m:%d %H:%M:%S", tz = "UTC"),
    TimeStampGPS = gps_utc,
    BandName = as.character(band_raw),
    FileName = as.character(filename_raw),
    SourceFile = as.character(sourcefile_raw),
    CameraModel = as.character(model_raw),
    CameraMake = as.character(make_raw),
    SpeedX = as.numeric(speed_x_raw),
    SpeedY = as.numeric(speed_y_raw),
    ShutterCount = as.numeric(shutter_count_raw),
    stringsAsFactors = FALSE
  )
  
  # Derive band name from filename if not in EXIF
  processed_data <- processed_data %>%
    mutate(
      BandName = ifelse(is.na(BandName) | BandName == "" | BandName == "NA",
                        derive_band_from_filename(FileName),
                        BandName),
      ImageIndex = row_number(),
      # Ground speed: horizontal magnitude in m/s, converted to mph
      GroundSpeed_ms = sqrt(ifelse(is.na(SpeedX), 0, SpeedX)^2 +
                            ifelse(is.na(SpeedY), 0, SpeedY)^2),
      GroundSpeed_mph = GroundSpeed_ms * 2.23694,
      # Combined camera rig string
      CameraRig = ifelse(!is.na(CameraMake) & !is.na(CameraModel),
                         paste(CameraMake, CameraModel),
                         ifelse(!is.na(CameraModel), CameraModel, NA_character_)),
      TimeStampUTC = dplyr::coalesce(
        TimeStampGPS,
        TimeStamp - round(Longitude / 15) * 3600,
        TimeStamp
      )
    ) %>%
    select(-TimeStampGPS) %>%
    arrange(TimeStamp)
  
  return(processed_data)
}

#' Enhanced quality data filtering
filter_quality_data_enhanced <- function(processed_data) {
  
  # Count issues before filtering
  issues <- list(
    missing_irradiance = sum(is.na(processed_data$Irradiance)),
    missing_gps = sum(is.na(processed_data$Latitude) | is.na(processed_data$Longitude)),
    missing_yaw = sum(is.na(processed_data$FlightYaw)),
    unknown_band = sum(processed_data$BandName == "Unknown", na.rm = TRUE)
  )
  
  # Apply minimal filtering
  filtered_data <- processed_data %>%
    filter(
      !is.na(Irradiance),
      !is.na(Latitude),
      !is.na(Longitude),
      !is.na(FlightYaw),
      BandName != "Unknown"
    )
  
  filtering_stats <- list(
    original_count = nrow(processed_data),
    filtered_count = nrow(filtered_data),
    removed_count = nrow(processed_data) - nrow(filtered_data),
    retention_rate = nrow(filtered_data) / nrow(processed_data) * 100,
    issues = issues
  )
  
  return(list(
    data = filtered_data,
    stats = filtering_stats
  ))
}

# ==============================================================================
# 2. EXPOSURE SETTINGS VALIDATION
# ==============================================================================

#' Validate exposure settings against expected values
validate_exposure_settings <- function(data) {
  
  expected_settings <- list(
    fstop = 5.6,
    rgb_shutter = 1/2000,
    green_shutter = 1/2500,
    red_edge_shutter = 1/1000,
    nir_shutter = 1/2500
  )
  
  validation_results <- data %>%
    group_by(BandName) %>%
    summarise(
      n_images = n(),
      mean_fstop = mean(FNumber, na.rm = TRUE),
      mean_shutter = mean(exposure_time, na.rm = TRUE),
      sd_fstop = sd(FNumber, na.rm = TRUE),
      sd_shutter = sd(exposure_time, na.rm = TRUE),
      .groups = 'drop'
    )
  
  issues <- c()
  all_valid <- TRUE
  
  fstop_check <- validation_results %>%
    filter(!is.na(mean_fstop)) %>%
    filter(abs(mean_fstop - expected_settings$fstop) > 0.5)
  
  if (nrow(fstop_check) > 0) {
    issues <- c(issues, paste("F-stop deviations in:", 
                              paste(fstop_check$BandName, collapse = ", ")))
    all_valid <- FALSE
  }
  
  for (i in 1:nrow(validation_results)) {
    band <- validation_results$BandName[i]
    actual_shutter <- validation_results$mean_shutter[i]
    
    if (is.na(actual_shutter)) next
    
    expected_shutter <- switch(as.character(band),
      "RGB" = expected_settings$rgb_shutter,
      "Green" = expected_settings$green_shutter,
      "Red" = expected_settings$green_shutter,
      "RedEdge" = expected_settings$red_edge_shutter,
      "NIR" = expected_settings$nir_shutter,
      1/2000
    )
    
    if (abs(actual_shutter - expected_shutter) > expected_shutter * 0.2) {
      issues <- c(issues, sprintf("%s band shutter speed: %.5f (expected ~%.5f)", 
                                  band, actual_shutter, expected_shutter))
      all_valid <- FALSE
    }
  }
  
  return(list(
    all_valid = all_valid,
    issues = issues,
    validation_results = validation_results,
    expected_settings = expected_settings
  ))
}

# ==============================================================================
# 3. ENHANCED FLIGHT DIRECTION ANALYSIS
# ==============================================================================

#' Normalize yaw angle differences
normalize_yaw_diff <- function(yaw_diff) {
  yaw_diff[yaw_diff > 180] <- yaw_diff[yaw_diff > 180] - 360
  yaw_diff[yaw_diff < -180] <- yaw_diff[yaw_diff < -180] + 360
  return(yaw_diff)
}

#' Enhanced flight direction categorization
categorize_flight_direction_enhanced <- function(yaw) {
  yaw <- ((yaw %% 360) + 360) %% 360
  
  specific_direction <- case_when(
    (yaw >= 337.5 | yaw < 22.5) ~ "North",
    (yaw >= 22.5 & yaw < 67.5) ~ "NorthEast",
    (yaw >= 67.5 & yaw < 112.5) ~ "East",
    (yaw >= 112.5 & yaw < 157.5) ~ "SouthEast",
    (yaw >= 157.5 & yaw < 202.5) ~ "South",
    (yaw >= 202.5 & yaw < 247.5) ~ "SouthWest",
    (yaw >= 247.5 & yaw < 292.5) ~ "West",
    (yaw >= 292.5 & yaw < 337.5) ~ "NorthWest",
    TRUE ~ "Unknown"
  )
  
  direction_group <- case_when(
    specific_direction %in% c("North", "South") ~ "NS_Axis",
    specific_direction %in% c("East", "West") ~ "EW_Axis",
    specific_direction %in% c("NorthEast", "SouthWest") ~ "NESW_Axis",
    specific_direction %in% c("NorthWest", "SouthEast") ~ "NWSE_Axis",
    TRUE ~ "Other"
  )
  
  mission_pattern <- case_when(
    specific_direction %in% c("North", "South") ~ "North-South_Mission",
    specific_direction %in% c("East", "West") ~ "East-West_Mission",
    specific_direction %in% c("NorthEast", "SouthWest") ~ "NE-SW_Mission",
    specific_direction %in% c("NorthWest", "SouthEast") ~ "NW-SE_Mission",
    TRUE ~ "Mixed_Mission"
  )
  
  return(list(
    specific_direction = specific_direction,
    direction_group = direction_group,
    mission_pattern = mission_pattern
  ))
}

#' Enhanced flight direction analysis
analyze_flight_directions_enhanced <- function(filtered_data) {
  
  direction_results <- categorize_flight_direction_enhanced(filtered_data$FlightYaw)
  
  data_with_directions <- filtered_data %>%
    mutate(
      YawDiff = normalize_yaw_diff(c(0, diff(FlightYaw))),
      turn_flag = abs(YawDiff) > 30,
      FlightDirection = direction_results$specific_direction,
      DirectionGroup = direction_results$direction_group,
      MissionPattern = direction_results$mission_pattern,
      direction_change = FlightDirection != lag(FlightDirection, default = first(FlightDirection)),
      flight_line = cumsum(direction_change | turn_flag)
    )
  
  direction_stats <- data_with_directions %>%
    group_by(FlightDirection) %>%
    summarise(
      n_images = n(),
      percent_of_total = n() / nrow(data_with_directions) * 100,
      n_bands = n_distinct(BandName),
      mean_irradiance = mean(Irradiance, na.rm = TRUE),
      sd_irradiance = sd(Irradiance, na.rm = TRUE),
      cv_irradiance = sd(Irradiance, na.rm = TRUE) / mean(Irradiance, na.rm = TRUE) * 100,
      time_span_mins = as.numeric(difftime(max(TimeStamp), min(TimeStamp), units = "mins")),
      .groups = 'drop'
    ) %>%
    arrange(desc(n_images))
  
  axis_stats <- data_with_directions %>%
    group_by(DirectionGroup) %>%
    summarise(
      n_images = n(),
      percent_of_total = n() / nrow(data_with_directions) * 100,
      n_directions = n_distinct(FlightDirection),
      mean_irradiance = mean(Irradiance, na.rm = TRUE),
      sd_irradiance = sd(Irradiance, na.rm = TRUE),
      cv_irradiance = sd(Irradiance, na.rm = TRUE) / mean(Irradiance, na.rm = TRUE) * 100,
      .groups = 'drop'
    ) %>%
    arrange(desc(n_images))
  
  MIN_PERCENT_FOR_MAIN <- 10
  MIN_IMAGES_FOR_MAIN <- 20
  MAX_MAIN_DIRECTIONS <- 4
  
  main_directions <- direction_stats %>%
    filter(
      percent_of_total >= MIN_PERCENT_FOR_MAIN,
      n_images >= MIN_IMAGES_FOR_MAIN
    ) %>%
    slice_head(n = MAX_MAIN_DIRECTIONS) %>%
    pull(FlightDirection)
  
  if (length(main_directions) < 2) {
    main_directions <- direction_stats %>%
      slice_head(n = 2) %>%
      pull(FlightDirection)
  }
  
  main_direction_coverage <- direction_stats %>%
    filter(FlightDirection %in% main_directions) %>%
    summarise(total_percent = sum(percent_of_total)) %>%
    pull(total_percent)
  
  return(list(
    data = data_with_directions,
    direction_stats = direction_stats,
    axis_stats = axis_stats,
    main_directions = main_directions,
    excluded_directions = setdiff(direction_stats$FlightDirection, main_directions),
    main_coverage = main_direction_coverage
  ))
}

# ==============================================================================
# 4. SUN SENSOR / IRRADIANCE STABILITY ANALYSIS (KEY FEATURE)
# ==============================================================================

#' Analyze sun sensor stability by flight direction
analyze_sun_sensor_stability <- function(data_with_directions) {
  
  direction_band_analysis <- data_with_directions %>%
    group_by(FlightDirection, BandName) %>%
    summarise(
      n_images = n(),
      mean_irradiance = mean(Irradiance, na.rm = TRUE),
      sd_irradiance = sd(Irradiance, na.rm = TRUE),
      cv_percent = (sd_irradiance / mean_irradiance) * 100,
      min_irradiance = min(Irradiance, na.rm = TRUE),
      max_irradiance = max(Irradiance, na.rm = TRUE),
      range_irradiance = max_irradiance - min_irradiance,
      range_percent = (range_irradiance / mean_irradiance) * 100,
      q25 = quantile(Irradiance, 0.25, na.rm = TRUE),
      q75 = quantile(Irradiance, 0.75, na.rm = TRUE),
      iqr = q75 - q25,
      .groups = 'drop'
    ) %>%
    filter(n_images >= 5)
  
  bidirectional_analysis <- data_with_directions %>%
    group_by(DirectionGroup, BandName) %>%
    summarise(
      n_images = n(),
      n_directions = n_distinct(FlightDirection),
      directions = paste(unique(FlightDirection), collapse = " / "),
      mean_irradiance = mean(Irradiance, na.rm = TRUE),
      sd_irradiance = sd(Irradiance, na.rm = TRUE),
      cv_percent = (sd_irradiance / mean_irradiance) * 100,
      .groups = 'drop'
    ) %>%
    filter(n_images >= 10)
  
  direction_differences <- direction_band_analysis %>%
    left_join(
      data_with_directions %>% 
        select(FlightDirection, DirectionGroup) %>% 
        distinct(),
      by = "FlightDirection"
    ) %>%
    group_by(DirectionGroup, BandName) %>%
    summarise(
      n_directions = n(),
      directions = paste(FlightDirection, collapse = " vs "),
      mean_diff_percent = if(n() >= 2) {
        abs(diff(mean_irradiance)) / mean(mean_irradiance) * 100
      } else { NA_real_ },
      max_irr = max(mean_irradiance),
      min_irr = min(mean_irradiance),
      irradiance_ratio = max_irr / min_irr,
      .groups = 'drop'
    ) %>%
    filter(n_directions >= 2)
  
  temporal_trends <- data_with_directions %>%
    group_by(BandName) %>%
    arrange(TimeStamp) %>%
    mutate(time_minutes = as.numeric(difftime(TimeStamp, min(TimeStamp), units = "mins"))) %>%
    summarise(
      temporal_correlation = cor(time_minutes, Irradiance, use = "complete.obs"),
      .groups = 'drop'
    ) %>%
    mutate(
      trend_direction = ifelse(temporal_correlation > 0.1, "Increasing",
                               ifelse(temporal_correlation < -0.1, "Decreasing", "Stable"))
    )
  
  stability_scores <- direction_band_analysis %>%
    group_by(BandName) %>%
    summarise(
      mean_cv = mean(cv_percent, na.rm = TRUE),
      max_cv = max(cv_percent, na.rm = TRUE),
      mean_range_percent = mean(range_percent, na.rm = TRUE),
      stability_score = 100 - min(mean_cv * 2, 100),
      .groups = 'drop'
    )
  
  STABILITY_CV_THRESHOLD <- 15
  DIRECTION_DIFF_THRESHOLD <- 10
  
  band_pass_fail <- stability_scores %>%
    mutate(
      cv_pass = mean_cv < STABILITY_CV_THRESHOLD,
      stability_pass = stability_score > 70,
      overall_pass = cv_pass & stability_pass,
      status = ifelse(overall_pass, "PASS", "REVIEW")
    )
  
  bidirectional_pass <- direction_differences %>%
    mutate(
      diff_pass = is.na(mean_diff_percent) | mean_diff_percent < DIRECTION_DIFF_THRESHOLD
    ) %>%
    group_by(BandName) %>%
    summarise(
      all_directions_consistent = all(diff_pass, na.rm = TRUE),
      max_direction_diff = max(mean_diff_percent, na.rm = TRUE),
      .groups = 'drop'
    )
  
  overall_pass <- all(band_pass_fail$overall_pass) && 
                  all(bidirectional_pass$all_directions_consistent, na.rm = TRUE)
  
  issues <- c()
  if (any(!band_pass_fail$cv_pass)) {
    issues <- c(issues, paste("High CV in bands:", 
                              paste(band_pass_fail$BandName[!band_pass_fail$cv_pass], collapse = ", ")))
  }
  if (any(!bidirectional_pass$all_directions_consistent, na.rm = TRUE)) {
    issues <- c(issues, "Significant irradiance differences between flight directions detected")
  }
  
  return(list(
    direction_band_analysis = direction_band_analysis,
    bidirectional_analysis = bidirectional_analysis,
    direction_differences = direction_differences,
    temporal_trends = temporal_trends,
    stability_scores = stability_scores,
    band_pass_fail = band_pass_fail,
    bidirectional_pass = bidirectional_pass,
    overall_pass = overall_pass,
    issues = issues
  ))
}

#' Create sun sensor stability visualization (native plotly)
create_sun_sensor_stability_plot <- function(stability_analysis) {
  d <- stability_analysis$direction_band_analysis
  bands <- unique(d$BandName)
  n_bands <- length(bands)
  ncol <- 2
  nrow <- ceiling(n_bands / ncol)

  p <- plotly::plot_ly()
  fig <- plotly::subplot(
    lapply(seq_along(bands), function(i) {
      bd <- d[d$BandName == bands[i], ]
      plotly::plot_ly(bd, x = ~FlightDirection, y = ~mean_irradiance, type = "bar",
                      error_y = list(type = "data", array = bd$sd_irradiance),
                      marker = list(opacity = 0.8),
                      showlegend = FALSE) %>%
        plotly::layout(annotations = list(list(
          text = bands[i], xref = "paper", yref = "paper",
          x = 0.5, y = 1.05, showarrow = FALSE, font = list(size = 13, face = "bold")
        )))
    }),
    nrows = nrow, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE
  ) %>%
    plotly::layout(
      title = list(text = "Sun Sensor Stability by Flight Direction<br><sub>Error bars show +/- 1 SD</sub>"),
      showlegend = FALSE
    )

  return(fig)
}

#' Create bidirectional comparison plot (native plotly)
create_bidirectional_plot <- function(stability_analysis) {
  d <- stability_analysis$direction_differences
  if (nrow(d) == 0) return(NULL)

  bands <- unique(d$BandName)
  fig <- plotly::plot_ly()
  for (band in bands) {
    bd <- d[d$BandName == band, ]
    fig <- fig %>% plotly::add_trace(
      data = bd, x = ~DirectionGroup, y = ~mean_diff_percent,
      type = "bar", name = band, opacity = 0.8
    )
  }
  fig <- fig %>%
    plotly::layout(
      barmode = "group",
      title = list(text = "Irradiance Difference Between Opposite Flight Directions<br><sub>Lower values indicate better sun sensor stability</sub>"),
      xaxis = list(title = "Direction Axis"),
      yaxis = list(title = "Mean Difference (%)"),
      legend = list(orientation = "h", y = -0.2),
      shapes = list(list(
        type = "line", x0 = 0, x1 = 1, xref = "paper",
        y0 = 10, y1 = 10, line = list(color = "red", dash = "dash", width = 2)
      )),
      annotations = list(list(
        x = 1, xref = "paper", y = 10.5, text = "10% threshold",
        showarrow = FALSE, font = list(color = "red")
      ))
    )
  return(fig)
}

#' Create stability score gauge plot (native plotly)
create_stability_gauge <- function(stability_analysis) {
  scores <- stability_analysis$stability_scores %>%
    mutate(
      color = case_when(
        stability_score >= 80 ~ "#27AE60",
        stability_score >= 60 ~ "#F39C12",
        TRUE ~ "#E74C3C"
      ),
      status = case_when(
        stability_score >= 80 ~ "Good",
        stability_score >= 60 ~ "Acceptable",
        TRUE ~ "Poor"
      )
    )

  fig <- plotly::plot_ly(scores, x = ~BandName, y = ~stability_score,
                         type = "bar", color = ~status,
                         colors = c("Good" = "#27AE60", "Acceptable" = "#F39C12", "Poor" = "#E74C3C"),
                         opacity = 0.8,
                         text = ~sprintf("%.0f", stability_score), textposition = "outside") %>%
    plotly::layout(
      title = list(text = "Sun Sensor Stability Scores by Band<br><sub>Score > 70 indicates acceptable stability</sub>"),
      xaxis = list(title = "Spectral Band"),
      yaxis = list(title = "Stability Score", range = c(0, 110), dtick = 20),
      legend = list(orientation = "h", y = -0.2),
      shapes = list(list(
        type = "line", x0 = 0, x1 = 1, xref = "paper",
        y0 = 70, y1 = 70, line = list(color = "gray50", dash = "dash")
      ))
    )
  return(fig)
}

# ==============================================================================
# 4B. LIGHTING CONSISTENCY ANALYSIS
# ==============================================================================

#' Solar elevation angle in degrees (NOAA simplified algorithm, vectorized).
#' Accurate to ~0.1 deg — ample for irradiance detrending.
solar_elevation_deg <- function(time_utc, lat, lon) {
  doy <- as.numeric(strftime(time_utc, format = "%j", tz = "UTC"))
  hr  <- as.numeric(strftime(time_utc, format = "%H", tz = "UTC")) +
         as.numeric(strftime(time_utc, format = "%M", tz = "UTC")) / 60 +
         as.numeric(strftime(time_utc, format = "%S", tz = "UTC")) / 3600
  gamma <- 2 * pi / 365 * (doy - 1 + (hr - 12) / 24)
  eqtime <- 229.18 * (0.000075 + 0.001868 * cos(gamma) - 0.032077 * sin(gamma) -
                      0.014615 * cos(2 * gamma) - 0.040849 * sin(2 * gamma))
  decl <- 0.006918 - 0.399912 * cos(gamma) + 0.070257 * sin(gamma) -
          0.006758 * cos(2 * gamma) + 0.000907 * sin(2 * gamma) -
          0.002697 * cos(3 * gamma) + 0.00148 * sin(3 * gamma)
  tst <- hr * 60 + eqtime + 4 * lon
  ha_rad <- (tst / 4 - 180) * pi / 180
  lat_rad <- lat * pi / 180
  cos_zen <- sin(lat_rad) * sin(decl) + cos(lat_rad) * cos(decl) * cos(ha_rad)
  90 - acos(pmin(1, pmax(-1, cos_zen))) * 180 / pi
}

#' Analyze lighting consistency from EXIF-derived signals.
#'
#' Methods (strongest first):
#' 1. Solar-elevation detrending: clear-sky irradiance ~ sin(elevation), so the
#'    CV of Irradiance/sin(elev) separates benign solar drift from clouds/faults.
#' 2. Robust step detection (rolling median + MAD z-score) on the detrended
#'    series flags discrete cloud events; row-turn transients are masked.
#' 3. Auto-exposure EV cross-check: camera-side scene brightness corroborates
#'    sun-sensor events (only informative when exposure is not fixed).
#' 4. Yaw regression on the detrended residual detects heading-dependent
#'    irradiance = DLS cosine-response error.
analyze_lighting_consistency <- function(data_with_directions, thresholds = NULL) {
  th <- list(cv_stable = 5, cv_review = 12, step_z = 3.5, step_depth_pct = 8,
             deep_depth_pct = 15, yaw_r2_limit = 0.3, turn_yaw_deg = 30)
  if (!is.null(thresholds)) th[names(thresholds)] <- thresholds

  data <- data_with_directions %>%
    filter(!is.na(Irradiance), Irradiance > 0, !is.na(TimeStamp))
  if (nrow(data) < 20) {
    return(list(available = FALSE,
                message = "Too few images with irradiance data for lighting analysis"))
  }
  if (!"YawDiff" %in% names(data)) data$YawDiff <- 0

  lat0 <- stats::median(data$Latitude, na.rm = TRUE)
  lon0 <- stats::median(data$Longitude, na.rm = TRUE)
  ts_utc <- if ("TimeStampUTC" %in% names(data)) data$TimeStampUTC else data$TimeStamp
  data$SolarElev <- solar_elevation_deg(ts_utc, lat0, lon0)

  # Sun "below horizon" means the UTC estimate is wrong — fall back to raw CV
  solar_ok <- is.finite(lat0) && is.finite(lon0) &&
              isTRUE(stats::median(data$SolarElev, na.rm = TRUE) > 5)

  per_band_series <- lapply(split(data, data$BandName), function(d) {
    d <- d[order(d$TimeStamp), ]
    n <- nrow(d)
    d$time_min <- as.numeric(difftime(d$TimeStamp, min(d$TimeStamp), units = "mins"))
    d$sin_elev <- if (solar_ok) pmax(sin(d$SolarElev * pi / 180), 0.05) else rep(1, n)
    d$IrrDetrended <- d$Irradiance / d$sin_elev

    if (n >= 7) {
      k <- min(21L, if (n %% 2 == 1L) n else n - 1L)
      rmed <- zoo::rollmedian(d$IrrDetrended, k = k, fill = NA, align = "center")
      rmed[is.na(rmed)] <- stats::median(d$IrrDetrended, na.rm = TRUE)
    } else {
      rmed <- rep(stats::median(d$IrrDetrended, na.rm = TRUE), n)
    }
    resid <- d$IrrDetrended - rmed
    mad_r <- stats::mad(resid, na.rm = TRUE)
    d$RollingMedian <- rmed
    d$RelDeviation <- resid / pmax(rmed, 1e-9) * 100
    d$StepFlag <- mad_r > 0 &
      abs(resid) / mad_r > th$step_z &
      abs(d$RelDeviation) > th$step_depth_pct &
      abs(d$YawDiff) <= th$turn_yaw_deg
    d$StepFlag[is.na(d$StepFlag)] <- FALSE
    d$EV <- log2(pmax(d$FNumber, 0.1)^2 / pmax(d$exposure_time, 1e-6)) -
            log2(pmax(d$ISO, 1) / 100)
    d
  })
  series <- dplyr::bind_rows(per_band_series)

  per_band <- series %>%
    group_by(BandName) %>%
    summarise(
      n_images = n(),
      raw_cv = sd(Irradiance) / mean(Irradiance) * 100,
      detrended_cv = sd(IrrDetrended) / mean(IrrDetrended) * 100,
      solar_change_pct = (dplyr::last(sin_elev) - dplyr::first(sin_elev)) /
                         mean(sin_elev) * 100,
      n_events = { r <- rle(StepFlag); sum(r$lengths >= 2 & r$values) },
      affected_pct = sum(StepFlag) / n() * 100,
      max_depth_pct = if (any(StepFlag)) max(abs(RelDeviation)[StepFlag]) else 0,
      ev_informative = isTRUE(sd(exposure_time, na.rm = TRUE) > 1e-6) ||
                       isTRUE(sd(ISO, na.rm = TRUE) > 0.5),
      irr_ev_corr = if (ev_informative && n() >= 10 && raw_cv > 2 &&
                        isTRUE(sd(EV, na.rm = TRUE) > 0)) {
        suppressWarnings(cor(log(Irradiance), EV, method = "spearman",
                             use = "complete.obs"))
      } else NA_real_,
      yaw_r2 = if (n() >= 30 && dplyr::n_distinct(FlightYaw) >= 4) {
        m <- stats::lm(IrrDetrended / mean(IrrDetrended) ~
                         sin(FlightYaw * pi / 180) + cos(FlightYaw * pi / 180))
        summary(m)$r.squared
      } else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(
      classification = case_when(
        !is.na(yaw_r2) & yaw_r2 > th$yaw_r2_limit ~ "Heading-dependent (sensor)",
        n_events > 0 & max_depth_pct > th$deep_depth_pct ~ "Cloud-interrupted",
        n_events > 0 ~ "Thin cloud / review",
        detrended_cv >= th$cv_review ~ "Erratic",
        detrended_cv < th$cv_stable & raw_cv - detrended_cv > 3 ~ "Solar drift (benign)",
        detrended_cv < th$cv_stable ~ "Stable",
        TRUE ~ "Mostly stable"
      ),
      score = pmax(0, pmin(100, round(
        100 - 3 * detrended_cv - 10 * n_events -
          25 * (!is.na(yaw_r2) & yaw_r2 > th$yaw_r2_limit)
      ))),
      pass = classification %in% c("Stable", "Solar drift (benign)", "Mostly stable")
    )

  severity <- c("Stable" = 0, "Solar drift (benign)" = 1, "Mostly stable" = 2,
                "Thin cloud / review" = 3, "Erratic" = 4, "Cloud-interrupted" = 5,
                "Heading-dependent (sensor)" = 6)
  overall_class <- per_band$classification[which.max(severity[per_band$classification])]

  issues <- c()
  bad <- per_band[!per_band$pass, ]
  if (nrow(bad) > 0) {
    issues <- sprintf("Lighting %s: %s (detrended CV %.1f%%, %d events)",
                      bad$BandName, bad$classification, bad$detrended_cv, bad$n_events)
  }

  list(
    available = TRUE,
    per_band = per_band,
    series = series %>%
      select(any_of(c("BandName", "TimeStamp", "time_min", "Irradiance",
                      "IrrDetrended", "sin_elev", "RollingMedian", "RelDeviation",
                      "StepFlag", "EV", "SolarElev"))),
    solar = list(
      ok = solar_ok,
      elev_start = round(data$SolarElev[which.min(as.numeric(ts_utc))], 1),
      elev_end = round(data$SolarElev[which.max(as.numeric(ts_utc))], 1)
    ),
    overall_class = overall_class,
    overall_score = min(per_band$score),
    overall_pass = all(per_band$pass),
    issues = issues
  )
}

#' Irradiance timeline with clear-sky reference and detected cloud events
create_lighting_timeline_plot <- function(lighting_analysis, band_name) {
  d <- lighting_analysis$series %>% filter(BandName == band_name)
  if (nrow(d) == 0) return(NULL)

  fig <- plotly::plot_ly() %>%
    plotly::add_trace(data = d, x = ~time_min, y = ~Irradiance,
                      type = "scatter", mode = "markers",
                      marker = list(size = 4, opacity = 0.45, color = "#2D5F3F"),
                      name = "Irradiance") %>%
    plotly::add_trace(data = d, x = ~time_min, y = ~RollingMedian * sin_elev,
                      type = "scatter", mode = "lines",
                      line = list(color = "#27AE60", width = 2),
                      name = "Rolling median")

  if (isTRUE(lighting_analysis$solar$ok)) {
    scale <- mean(d$Irradiance) / mean(d$sin_elev)
    fig <- fig %>%
      plotly::add_trace(data = d, x = ~time_min, y = ~sin_elev * scale,
                        type = "scatter", mode = "lines",
                        line = list(color = "#F39C12", width = 2, dash = "dash"),
                        name = "Clear-sky reference (solar geometry)")
  }

  flagged <- d[d$StepFlag, ]
  if (nrow(flagged) > 0) {
    fig <- fig %>%
      plotly::add_trace(data = flagged, x = ~time_min, y = ~Irradiance,
                        type = "scatter", mode = "markers",
                        marker = list(size = 9, symbol = "triangle-down",
                                      color = "#E74C3C"),
                        name = "Cloud event")
  }

  fig %>% plotly::layout(
    title = list(text = paste0(band_name,
      " — Irradiance Timeline<br><sub>Dashed line shows expected clear-sky shape from solar elevation</sub>")),
    xaxis = list(title = "Time (minutes from start)"),
    yaxis = list(title = "Irradiance"),
    legend = list(orientation = "h", y = -0.2)
  )
}

#' Sun-sensor (up-looking) vs auto-exposure (down-looking) corroboration
create_lighting_ev_plot <- function(lighting_analysis, band_name) {
  d <- lighting_analysis$series %>% filter(BandName == band_name)
  pb <- lighting_analysis$per_band %>% filter(BandName == band_name)
  if (nrow(d) == 0 || nrow(pb) == 0 || !isTRUE(pb$ev_informative)) return(NULL)

  d <- d %>%
    mutate(IrrNorm = Irradiance / stats::median(Irradiance, na.rm = TRUE),
           SceneNorm = 2^(EV - stats::median(EV, na.rm = TRUE)))

  corr_txt <- if (!is.na(pb$irr_ev_corr)) {
    sprintf("Spearman correlation: %.2f — simultaneous dips confirm real lighting events", pb$irr_ev_corr)
  } else {
    "Correlation unavailable (insufficient irradiance variation)"
  }

  plotly::plot_ly() %>%
    plotly::add_trace(data = d, x = ~time_min, y = ~IrrNorm,
                      type = "scatter", mode = "lines",
                      line = list(color = "#2D5F3F", width = 1.5),
                      name = "Sun sensor (relative)") %>%
    plotly::add_trace(data = d, x = ~time_min, y = ~SceneNorm,
                      type = "scatter", mode = "lines",
                      line = list(color = "#8E44AD", width = 1.5),
                      name = "Scene brightness via auto-exposure (relative)") %>%
    plotly::layout(
      title = list(text = paste0(band_name, " — Two-Sensor Corroboration<br><sub>",
                                 corr_txt, "</sub>")),
      xaxis = list(title = "Time (minutes from start)"),
      yaxis = list(title = "Relative level (1 = flight median)"),
      legend = list(orientation = "h", y = -0.2)
    )
}

#' Lighting score bar chart by band
create_lighting_score_plot <- function(lighting_analysis) {
  d <- lighting_analysis$per_band %>%
    mutate(status = ifelse(pass, "Pass", "Review"))
  plotly::plot_ly(d, x = ~BandName, y = ~score, type = "bar",
                  color = ~status,
                  colors = c("Pass" = "#27AE60", "Review" = "#E74C3C"),
                  opacity = 0.85,
                  text = ~classification, textposition = "outside") %>%
    plotly::layout(
      title = list(text = "Lighting Consistency Score by Band<br><sub>Score ≥ 70 indicates consistent lighting</sub>"),
      xaxis = list(title = "Spectral Band"),
      yaxis = list(title = "Lighting Score", range = c(0, 115), dtick = 20),
      legend = list(orientation = "h", y = -0.2),
      shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                         y0 = 70, y1 = 70,
                         line = list(color = "gray", dash = "dash")))
    )
}

#' Deep scan: sampled raw-TIFF reflectance proxy.
#' Reads a time-stratified sample of multispectral TIFFs, computes a trimmed
#' central-crop mean DN, normalizes by exposure to a radiance proxy, and divides
#' by DLS irradiance. A stable reflectance proxy means lighting was consistent
#' AND the sun-sensor compensation would actually flatten the imagery.
deep_scan_reflectance_proxy <- function(filtered_data, max_images = 48, progress = NULL) {
  if (!requireNamespace("tiff", quietly = TRUE)) {
    return(list(available = FALSE,
                message = "Package 'tiff' is not installed. Run install.packages(\"tiff\") to enable the deep scan."))
  }
  if (!"SourceFile" %in% names(filtered_data)) {
    return(list(available = FALSE,
                message = "Full image paths unavailable — reload the data to enable the deep scan."))
  }

  ms <- filtered_data %>%
    filter(BandName != "RGB",
           grepl("\\.tiff?$", SourceFile, ignore.case = TRUE),
           is.finite(exposure_time), exposure_time > 0,
           is.finite(ISO), ISO > 0,
           is.finite(Irradiance), Irradiance > 0)
  if (nrow(ms) < 8) {
    return(list(available = TRUE, success = FALSE,
                message = "Not enough multispectral TIFFs with exposure data for a deep scan."))
  }

  quota <- max(8, ceiling(max_images / dplyr::n_distinct(ms$BandName)))
  sampled <- ms %>%
    group_by(BandName) %>%
    arrange(TimeStamp) %>%
    slice(unique(round(seq(1, dplyr::n(), length.out = min(dplyr::n(), quota))))) %>%
    ungroup()
  sampled <- sampled[file.exists(sampled$SourceFile), ]
  if (nrow(sampled) < 8) {
    return(list(available = TRUE, success = FALSE,
                message = "Sampled image files not found on disk (drive disconnected?)."))
  }

  n <- nrow(sampled)
  mean_dn <- vapply(seq_len(n), function(i) {
    if (!is.null(progress)) progress(i / n)
    img <- tryCatch(tiff::readTIFF(sampled$SourceFile[i], as.is = TRUE),
                    error = function(e) NULL)
    if (is.null(img)) return(NA_real_)
    if (length(dim(img)) == 3) img <- img[, , 1]
    nr <- nrow(img); nc <- ncol(img)
    # Central 50% crop sidesteps vignetting; trimmed mean dodges speculars
    mean(img[(nr %/% 4):(3 * nr %/% 4), (nc %/% 4):(3 * nc %/% 4)], trim = 0.05)
  }, numeric(1))

  sampled$MeanDN <- mean_dn
  scanned <- sampled %>% filter(is.finite(MeanDN), MeanDN > 0)
  if (nrow(scanned) < 8) {
    return(list(available = TRUE, success = FALSE,
                message = "Could not read enough TIFFs — files may be corrupt or in an unsupported format."))
  }

  results <- scanned %>%
    mutate(RadianceProxy = MeanDN / (exposure_time * ISO),
           ReflectProxy = RadianceProxy / Irradiance) %>%
    group_by(BandName) %>%
    summarise(
      n_sampled = dplyr::n(),
      reflect_cv = sd(ReflectProxy) / mean(ReflectProxy) * 100,
      dls_tracking = if (dplyr::n() >= 6 && isTRUE(sd(RadianceProxy) > 0) &&
                         isTRUE(sd(Irradiance) > 0)) {
        suppressWarnings(cor(RadianceProxy, Irradiance, method = "spearman"))
      } else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(pass = reflect_cv < 15)

  list(
    available = TRUE, success = TRUE,
    results = results,
    n_read = nrow(scanned),
    overall_pass = all(results$pass),
    message = sprintf(
      "Read %d of %d sampled TIFFs. Reflectance proxy CV < 15%% per band indicates the imagery itself confirms consistent, well-compensated lighting.",
      nrow(scanned), n)
  )
}

# ==============================================================================
# 5. ENHANCED BAND ANALYSIS
# ==============================================================================

#' Analyze single band with enhanced metrics
analyze_single_band_enhanced <- function(band_data, band_name) {
  
  basic_stats <- band_data %>%
    summarise(
      n_images = n(),
      mean_irradiance = mean(Irradiance, na.rm = TRUE),
      sd_irradiance = sd(Irradiance, na.rm = TRUE),
      cv_percent = sd_irradiance / mean_irradiance * 100,
      min_irradiance = min(Irradiance, na.rm = TRUE),
      max_irradiance = max(Irradiance, na.rm = TRUE),
      range_irradiance = max_irradiance - min_irradiance
    )
  
  direction_stats <- band_data %>%
    group_by(FlightDirection) %>%
    summarise(
      n_images = n(),
      mean_irr = mean(Irradiance, na.rm = TRUE),
      sd_irr = sd(Irradiance, na.rm = TRUE),
      cv_irr = sd_irr / mean_irr * 100,
      min_irr = min(Irradiance, na.rm = TRUE),
      max_irr = max(Irradiance, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(n_images >= 10)
  
  band_data_with_rolling <- band_data %>%
    group_by(FlightDirection) %>%
    arrange(TimeStamp) %>%
    mutate(
      RollingAvg = if(n() >= 3) {
        rollmean(Irradiance, k = min(11L, n()), fill = NA, align = "center")
      } else {
        rep(mean(Irradiance, na.rm = TRUE), n())
      },

      # Fast rolling SD via E[X^2] - E[X]^2 identity (avoids slow rollapply)
      RollingSD = if(n() >= 3) {
        rm_sq <- rollmean(Irradiance^2, k = min(11L, n()), fill = NA, align = "center")
        sqrt(pmax(0, rm_sq - RollingAvg^2))
      } else {
        rep(sd(Irradiance, na.rm = TRUE), n())
      },

      IrrDiff = abs(Irradiance - RollingAvg),
      IrrCV = ifelse(!is.na(RollingSD) & RollingAvg > 0, RollingSD / RollingAvg * 100, NA),
      IrrFlag = !is.na(RollingAvg) & IrrDiff > 0.1 * abs(RollingAvg)
    ) %>%
    ungroup()
  
  anomaly_stats <- band_data_with_rolling %>%
    group_by(FlightDirection) %>%
    summarise(
      n_total = n(),
      n_flagged = sum(IrrFlag, na.rm = TRUE),
      percent_flagged = n_flagged / n_total * 100,
      .groups = 'drop'
    ) %>%
    filter(n_total >= 10)
  
  return(list(
    band_name = band_name,
    basic_stats = basic_stats,
    direction_stats = direction_stats,
    processed_data = band_data_with_rolling,
    anomaly_stats = anomaly_stats
  ))
}

#' Analyze all bands
analyze_all_bands_enhanced <- function(filtered_data) {
  bands <- unique(filtered_data$BandName)
  band_list <- lapply(bands, function(band) {
    band_data <- filtered_data %>% filter(BandName == band)
    if (nrow(band_data) > 0) analyze_single_band_enhanced(band_data, band) else NULL
  })
  names(band_list) <- bands
  Filter(Negate(is.null), band_list)
}

# ==============================================================================
# 6. QUALITY ASSESSMENT
# ==============================================================================

#' Assess overall quality with stability metrics.
#' GPS consistency is reported as informational only — imagery is georeferenced
#' downstream, so GPS jitter does not gate the QA verdict.
assess_overall_quality <- function(filtered_data, band_results, stability_analysis = NULL,
                                   lighting_analysis = NULL) {

  irr_flags <- filtered_data %>%
    group_by(BandName, FlightDirection) %>%
    mutate(
      mean_irr = mean(Irradiance, na.rm = TRUE),
      deviation = abs(Irradiance - mean_irr) / mean_irr * 100
    ) %>%
    ungroup() %>%
    summarise(
      high_deviation_rate = sum(deviation > 10, na.rm = TRUE) / n() * 100
    )

  # Prefer the detrended lighting analysis when available; the raw deviation
  # rate cannot distinguish benign solar drift from clouds
  if (!is.null(lighting_analysis) && isTRUE(lighting_analysis$available)) {
    lighting_consistency <- list(
      pass = lighting_analysis$overall_pass,
      high_deviation_rate = irr_flags$high_deviation_rate,
      classification = lighting_analysis$overall_class,
      score = lighting_analysis$overall_score
    )
  } else {
    lighting_consistency <- list(
      pass = irr_flags$high_deviation_rate < 20,
      high_deviation_rate = irr_flags$high_deviation_rate,
      classification = NULL,
      score = NULL
    )
  }

  exp_changes <- filtered_data %>%
    arrange(TimeStamp) %>%
    mutate(
      exp_change = abs(c(0, diff(exposure_time))),
      iso_change = abs(c(0, diff(ISO)))
    ) %>%
    summarise(
      exp_change_rate = sum(exp_change > 0.001 | iso_change > 100, na.rm = TRUE) / n() * 100
    )

  exposure_consistency <- list(
    pass = exp_changes$exp_change_rate < 10,
    change_rate = exp_changes$exp_change_rate
  )

  gps_jumps <- filtered_data %>%
    arrange(TimeStamp) %>%
    mutate(
      lat_diff = abs(c(0, diff(Latitude))),
      lon_diff = abs(c(0, diff(Longitude))),
      gps_jump = lat_diff > 0.00005 | lon_diff > 0.00005
    ) %>%
    summarise(
      jump_rate = sum(gps_jump) / n() * 100
    )

  gps_consistency <- list(
    pass = gps_jumps$jump_rate < 5,
    jump_rate = gps_jumps$jump_rate,
    informational = TRUE
  )

  # Use pre-computed stability if provided (avoids expensive duplicate computation)
  if (is.null(stability_analysis)) {
    stability_analysis <- analyze_sun_sensor_stability(filtered_data)
  }

  # GPS intentionally excluded — informational only
  overall_pass <- lighting_consistency$pass &
                  exposure_consistency$pass &
                  stability_analysis$overall_pass

  issues <- c()
  if (!lighting_consistency$pass) {
    issues <- c(issues, if (!is.null(lighting_consistency$classification)) {
      paste("Lighting:", lighting_consistency$classification)
    } else "Lighting instability detected")
  }
  if (!exposure_consistency$pass) issues <- c(issues, "Exposure variations detected")
  issues <- c(issues, stability_analysis$issues)
  
  return(list(
    lighting_consistency = lighting_consistency,
    exposure_consistency = exposure_consistency,
    gps_consistency = gps_consistency,
    stability_analysis = stability_analysis,
    overall_pass = overall_pass,
    issues = issues
  ))
}

# ==============================================================================
# 7. VISUALIZATION FUNCTIONS
# ==============================================================================

#' Create direction distribution plot (native plotly)
create_direction_distribution_plot <- function(direction_analysis) {
  d <- direction_analysis$direction_stats %>%
    arrange(n_images) %>%
    mutate(
      is_main = FlightDirection %in% direction_analysis$main_directions,
      bar_color = ifelse(is_main, "#27AE60", "#E74C3C"),
      label = sprintf("%.1f%% (%d)", percent_of_total, n_images)
    )

  fig <- plotly::plot_ly(d, y = ~FlightDirection, x = ~percent_of_total,
                         type = "bar", orientation = "h",
                         marker = list(color = d$bar_color, opacity = 0.8),
                         text = ~label, textposition = "outside",
                         hoverinfo = "text",
                         hovertext = ~paste(FlightDirection, "<br>", label),
                         showlegend = FALSE) %>%
    plotly::layout(
      title = list(text = "Flight Direction Distribution<br><sub>Green = main directions used for analysis</sub>"),
      xaxis = list(title = "Percentage of Images"),
      yaxis = list(title = "Flight Direction", categoryorder = "array", categoryarray = d$FlightDirection),
      shapes = list(list(
        type = "line", y0 = 0, y1 = 1, yref = "paper",
        x0 = 10, x1 = 10, line = list(color = "red", dash = "dash", width = 2)
      ))
    )
  return(fig)
}

#' Create direction stats table
create_direction_stats_table <- function(direction_analysis) {
  
  stats_table <- direction_analysis$direction_stats %>%
    mutate(
      Status = ifelse(FlightDirection %in% direction_analysis$main_directions,
                     "Main", "Minor"),
      percent_of_total = round(percent_of_total, 1),
      mean_irradiance = round(mean_irradiance, 4),
      cv_irradiance = round(cv_irradiance, 1)
    ) %>%
    select(FlightDirection, n_images, percent_of_total, mean_irradiance, cv_irradiance, Status)
  
  return(stats_table)
}

#' Create flight path map
create_flight_path_map <- function(filtered_data) {
  
  center_lat <- mean(filtered_data$Latitude, na.rm = TRUE)
  center_lon <- mean(filtered_data$Longitude, na.rm = TRUE)
  
  dir_colors <- c(
    "North" = "#3498DB", "South" = "#E74C3C",
    "East" = "#2ECC71", "West" = "#F39C12",
    "NorthEast" = "#9B59B6", "SouthWest" = "#E67E22",
    "NorthWest" = "#1ABC9C", "SouthEast" = "#E91E63"
  )
  
  map <- leaflet(filtered_data) %>%
    setView(lng = center_lon, lat = center_lat, zoom = 17) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")
  
  # De-duplicate: one point per capture location (not per band)
  map_data <- filtered_data %>%
    filter(!duplicated(paste(Latitude, Longitude, TimeStamp)))

  for (dir in unique(map_data$FlightDirection)) {
    dir_data <- map_data %>% filter(FlightDirection == dir)
    color <- ifelse(dir %in% names(dir_colors), dir_colors[dir], "#95A5A6")

    map <- map %>%
      addCircleMarkers(
        data = dir_data,
        lng = ~Longitude, lat = ~Latitude,
        radius = 4, weight = 1, opacity = 0.8,
        color = color, fillColor = color, fillOpacity = 0.6,
        group = dir,
        label = ~paste0(basename(FileName), " (", dir, ")"),
        labelOptions = labelOptions(noHide = FALSE, direction = "top",
                                    style = list("font-size" = "11px"))
      )
  }
  
  map <- map %>%
    addAwesomeMarkers(
      lng = first(filtered_data$Longitude),
      lat = first(filtered_data$Latitude),
      popup = "Start",
      icon = awesomeIcons(icon = "play", markerColor = "green", library = "fa")
    ) %>%
    addAwesomeMarkers(
      lng = last(filtered_data$Longitude),
      lat = last(filtered_data$Latitude),
      popup = "End",
      icon = awesomeIcons(icon = "stop", markerColor = "red", library = "fa")
    ) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
      overlayGroups = unique(filtered_data$FlightDirection),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "bottomleft")
  
  return(map)
}

#' Create band irradiance plot (native plotly)
create_band_irradiance_plot <- function(band_data, band_name) {
  fig <- plotly::plot_ly(band_data, x = ~FlightDirection, y = ~Irradiance,
                         type = "box", color = ~FlightDirection,
                         boxpoints = "suspectedoutliers",
                         jitter = 0.3, pointpos = 0,
                         marker = list(opacity = 0.3, size = 3),
                         showlegend = FALSE) %>%
    plotly::layout(
      title = list(text = paste0(band_name, " Band - Irradiance by Flight Direction")),
      xaxis = list(title = "Flight Direction"),
      yaxis = list(title = "Irradiance")
    )
  return(fig)
}

#' Create band temporal plot (native plotly)
create_band_temporal_plot <- function(band_data, band_name) {
  d <- band_data %>%
    mutate(time_minutes = as.numeric(difftime(TimeStamp, min(TimeStamp), units = "mins")))

  directions <- unique(d$FlightDirection)
  fig <- plotly::plot_ly()
  for (dir in directions) {
    dd <- d[d$FlightDirection == dir, ]
    fig <- fig %>%
      plotly::add_trace(data = dd, x = ~time_minutes, y = ~Irradiance,
                        type = "scatter", mode = "lines",
                        line = list(width = 1.5), opacity = 0.6,
                        name = dir,
                        legendgroup = dir)
  }
  fig <- fig %>%
    plotly::layout(
      title = list(text = paste0(band_name, " Band - Temporal Irradiance Patterns")),
      xaxis = list(title = "Time (minutes from start)"),
      yaxis = list(title = "Irradiance"),
      legend = list(orientation = "h", y = -0.2)
    )
  return(fig)
}

#' Create anomaly plot (native plotly)
create_anomaly_plot <- function(processed_data, band_name) {
  n_flagged <- sum(processed_data$IrrFlag, na.rm = TRUE)
  normal <- processed_data[!processed_data$IrrFlag | is.na(processed_data$IrrFlag), ]
  anomaly <- processed_data[processed_data$IrrFlag & !is.na(processed_data$IrrFlag), ]

  fig <- plotly::plot_ly()
  if (nrow(normal) > 0) {
    fig <- fig %>% plotly::add_trace(
      data = normal, x = ~ImageIndex, y = ~Irradiance,
      type = "scatter", mode = "markers",
      marker = list(size = 5, opacity = 0.7, symbol = "circle"),
      color = ~FlightDirection, legendgroup = ~FlightDirection,
      name = ~FlightDirection, showlegend = TRUE
    )
  }
  if (nrow(anomaly) > 0) {
    fig <- fig %>% plotly::add_trace(
      data = anomaly, x = ~ImageIndex, y = ~Irradiance,
      type = "scatter", mode = "markers",
      marker = list(size = 7, opacity = 0.9, symbol = "triangle-up", color = "red"),
      name = "Anomaly", legendgroup = "Anomaly", showlegend = TRUE
    )
  }
  fig <- fig %>%
    plotly::layout(
      title = list(text = paste0(band_name, " Band - Anomaly Detection<br><sub>Triangles = anomalies (", n_flagged, " flagged)</sub>")),
      xaxis = list(title = "Image Index"),
      yaxis = list(title = "Irradiance"),
      legend = list(orientation = "h", y = -0.2)
    )
  return(fig)
}

#' Create CV heatmap (native plotly)
create_cv_heatmap <- function(filtered_data) {
  cv_data <- filtered_data %>%
    group_by(FlightDirection, BandName) %>%
    summarise(
      cv = sd(Irradiance, na.rm = TRUE) / mean(Irradiance, na.rm = TRUE) * 100,
      n = n(),
      .groups = 'drop'
    ) %>%
    filter(n >= 5)

  # Pivot to matrix form for heatmap
  cv_wide <- cv_data %>%
    tidyr::pivot_wider(id_cols = FlightDirection, names_from = BandName, values_from = cv)
  bands <- setdiff(names(cv_wide), "FlightDirection")
  mat <- as.matrix(cv_wide[, bands])
  rownames(mat) <- cv_wide$FlightDirection

  fig <- plotly::plot_ly(
    x = bands, y = cv_wide$FlightDirection, z = mat,
    type = "heatmap",
    colorscale = list(c(0, "#27AE60"), c(0.4, "#F39C12"), c(1, "#E74C3C")),
    zmin = 0, zmax = 25,
    text = matrix(sprintf("%.1f%%", mat), nrow = nrow(mat)),
    texttemplate = "%{text}", textfont = list(color = "black"),
    hoverinfo = "x+y+z",
    colorbar = list(title = "CV %")
  ) %>%
    plotly::layout(
      title = list(text = "Coefficient of Variation Heatmap<br><sub>Lower values (green) indicate better consistency</sub>"),
      xaxis = list(title = "Spectral Band"),
      yaxis = list(title = "Flight Direction")
    )
  return(fig)
}

#' Create change analysis plot (native plotly)
create_change_analysis_plot <- function(filtered_data) {
  change_data <- filtered_data %>%
    group_by(FlightDirection, BandName) %>%
    arrange(TimeStamp) %>%
    mutate(
      irr_change_percent = c(NA, diff(Irradiance) / head(Irradiance, -1) * 100),
      group_mean = mean(Irradiance, na.rm = TRUE),
      deviation_percent = (Irradiance - group_mean) / group_mean * 100
    ) %>%
    ungroup()

  change_summary <- change_data %>%
    group_by(FlightDirection, BandName) %>%
    summarise(
      cv_percent = sd(Irradiance, na.rm = TRUE) / mean(Irradiance, na.rm = TRUE) * 100,
      mean_abs_change = mean(abs(irr_change_percent), na.rm = TRUE),
      range_percent = (max(Irradiance) - min(Irradiance)) / mean(Irradiance) * 100,
      .groups = 'drop'
    ) %>%
    tidyr::pivot_longer(cols = c(cv_percent, mean_abs_change, range_percent),
                 names_to = "Metric", values_to = "Percent") %>%
    mutate(
      Metric = case_when(
        Metric == "cv_percent" ~ "CV",
        Metric == "mean_abs_change" ~ "Mean Abs Change",
        Metric == "range_percent" ~ "Total Range"
      )
    )

  bands <- unique(change_summary$BandName)
  ncol <- 2
  nrow <- ceiling(length(bands) / ncol)

  fig <- plotly::subplot(
    lapply(seq_along(bands), function(i) {
      bd <- change_summary[change_summary$BandName == bands[i], ]
      plotly::plot_ly(bd, x = ~FlightDirection, y = ~Percent, color = ~Metric,
                      type = "bar", opacity = 0.8,
                      showlegend = (i == 1)) %>%
        plotly::layout(
          barmode = "group",
          annotations = list(list(
            text = bands[i], xref = "paper", yref = "paper",
            x = 0.5, y = 1.05, showarrow = FALSE, font = list(size = 13, face = "bold")
          ))
        )
    }),
    nrows = nrow, shareX = FALSE, shareY = FALSE
  ) %>%
    plotly::layout(
      title = list(text = "Irradiance Variability Metrics by Direction"),
      barmode = "group",
      legend = list(orientation = "h", y = -0.15)
    )
  return(fig)
}

# ==============================================================================
# 8. ENHANCED MAPPING FUNCTIONS
# ==============================================================================

#' Create enhanced map with multiple visualization options
create_enhanced_map <- function(band_data, band_name, metric = "irradiance") {
  
  if (metric == "irradiance") {
    values <- band_data$Irradiance
    title <- "Irradiance"
    palette <- "RdYlGn"
  } else if (metric == "anomalies") {
    if ("IrrFlag" %in% names(band_data)) {
      values <- ifelse(band_data$IrrFlag, 1, 0)
    } else {
      mean_val <- mean(band_data$Irradiance, na.rm = TRUE)
      sd_val <- sd(band_data$Irradiance, na.rm = TRUE)
      values <- ifelse(abs(band_data$Irradiance - mean_val) > 2 * sd_val, 1, 0)
    }
    title <- "Anomalies"
    palette <- "Set1"
  } else if (metric == "deviation") {
    mean_val <- mean(band_data$Irradiance, na.rm = TRUE)
    values <- (band_data$Irradiance - mean_val) / mean_val * 100
    title <- "Deviation %"
    palette <- "RdBu"
  } else if (metric == "quantiles") {
    values <- band_data$Irradiance
    title <- "Irradiance Quantiles"
    palette <- "Spectral"
  }
  
  n_classes <- 5
  if (metric == "anomalies") {
    colors <- c("#27AE60", "#E74C3C")
  } else {
    colors <- rev(brewer.pal(min(n_classes, 11), palette))
  }
  
  center_lat <- mean(band_data$Latitude, na.rm = TRUE)
  center_lon <- mean(band_data$Longitude, na.rm = TRUE)
  
  map <- leaflet(band_data) %>%
    setView(lng = center_lon, lat = center_lat, zoom = 17) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addScaleBar(position = "bottomleft")
  
  if (metric == "anomalies") {
    map <- map %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = 8, color = "black", weight = 1,
        fillColor = ~ifelse(values == 1, "#E74C3C", "#27AE60"),
        fillOpacity = 0.8, group = "Data Points",
        popup = ~paste(
          "<strong>", band_name, " Band</strong><br/>",
          "<strong>Status:</strong>", ifelse(values == 1, "Anomaly", "Normal"), "<br/>",
          "<strong>Irradiance:</strong>", round(Irradiance, 4), "<br/>",
          "<strong>Direction:</strong>", FlightDirection
        )
      )
  } else {
    pal <- colorNumeric(palette = colors, domain = values, na.color = "gray")
    
    map <- map %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = 8, color = "black", weight = 1,
        fillColor = ~pal(values),
        fillOpacity = 0.8, group = "Data Points",
        popup = ~paste(
          "<strong>", band_name, " Band</strong><br/>",
          "<strong>", title, ":</strong>", round(values, 3), "<br/>",
          "<strong>Direction:</strong>", FlightDirection
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal, values = values,
        title = paste(band_name, "<br>", title),
        opacity = 0.9
      )
  }
  
  map <- map %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
      overlayGroups = "Data Points",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addMiniMap(
      tiles = providers$CartoDB.Positron,
      toggleDisplay = TRUE, minimized = FALSE,
      position = "bottomleft", width = 150, height = 150
    )
  
  return(map)
}

# ==============================================================================
# 9. QUALITY METRICS CALCULATION
# ==============================================================================

#' Calculate comprehensive quality metrics
calculate_quality_metrics <- function(filtered_data) {
  
  metrics <- filtered_data %>%
    group_by(FlightDirection, BandName) %>%
    summarise(
      n_images = n(),
      mean_irradiance = mean(Irradiance, na.rm = TRUE),
      sd_irradiance = sd(Irradiance, na.rm = TRUE),
      cv_percent = sd_irradiance / mean_irradiance * 100,
      min_irradiance = min(Irradiance, na.rm = TRUE),
      max_irradiance = max(Irradiance, na.rm = TRUE),
      range_percent = (max_irradiance - min_irradiance) / mean_irradiance * 100,
      .groups = 'drop'
    ) %>%
    arrange(BandName, FlightDirection)
  
  change_metrics <- filtered_data %>%
    group_by(FlightDirection, BandName) %>%
    arrange(TimeStamp) %>%
    mutate(
      irr_change_percent = c(NA, diff(Irradiance) / head(Irradiance, -1) * 100)
    ) %>%
    summarise(
      mean_change_percent = mean(abs(irr_change_percent), na.rm = TRUE),
      max_change_percent = max(abs(irr_change_percent), na.rm = TRUE),
      .groups = 'drop'
    )
  
  metrics <- metrics %>%
    left_join(change_metrics, by = c("FlightDirection", "BandName"))

  return(metrics)
}

# ==============================================================================
# 10. ADVANCED STATISTICAL QA/QC
# ==============================================================================

#' Perform advanced statistical QA checks on drone multispectral data
#' Uses scientifically rigorous methods: MAD-based outlier detection, inter-band
#' ratio stability, spatial coverage analysis, and flight geometry validation.
#' @param filtered_data The filtered data frame with all computed columns
#' @return A named list of QA check results
perform_advanced_qa <- function(filtered_data) {

  # ---------- 1. Altitude (AGL) Stability ----------
  # Inconsistent altitude = variable GSD = unreliable reflectance comparison
  alt_vals <- as.numeric(filtered_data$Altitude)
  alt_vals <- alt_vals[!is.na(alt_vals) & is.finite(alt_vals)]
  if (length(alt_vals) >= 2 && mean(alt_vals) != 0) {
    alt_cv <- sd(alt_vals) / abs(mean(alt_vals)) * 100
    alt_range_ft <- (max(alt_vals) - min(alt_vals)) * 3.28084
    altitude_check <- list(
      pass = alt_cv < 5,
      cv = round(alt_cv, 1),
      range_ft = round(alt_range_ft, 0),
      detail = sprintf("CV: %.1f%%, Range: %.0f ft", alt_cv, alt_range_ft)
    )
  } else {
    altitude_check <- list(pass = TRUE, cv = NA, range_ft = NA,
                           detail = "Insufficient altitude data")
  }

  # ---------- 2. Gimbal Stability (Nadir Check) ----------
  # Camera should be at -90 deg (nadir). Deviations cause geometric distortion.
  if ("GimbalPitch" %in% names(filtered_data)) {
    gimbal_vals <- filtered_data$GimbalPitch[!is.na(filtered_data$GimbalPitch)]
    if (length(gimbal_vals) >= 2) {
      gimbal_dev <- abs(gimbal_vals - (-90))
      pct_off <- sum(gimbal_dev > 5) / length(gimbal_dev) * 100
      mean_dev <- mean(gimbal_dev)
      gimbal_check <- list(
        pass = pct_off < 5,
        pct_off_nadir = round(pct_off, 1),
        mean_deviation = round(mean_dev, 1),
        detail = sprintf("%.1f%% off-nadir (>5 deg), Mean dev: %.1f deg", pct_off, mean_dev)
      )
    } else {
      gimbal_check <- list(pass = TRUE, pct_off_nadir = NA,
                           mean_deviation = NA, detail = "Insufficient gimbal data")
    }
  } else {
    gimbal_check <- list(pass = TRUE, pct_off_nadir = NA,
                         mean_deviation = NA, detail = "No gimbal data available")
  }

  # ---------- 3. Capture Interval Regularity ----------
  # Gaps in capture indicate missed triggers = coverage holes
  unique_times <- sort(unique(filtered_data$TimeStamp))
  if (length(unique_times) > 2) {
    intervals <- as.numeric(diff(unique_times), units = "secs")
    intervals <- intervals[intervals > 0]
    if (length(intervals) > 0) {
      med_int <- median(intervals)
      gap_thresh <- med_int * 3
      n_gaps <- sum(intervals > gap_thresh)
      max_gap <- max(intervals)
      capture_check <- list(
        pass = n_gaps <= 2,
        median_interval = round(med_int, 1),
        n_gaps = n_gaps,
        max_gap = round(max_gap, 1),
        detail = sprintf("Median: %.1fs, %d gaps (>%.0fs), Max gap: %.1fs",
                         med_int, n_gaps, gap_thresh, max_gap)
      )
    } else {
      capture_check <- list(pass = TRUE, median_interval = NA,
                            n_gaps = 0, max_gap = NA,
                            detail = "Could not compute intervals")
    }
  } else {
    capture_check <- list(pass = TRUE, median_interval = NA,
                          n_gaps = 0, max_gap = NA,
                          detail = "Insufficient timestamps")
  }

  # ---------- 4. Inter-band Ratio Consistency ----------
  # For a well-calibrated multispectral sensor with a functioning DLS,
  # the ratios between bands should remain stable across the flight.
  # Drift in ratios indicates DLS failure, sensor degradation, or
  # uncorrected atmospheric changes. This is THE key scientific check
  # for multispectral radiometric quality.
  bands <- unique(filtered_data$BandName)
  if (length(bands) >= 2) {
    capture_data <- filtered_data %>%
      mutate(capture_sec = round(
        as.numeric(difftime(TimeStamp, min(TimeStamp), units = "secs")) / 2
      ) * 2) %>%
      group_by(capture_sec, BandName) %>%
      summarise(mean_irr = mean(Irradiance, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = BandName, values_from = mean_irr)

    ratio_results <- list()
    compute_ratio <- function(b1, b2) {
      if (b1 %in% names(capture_data) && b2 %in% names(capture_data)) {
        r <- capture_data[[b1]] / capture_data[[b2]]
        r <- r[is.finite(r) & !is.na(r)]
        if (length(r) > 2) list(cv = sd(r) / mean(r) * 100,
                                mean = mean(r), n = length(r))
        else NULL
      } else NULL
    }

    r <- compute_ratio("NIR", "Red")
    if (!is.null(r)) ratio_results[["NIR/Red"]] <- r
    r <- compute_ratio("NIR", "Green")
    if (!is.null(r)) ratio_results[["NIR/Green"]] <- r
    r <- compute_ratio("RedEdge", "Red")
    if (!is.null(r)) ratio_results[["RedEdge/Red"]] <- r
    r <- compute_ratio("RedEdge", "Green")
    if (!is.null(r)) ratio_results[["RedEdge/Green"]] <- r

    all_cvs <- sapply(ratio_results, function(x) x$cv)
    band_ratio_check <- list(
      pass = length(all_cvs) == 0 || all(all_cvs < 10),
      ratios = ratio_results,
      detail = if (length(all_cvs) > 0)
        paste(names(ratio_results), sprintf("CV=%.1f%%", all_cvs), collapse = "; ")
      else "No cross-band ratios computed"
    )
  } else {
    band_ratio_check <- list(pass = TRUE, ratios = list(),
                             detail = "Single band — ratio check N/A")
  }

  # ---------- 5. Robust Outlier Detection (Modified Z-Score / MAD) ----------
  # Standard +-2 SD is sensitive to the very outliers it tries to detect.
  # The Modified Z-Score uses Median Absolute Deviation (MAD), which is
  # robust to up to 50% contamination (Iglewicz & Hoaglin, 1993).
  # Threshold: |Modified Z| > 3.5
  outlier_data <- filtered_data %>%
    group_by(BandName, FlightDirection) %>%
    mutate(
      med_irr = median(Irradiance, na.rm = TRUE),
      mad_irr = mad(Irradiance, na.rm = TRUE),
      mod_z = ifelse(mad_irr > 0, (Irradiance - med_irr) / mad_irr, 0),
      is_outlier_mad = abs(mod_z) > 3.5
    ) %>%
    ungroup()

  outlier_stats <- outlier_data %>%
    group_by(BandName) %>%
    summarise(
      n_total = n(),
      n_outliers = sum(is_outlier_mad, na.rm = TRUE),
      pct_outliers = round(n_outliers / n_total * 100, 1),
      .groups = "drop"
    )

  # Capture full details of flagged outlier images for thumbnail review

  flagged_images <- outlier_data %>%
    filter(is_outlier_mad) %>%
    select(any_of(c("FileName", "SourceFile", "BandName", "Irradiance", "mod_z",
                     "Latitude", "Longitude", "FlightDirection",
                     "TimeStamp", "Altitude", "GimbalPitch"))) %>%
    arrange(desc(abs(mod_z)))

  total_pct <- sum(outlier_stats$n_outliers) / sum(outlier_stats$n_total) * 100
  outlier_check <- list(
    pass = total_pct < 5,
    total_pct = round(total_pct, 1),
    per_band = outlier_stats,
    flagged_images = flagged_images,
    detail = sprintf("%.1f%% outliers (MAD-based Modified Z-Score)", total_pct)
  )

  # ---------- 6. Spatial Coverage Uniformity ----------
  # Divides the survey area into a 10x10 grid and checks what fraction
  # of cells contain image data. Low coverage = gaps in orthomosaic.
  lat_range <- range(filtered_data$Latitude, na.rm = TRUE)
  lon_range <- range(filtered_data$Longitude, na.rm = TRUE)

  if (diff(lat_range) > 0 && diff(lon_range) > 0) {
    n_grid <- 10
    lat_breaks <- seq(lat_range[1], lat_range[2], length.out = n_grid + 1)
    lon_breaks <- seq(lon_range[1], lon_range[2], length.out = n_grid + 1)

    grid_data <- filtered_data %>%
      mutate(
        lat_bin = findInterval(Latitude, lat_breaks, all.inside = TRUE),
        lon_bin = findInterval(Longitude, lon_breaks, all.inside = TRUE)
      )

    cells_covered <- grid_data %>% distinct(lat_bin, lon_bin) %>% nrow()
    total_cells <- n_grid * n_grid
    cov_pct <- cells_covered / total_cells * 100

    cell_counts <- grid_data %>% count(lat_bin, lon_bin)
    coverage_check <- list(
      pass = cov_pct > 60,
      coverage_pct = round(cov_pct, 0),
      cells_covered = cells_covered,
      total_cells = total_cells,
      min_density = min(cell_counts$n),
      mean_density = round(mean(cell_counts$n), 0),
      detail = sprintf("%.0f%% grid coverage (%d/%d cells)", cov_pct, cells_covered, total_cells)
    )
  } else {
    coverage_check <- list(pass = TRUE, coverage_pct = NA,
                           cells_covered = NA, total_cells = NA,
                           detail = "Cannot compute (single point)")
  }

  # ---------- Overall Advanced QA Verdict ----------
  all_pass <- altitude_check$pass & gimbal_check$pass & capture_check$pass &
              band_ratio_check$pass & outlier_check$pass & coverage_check$pass

  issues <- c()
  if (!altitude_check$pass) issues <- c(issues, paste("Altitude:", altitude_check$detail))
  if (!gimbal_check$pass) issues <- c(issues, paste("Gimbal:", gimbal_check$detail))
  if (!capture_check$pass) issues <- c(issues, paste("Capture:", capture_check$detail))
  if (!band_ratio_check$pass) issues <- c(issues, paste("Band ratios:", band_ratio_check$detail))
  if (!outlier_check$pass) issues <- c(issues, paste("Outliers:", outlier_check$detail))
  if (!coverage_check$pass) issues <- c(issues, paste("Coverage:", coverage_check$detail))

  list(
    altitude         = altitude_check,
    gimbal           = gimbal_check,
    capture_interval = capture_check,
    band_ratios      = band_ratio_check,
    outliers         = outlier_check,
    coverage         = coverage_check,
    overall_pass     = all_pass,
    issues           = issues
  )
}

# ==============================================================================
# 11. REPORT DATA COMPILATION
# ==============================================================================

#' Compile all analysis results into a report-ready parameter list
#' @param filtered_data The filtered data frame with all computed columns
#' @param quality_results Output from assess_overall_quality()
#' @param stability_analysis Output from analyze_sun_sensor_stability()
#' @param band_results Output from analyze_all_bands_enhanced()
#' @param operator_info Named list with operator_name, ranch, blocks, wind_speed, notes
#' @param advanced_qa Output from perform_advanced_qa() (optional)
#' @param lighting_analysis Output from analyze_lighting_consistency() (optional)
#' @return A named list of all report parameters
compile_report_data <- function(filtered_data, quality_results, stability_analysis,
                                band_results, operator_info, advanced_qa = NULL,
                                folder_path = NULL, lighting_analysis = NULL) {

  # Flight date/time
  flight_start <- min(filtered_data$TimeStamp, na.rm = TRUE)
  flight_end   <- max(filtered_data$TimeStamp, na.rm = TRUE)
  flight_duration_min <- round(as.numeric(difftime(flight_end, flight_start, units = "mins")), 1)

  # Bands

  bands_used <- sort(unique(filtered_data$BandName))
  n_bands <- length(bands_used)
  total_images <- nrow(filtered_data)
  unique_captures <- if (n_bands > 0) round(total_images / n_bands) else total_images

  # Camera rig
  camera_rig <- na.omit(unique(filtered_data$CameraRig))
  camera_rig_str <- if (length(camera_rig) > 0) paste(camera_rig, collapse = ", ") else "N/A"

  # AGL (meters to feet)
  mean_agl_m <- mean(as.numeric(filtered_data$Altitude), na.rm = TRUE)
  mean_agl_ft <- round(mean_agl_m * 3.28084, 0)
  agl_str <- if (is.finite(mean_agl_ft)) paste0(mean_agl_ft, " ft") else "N/A"

  # Speed (mph)
  if ("GroundSpeed_mph" %in% names(filtered_data)) {
    mean_speed <- mean(filtered_data$GroundSpeed_mph, na.rm = TRUE)
    speed_str <- if (is.finite(mean_speed) && mean_speed > 0) sprintf("%.1f mph", mean_speed) else "N/A"
  } else {
    speed_str <- "N/A"
  }

  # Per-band exposure summary
  exposure_summary <- filtered_data %>%
    dplyr::group_by(BandName) %>%
    dplyr::summarise(
      Shutter = paste0("1/", round(1 / median(exposure_time, na.rm = TRUE))),
      ISO = as.character(round(median(ISO, na.rm = TRUE))),
      FStop = as.character(round(median(FNumber, na.rm = TRUE), 1)),
      .groups = "drop"
    )

  # Shutter count
  shutter_count_vals <- na.omit(filtered_data$ShutterCount)
  shutter_count_str <- if (length(shutter_count_vals) > 0) {
    sprintf("%d - %d", min(shutter_count_vals), max(shutter_count_vals))
  } else {
    "N/A"
  }

  # Source folder
  if (!is.null(folder_path) && nchar(folder_path) > 0) {
    source_str <- folder_path
  } else {
    source_str <- "N/A"
  }

  # Per-band stability
  band_stability <- stability_analysis$band_pass_fail %>%
    dplyr::select(BandName, stability_score, status) %>%
    dplyr::mutate(stability_score = round(stability_score, 0))

  # Quality pass/fail (GPS is informational — georeferenced downstream)
  lighting_detail <- if (!is.null(lighting_analysis) && isTRUE(lighting_analysis$available)) {
    worst <- lighting_analysis$per_band[which.min(lighting_analysis$per_band$score), ]
    sprintf("%s (detrended CV %.1f%%, %d cloud events)",
            lighting_analysis$overall_class, worst$detrended_cv, sum(lighting_analysis$per_band$n_events))
  } else {
    sprintf("%.1f%% deviation", quality_results$lighting_consistency$high_deviation_rate)
  }

  qa_results <- list(
    lighting  = list(pass = quality_results$lighting_consistency$pass,
                     detail = lighting_detail),
    exposure  = list(pass = quality_results$exposure_consistency$pass,
                     detail = sprintf("%.1f%% changes", quality_results$exposure_consistency$change_rate)),
    gps       = list(pass = quality_results$gps_consistency$pass,
                     detail = sprintf("%.1f%% jumps (informational — georeferenced downstream)",
                                      quality_results$gps_consistency$jump_rate)),
    sunsensor = list(pass = stability_analysis$overall_pass,
                     detail = if (length(stability_analysis$issues) > 0)
                       paste(stability_analysis$issues, collapse = "; ") else "All bands stable")
  )

  # Solar elevation across the flight (context for lighting drift)
  solar_str <- if (!is.null(lighting_analysis) && isTRUE(lighting_analysis$available) &&
                   isTRUE(lighting_analysis$solar$ok)) {
    sprintf("%.0f deg to %.0f deg", lighting_analysis$solar$elev_start,
            lighting_analysis$solar$elev_end)
  } else {
    "N/A"
  }

  rd <- list(
    # Operator-entered fields
    operator_name = operator_info$operator_name,
    ranch         = operator_info$ranch,
    blocks        = operator_info$blocks,
    wind_speed    = operator_info$wind_speed,
    notes         = operator_info$notes,
    # Computed mission info (camera clock is local time)
    flight_date      = format(flight_start, "%Y-%m-%d"),
    flight_time      = paste(format(flight_start, "%H:%M"), "-", format(flight_end, "%H:%M"), "local"),
    flight_duration  = paste0(flight_duration_min, " min"),
    total_images     = total_images,
    unique_captures  = unique_captures,
    bands_used       = paste(bands_used, collapse = ", "),
    n_bands          = n_bands,
    camera_rig       = camera_rig_str,
    agl              = agl_str,
    speed            = speed_str,
    exposure_summary = exposure_summary,
    shutter_count    = shutter_count_str,
    source_files     = source_str,
    solar_elevation  = solar_str,
    # Quality assessment
    qa_results       = qa_results,
    overall_pass     = quality_results$overall_pass,
    issues           = quality_results$issues,
    # Band stability
    band_stability   = band_stability,
    # Generation timestamp
    report_generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # Per-band lighting consistency table for the report
  if (!is.null(lighting_analysis) && isTRUE(lighting_analysis$available)) {
    rd$lighting_summary <- lighting_analysis$per_band %>%
      dplyr::transmute(
        BandName,
        Classification = classification,
        DetrendedCV = sprintf("%.1f%%", detrended_cv),
        CloudEvents = n_events,
        Score = score
      )
  }

  # Include advanced QA results if available
  if (!is.null(advanced_qa)) {
    rd$advanced_qa <- advanced_qa
    # Combine overall verdict with advanced checks
    rd$overall_pass <- rd$overall_pass & advanced_qa$overall_pass
    rd$issues <- c(rd$issues, advanced_qa$issues)
  }

  rd
}

# ==============================================================================
# 12. THUMBNAIL EXTRACTION FOR OUTLIER REVIEW
# ==============================================================================

#' Validate ExifTool setup and configure perl path
#' @param perl_path Path to perl executable, or "" for system default
#' @return list(ok = TRUE/FALSE, message = "...")
validate_exiftool_setup <- function(perl_path = "") {
  # Set perl path if provided
  if (!is.null(perl_path) && nchar(perl_path) > 0) {
    if (!file.exists(perl_path)) {
      return(list(
        ok = FALSE,
        message = paste0("Configured perl path does not exist: ", perl_path,
                         "\nUpdate perl_path in R/config.R or set to \"\" to use system default.")
      ))
    }
    options(exifr.perlpath = perl_path)
  }

  # Try to configure exifr
  result <- tryCatch({
    exifr::configure_exiftool(quiet = TRUE)
    list(ok = TRUE, message = "ExifTool configured successfully.")
  }, error = function(e) {
    # Retry with auto-install attempt
    retry <- tryCatch({
      exifr::configure_exiftool(quiet = FALSE)
      list(ok = TRUE, message = "ExifTool configured after auto-install.")
    }, error = function(e2) {
      list(
        ok = FALSE,
        message = paste0("ExifTool configuration failed: ", e2$message,
                         "\nInstall ExifTool and ensure perl is available, ",
                         "or set perl_path in R/config.R.")
      )
    })
    retry
  })

  result
}

# ==============================================================================
# 13. BATCH PROCESSING
# ==============================================================================

#' Scan a parent folder for subfolders containing drone image files
#' @param parent_folder Path to parent directory
#' @return Data frame with columns: folder_name, folder_path, image_count, file_types
scan_flight_folders <- function(parent_folder) {
  if (!dir.exists(parent_folder)) return(data.frame())

  subdirs <- list.dirs(parent_folder, full.names = TRUE, recursive = FALSE)
  # Also include the parent folder itself if it has images
  all_dirs <- c(parent_folder, subdirs)

  results <- lapply(all_dirs, function(d) {
    image_exts <- c("tif", "tiff", "dng", "jpg", "jpeg")
    files <- list.files(d, pattern = paste0("\\.(", paste(image_exts, collapse = "|"), ")$"),
                        ignore.case = TRUE, recursive = FALSE)
    if (length(files) > 0) {
      exts <- unique(tolower(tools::file_ext(files)))
      data.frame(
        folder_name = basename(d),
        folder_path = d,
        image_count = length(files),
        file_types = paste(toupper(exts), collapse = ", "),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  result_df <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(result_df) || nrow(result_df) == 0) {
    return(data.frame(folder_name = character(), folder_path = character(),
                      image_count = integer(), file_types = character(),
                      stringsAsFactors = FALSE))
  }
  result_df
}

#' Process a single flight folder through the full QA pipeline
#' @param folder_path Path to folder containing drone images
#' @param perl_path Optional perl path for exiftool
#' @return Named list with success flag, all analysis results, and summary row
process_single_flight <- function(folder_path, perl_path = NULL) {
  tryCatch({
    # Configure perl if provided
    if (!is.null(perl_path) && nchar(perl_path) > 0) {
      options(exifr.perlpath = perl_path)
    }

    # 1. Read EXIF
    exif_data <- exifr::read_exif(folder_path, recursive = TRUE, tags = REQUIRED_EXIF_TAGS)
    if (nrow(exif_data) == 0) stop("No images found")

    # 2. Process
    processed_data <- process_exif_data_enhanced(exif_data)
    exposure_validation <- validate_exposure_settings(processed_data)

    # 3. Filter
    filter_result <- filter_quality_data_enhanced(processed_data)
    filtered_data <- filter_result$data
    if (nrow(filtered_data) == 0) stop("No valid images after filtering")

    # 4. Directions
    direction_analysis <- analyze_flight_directions_enhanced(filtered_data)
    filtered_data <- direction_analysis$data

    # 5. Bands
    band_results <- analyze_all_bands_enhanced(filtered_data)

    # 6. Sun sensor
    stability_analysis <- analyze_sun_sensor_stability(filtered_data)

    # 7. Lighting consistency
    lighting_analysis <- analyze_lighting_consistency(filtered_data)

    # 8. Quality
    quality_results <- assess_overall_quality(filtered_data, band_results,
                                              stability_analysis, lighting_analysis)

    # 9. Advanced QA
    advanced_qa <- perform_advanced_qa(filtered_data)

    # Combined verdict
    overall_pass <- quality_results$overall_pass & advanced_qa$overall_pass
    all_issues <- c(quality_results$issues, advanced_qa$issues)

    # Flight time info
    flight_start <- min(filtered_data$TimeStamp, na.rm = TRUE)
    flight_end <- max(filtered_data$TimeStamp, na.rm = TRUE)
    duration_min <- round(as.numeric(difftime(flight_end, flight_start, units = "mins")), 1)

    # Summary row for batch table
    summary_row <- data.frame(
      Folder = basename(folder_path),
      Date = format(flight_start, "%Y-%m-%d"),
      Images = nrow(filtered_data),
      Bands = length(unique(filtered_data$BandName)),
      Duration = paste0(duration_min, " min"),
      Lighting = if (isTRUE(lighting_analysis$available)) lighting_analysis$overall_class else "N/A",
      Pass = overall_pass,
      Outlier_Pct = advanced_qa$outliers$total_pct,
      Issues = if (length(all_issues) > 0) paste(all_issues, collapse = "; ") else "None",
      stringsAsFactors = FALSE
    )

    list(
      success = TRUE,
      error = NULL,
      folder_path = folder_path,
      filtered_data = filtered_data,
      quality_results = quality_results,
      stability_analysis = stability_analysis,
      band_results = band_results,
      advanced_qa = advanced_qa,
      lighting_analysis = lighting_analysis,
      exposure_validation = exposure_validation,
      summary = summary_row
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      folder_path = folder_path,
      summary = data.frame(
        Folder = basename(folder_path),
        Date = "—",
        Images = 0,
        Bands = 0,
        Duration = "—",
        Lighting = "—",
        Pass = FALSE,
        Outlier_Pct = NA_real_,
        Issues = paste("ERROR:", e$message),
        stringsAsFactors = FALSE
      )
    )
  })
}
