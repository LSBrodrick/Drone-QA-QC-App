# ==============================================================================
# ENHANCED DRONE QA/QC ANALYSIS FUNCTIONS
# Based on the Mavic 3 Multispectral Flight QA/QC script
# ==============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(exifr)
  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(zoo)
  library(tidyr)
  library(viridis)
  library(RColorBrewer)
  library(circular)
  library(scales)
  library(purrr)
})

# ==============================================================================
# 1. ENHANCED DATA PROCESSING
# ==============================================================================

#' Process EXIF data with enhanced field extraction
process_exif_data_enhanced <- function(exif_data) {
  
  # Rename columns for consistency
  processed_data <- exif_data %>%
    rename(
      Irradiance = Irradiance,
      sun_sensor_status = LS_status,
      FlightYaw = FlightYawDegree,
      FlightPitch = FlightPitchDegree,
      FlightRoll = FlightRollDegree,
      GimbalPitch = GimbalPitchDegree,
      exposure_time = ExposureTime,
      ISO = ISO,
      Latitude = GPSLatitude,
      Longitude = GPSLongitude,
      TimeStamp = DateTimeOriginal,
      FNumber = FNumber
    ) %>%
    mutate(
      # Convert timestamp
      TimeStamp = as.POSIXct(TimeStamp, format = "%Y:%m:%d %H:%M:%S", tz = "UTC"),
      
      # Add sequential index
      ImageIndex = row_number(),
      
      # Convert all angles to numeric
      FlightYaw = as.numeric(FlightYaw),
      FlightPitch = as.numeric(FlightPitch),
      FlightRoll = as.numeric(FlightRoll),
      GimbalPitch = as.numeric(GimbalPitch),
      
      # Convert camera settings to numeric
      exposure_time = as.numeric(exposure_time),
      ISO = as.numeric(ISO),
      FNumber = as.numeric(FNumber),
      Irradiance = as.numeric(Irradiance),
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude)
    ) %>%
    arrange(TimeStamp)
  
  return(processed_data)
}

#' Enhanced quality data filtering
filter_quality_data_enhanced <- function(processed_data) {
  
  # Apply minimal filtering - only remove images with missing critical data
  filtered_data <- processed_data %>%
    filter(
      !is.na(Irradiance),
      !is.na(Latitude),
      !is.na(Longitude),
      !is.na(FlightYaw)
    )
  
  filtering_stats <- list(
    original_count = nrow(processed_data),
    filtered_count = nrow(filtered_data),
    removed_count = nrow(processed_data) - nrow(filtered_data),
    retention_rate = nrow(filtered_data) / nrow(processed_data) * 100
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
  
  # Expected settings
  expected_settings <- list(
    fstop = 5.6,
    rgb_shutter = 1/2000,
    green_shutter = 1/2500,
    red_edge_shutter = 1/1000,
    nir_shutter = 1/2500
  )
  
  # Check settings by band
  validation_results <- data %>%
    group_by(BandName) %>%
    summarise(
      mean_fstop = mean(FNumber, na.rm = TRUE),
      mean_shutter = mean(exposure_time, na.rm = TRUE),
      sd_fstop = sd(FNumber, na.rm = TRUE),
      sd_shutter = sd(exposure_time, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Check for deviations
  issues <- c()
  all_valid <- TRUE
  
  # F-stop validation
  if (any(abs(validation_results$mean_fstop - expected_settings$fstop) > 0.1)) {
    issues <- c(issues, "F-stop deviations detected")
    all_valid <- FALSE
  }
  
  # Band-specific shutter speed validation
  for (i in 1:nrow(validation_results)) {
    band <- validation_results$BandName[i]
    actual_shutter <- validation_results$mean_shutter[i]
    
    expected_shutter <- switch(band,
      "RGB" = expected_settings$rgb_shutter,
      "Green" = expected_settings$green_shutter,
      "Red Edge" = expected_settings$red_edge_shutter,
      "NIR" = expected_settings$nir_shutter,
      1/2000  # Default
    )
    
    if (abs(actual_shutter - expected_shutter) > expected_shutter * 0.1) {
      issues <- c(issues, sprintf("%s band shutter speed mismatch", band))
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
  # Normalize yaw to 0-360 range
  yaw <- ((yaw %% 360) + 360) %% 360
  
  # Determine specific direction based on 45° sectors
  specific_direction <- case_when(
    (yaw >= 337.5 | yaw < 22.5) ~ "North",
    (yaw >= 22.5 & yaw < 67.5) ~ "NorthEast",
    (yaw >= 67.5 & yaw < 112.5) ~ "East",
    (yaw >= 112.5 & yaw < 157.5) ~ "SouthEast",
    (yaw >= 157.5 & yaw < 202.5) ~ "South",
    (yaw >= 202.5 & yaw < 247.5) ~ "SouthWest",
    (yaw >= 247.5 & yaw < 292.5) ~ "West",
    (yaw >= 292.5 & yaw < 337.5) ~ "NorthWest"
  )
  
  # Group directions for isolated comparison
  direction_group <- paste0(specific_direction, "_Group")
  
  # Determine mission pattern
  mission_pattern <- case_when(
    specific_direction %in% c("North", "South") ~ "North-South_Mission",
    specific_direction %in% c("East", "West") ~ "East-West_Mission",
    specific_direction %in% c("NorthEast", "SouthWest") ~ "NE-SW_Mission",
    specific_direction %in% c("NorthWest", "SouthEast") ~ "NW-SE_Mission"
  )
  
  return(list(
    specific_direction = specific_direction,
    direction_group = direction_group,
    mission_pattern = mission_pattern
  ))
}

#' Enhanced flight direction analysis
analyze_flight_directions_enhanced <- function(filtered_data) {
  
  # Apply direction categorization
  direction_results <- categorize_flight_direction_enhanced(filtered_data$FlightYaw)
  
  # Add direction information to dataset
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
  
  # Calculate direction statistics
  direction_stats <- data_with_directions %>%
    group_by(DirectionGroup) %>%
    summarise(
      n_images = n(),
      percent_of_total = n() / nrow(data_with_directions) * 100,
      n_bands = n_distinct(BandName),
      mean_irradiance = mean(Irradiance, na.rm = TRUE),
      time_span_mins = as.numeric(difftime(max(TimeStamp), min(TimeStamp), units = "mins")),
      .groups = 'drop'
    ) %>%
    arrange(desc(n_images))
  
  # Identify main flight directions
  MIN_PERCENT_FOR_MAIN <- 15
  MIN_IMAGES_FOR_MAIN <- 30
  MAX_MAIN_DIRECTIONS <- 4
  
  main_directions <- direction_stats %>%
    filter(
      percent_of_total >= MIN_PERCENT_FOR_MAIN,
      n_images >= MIN_IMAGES_FOR_MAIN
    ) %>%
    slice_head(n = MAX_MAIN_DIRECTIONS) %>%
    pull(DirectionGroup)
  
  # If too few main directions, take top 2
  if (length(main_directions) < 2) {
    main_directions <- direction_stats %>%
      slice_head(n = 2) %>%
      pull(DirectionGroup)
  }
  
  # Calculate coverage
  main_direction_coverage <- direction_stats %>%
    filter(DirectionGroup %in% main_directions) %>%
    summarise(total_percent = sum(percent_of_total)) %>%
    pull(total_percent)
  
  # Filter to main directions
  main_data <- data_with_directions %>%
    filter(DirectionGroup %in% main_directions)
  
  return(list(
    data = main_data,
    direction_stats = direction_stats,
    main_directions = main_directions,
    excluded_directions = setdiff(direction_stats$DirectionGroup, main_directions),
    main_coverage = main_direction_coverage
  ))
}

# ==============================================================================
# 4. ENHANCED BAND ANALYSIS
# ==============================================================================

#' Analyze single band with enhanced metrics
analyze_single_band_enhanced <- function(band_data, band_name) {
  
  # Basic statistics
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
  
  # Direction-isolated statistics
  direction_stats <- band_data %>%
    group_by(DirectionGroup) %>%
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
  
  # Rolling statistics for anomaly detection
  band_data_with_rolling <- band_data %>%
    group_by(DirectionGroup) %>%
    arrange(TimeStamp) %>%
    mutate(
      window_size = pmax(3, pmin(20, floor(n() * 0.1))),
      
      RollingAvg = if(n() >= 3) {
        rollmean(Irradiance, k = min(window_size[1], n()), fill = NA, align = "center")
      } else {
        rep(mean(Irradiance, na.rm = TRUE), n())
      },
      
      RollingSD = if(n() >= 3) {
        rollapply(Irradiance, width = min(window_size[1], n()), FUN = sd, fill = NA, align = "center")
      } else {
        rep(sd(Irradiance, na.rm = TRUE), n())
      },
      
      IrrDiff = abs(Irradiance - RollingAvg),
      IrrCV = ifelse(!is.na(RollingSD) & RollingAvg > 0, RollingSD / RollingAvg * 100, NA),
      IrrFlag = !is.na(RollingAvg) & IrrDiff > 0.1 * abs(RollingAvg)
    ) %>%
    ungroup()
  
  # Anomaly statistics
  anomaly_stats <- band_data_with_rolling %>%
    group_by(DirectionGroup) %>%
    summarise(
      n_total = n(),
      n_flagged = sum(IrrFlag, na.rm = TRUE),
      percent_flagged = n_flagged / n_total * 100,
      .groups = 'drop'
    ) %>%
    filter(n_total >= 10)
  
  # Temporal trend analysis
  trend_model <- lm(Irradiance ~ as.numeric(TimeStamp), data = band_data)
  trend_summary <- summary(trend_model)
  
  return(list(
    band_name = band_name,
    basic_stats = basic_stats,
    direction_stats = direction_stats,
    processed_data = band_data_with_rolling,
    anomaly_stats = anomaly_stats,
    trend_model = trend_model,
    trend_summary = trend_summary
  ))
}

#' Analyze all bands
analyze_all_bands_enhanced <- function(filtered_data) {
  bands <- unique(filtered_data$BandName)
  band_results <- list()
  
  for (band in bands) {
    band_data <- filtered_data %>% filter(BandName == band)
    if (nrow(band_data) > 0) {
      band_results[[band]] <- analyze_single_band_enhanced(band_data, band)
    }
  }
  
  return(band_results)
}

# ==============================================================================
# 5. QUALITY ASSESSMENT (WITHOUT STABILITY ANALYSIS)
# ==============================================================================

#' Assess overall quality without stability metrics
assess_overall_quality <- function(filtered_data, band_results) {
  
  # Lighting consistency
  irr_flags <- filtered_data %>%
    group_by(BandName, DirectionGroup) %>%
    mutate(
      mean_irr = mean(Irradiance, na.rm = TRUE),
      deviation = abs(Irradiance - mean_irr) / mean_irr * 100
    ) %>%
    ungroup() %>%
    summarise(
      high_deviation_rate = sum(deviation > 10, na.rm = TRUE) / n() * 100
    )
  
  lighting_consistency <- list(
    pass = irr_flags$high_deviation_rate < 20,
    high_deviation_rate = irr_flags$high_deviation_rate
  )
  
  # Exposure consistency
  exp_changes <- filtered_data %>%
    arrange(TimeStamp) %>%
    mutate(
      exp_change = abs(c(0, diff(exposure_time))),
      iso_change = abs(c(0, diff(ISO)))
    ) %>%
    summarise(
      exp_change_rate = sum(exp_change > 0.001 | iso_change > 100) / n() * 100
    )
  
  exposure_consistency <- list(
    pass = exp_changes$exp_change_rate < 10,
    change_rate = exp_changes$exp_change_rate
  )
  
  # GPS consistency
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
    jump_rate = gps_jumps$jump_rate
  )
  
  # Overall assessment
  overall_pass <- lighting_consistency$pass & 
                  exposure_consistency$pass & 
                  gps_consistency$pass
  
  issues <- c()
  if (!lighting_consistency$pass) issues <- c(issues, "Lighting instability")
  if (!exposure_consistency$pass) issues <- c(issues, "Exposure variations")
  if (!gps_consistency$pass) issues <- c(issues, "GPS irregularities")
  
  return(list(
    lighting_consistency = lighting_consistency,
    exposure_consistency = exposure_consistency,
    gps_consistency = gps_consistency,
    overall_pass = overall_pass,
    issues = issues
  ))
}

# ==============================================================================
# 6. VISUALIZATION FUNCTIONS
# ==============================================================================

#' Create direction distribution plot
create_direction_distribution_plot <- function(direction_analysis) {
  
  ggplot(direction_analysis$direction_stats,
         aes(x = reorder(DirectionGroup, n_images), 
             y = percent_of_total,
             fill = DirectionGroup %in% direction_analysis$main_directions)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "red", size = 1) +
    geom_text(aes(label = sprintf("%.1f%%\n(%d)", percent_of_total, n_images)), 
              hjust = -0.1, size = 3.5) +
    scale_fill_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60"),
                     guide = FALSE) +
    coord_flip() +
    labs(x = "Direction Group", y = "Percentage of Images",
         title = "Flight Direction Distribution") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 11)
    )
}

#' Create direction stats table
create_direction_stats_table <- function(direction_analysis) {
  
  stats_table <- direction_analysis$direction_stats %>%
    mutate(
      Status = ifelse(DirectionGroup %in% direction_analysis$main_directions,
                     "Included", "Excluded"),
      percent_of_total = round(percent_of_total, 1),
      mean_irradiance = round(mean_irradiance, 4)
    ) %>%
    select(DirectionGroup, n_images, percent_of_total, mean_irradiance, Status)
  
  datatable(
    stats_table,
    options = list(
      pageLength = 8,
      dom = 't',
      ordering = FALSE
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'Status',
      backgroundColor = styleEqual(c('Included', 'Excluded'), 
                                 c('#D5F4E6', '#FADBD8'))
    )
}

#' Create flight path map
create_flight_path_map <- function(filtered_data) {
  
  # Calculate center
  center_lat <- mean(filtered_data$Latitude, na.rm = TRUE)
  center_lon <- mean(filtered_data$Longitude, na.rm = TRUE)
  
  # Create map
  map <- leaflet(filtered_data) %>%
    setView(lng = center_lon, lat = center_lat, zoom = 17) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")
  
  # Add flight paths by direction
  for(dir in unique(filtered_data$DirectionGroup)) {
    dir_data <- filtered_data %>% 
      filter(DirectionGroup == dir) %>%
      arrange(TimeStamp)
    
    if(nrow(dir_data) > 1) {
      map <- map %>%
        addPolylines(
          data = dir_data,
          lng = ~Longitude, 
          lat = ~Latitude,
          color = ~case_when(
            grepl("North", dir) ~ "#3498DB",
            grepl("South", dir) ~ "#E74C3C",
            grepl("East", dir) ~ "#2ECC71",
            grepl("West", dir) ~ "#F39C12",
            TRUE ~ "#95A5A6"
          ),
          weight = 2,
          opacity = 0.8,
          group = dir,
          popup = ~paste("Direction:", dir)
        )
    }
  }
  
  # Add start/end markers
  map <- map %>%
    addMarkers(
      lng = first(filtered_data$Longitude),
      lat = first(filtered_data$Latitude),
      popup = "Start",
      icon = makeIcon(iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                     iconWidth = 25, iconHeight = 41)
    ) %>%
    addMarkers(
      lng = last(filtered_data$Longitude),
      lat = last(filtered_data$Latitude),
      popup = "End",
      icon = makeIcon(iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                     iconWidth = 25, iconHeight = 41)
    )
  
  # Add controls
  map <- map %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
      overlayGroups = unique(filtered_data$DirectionGroup),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "bottomleft")
  
  return(map)
}

#' Create band irradiance plot
create_band_irradiance_plot <- function(band_data, band_name) {
  
  ggplot(band_data, aes(x = FlightDirection, y = Irradiance)) +
    geom_boxplot(aes(fill = FlightDirection), alpha = 0.7, outlier.alpha = 0.3) +
    geom_jitter(alpha = 0.2, width = 0.2, size = 0.8) +
    scale_fill_viridis_d() +
    labs(
      title = paste(band_name, "Band - Irradiance by Direction"),
      subtitle = "Each direction analyzed independently",
      x = "Flight Direction",
      y = "Irradiance"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "none"
    )
}

#' Create band temporal plot
create_band_temporal_plot <- function(band_data, band_name) {
  
  band_data %>%
    mutate(time_minutes = as.numeric(difftime(TimeStamp, min(TimeStamp), units = "mins"))) %>%
   ggplot(aes(x = time_minutes, y = Irradiance)) +
    geom_line(aes(color = DirectionGroup, group = flight_line), alpha = 0.6) +
    geom_smooth(aes(color = DirectionGroup), method = "loess", se = TRUE, alpha = 0.2) +
    scale_color_viridis_d() +
    labs(
      title = paste(band_name, "Band - Temporal Patterns"),
      subtitle = "Trend lines show average behavior within each direction",
      x = "Time (minutes)",
      y = "Irradiance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "bottom"
    )
}

#' Create anomaly plot
create_anomaly_plot <- function(processed_data, band_name) {
  
  processed_data %>%
    ggplot(aes(x = ImageIndex, y = Irradiance)) +
    geom_point(aes(color = DirectionGroup, shape = IrrFlag), 
               size = 2, alpha = 0.7) +
    scale_shape_manual(values = c(16, 17), 
                       labels = c("Normal", "Anomaly"),
                       name = "Status") +
    scale_color_viridis_d(name = "Direction") +
    labs(
      title = paste(band_name, "Band - Anomaly Detection"),
      subtitle = sprintf("Triangles indicate anomalies (%d flagged)", 
                         sum(processed_data$IrrFlag, na.rm = TRUE)),
      x = "Image Index",
      y = "Irradiance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "bottom"
    )
}

#' Create CV heatmap
create_cv_heatmap <- function(filtered_data) {
  
  cv_data <- filtered_data %>%
    group_by(DirectionGroup, BandName) %>%
    summarise(
      cv = sd(Irradiance, na.rm = TRUE) / mean(Irradiance, na.rm = TRUE) * 100,
      n = n(),
      .groups = 'drop'
    ) %>%
    filter(n >= 5)
  
  ggplot(cv_data, aes(x = BandName, y = DirectionGroup, fill = cv)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", cv)), color = "black", size = 4) +
    scale_fill_gradient2(
      low = "#27AE60", mid = "#F39C12", high = "#E74C3C",
      midpoint = 10, limits = c(0, 20),
      name = "CV %"
    ) +
    labs(
      title = "Coefficient of Variation Heatmap",
      subtitle = "Lower values (green) indicate better consistency",
      x = "Spectral Band",
      y = "Direction Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

#' Create change analysis plot
create_change_analysis_plot <- function(filtered_data) {
  
  # Calculate irradiance changes
  change_data <- filtered_data %>%
    group_by(DirectionGroup, BandName) %>%
    arrange(TimeStamp) %>%
    mutate(
      irr_change_percent = c(NA, diff(Irradiance) / head(Irradiance, -1) * 100),
      group_mean = mean(Irradiance, na.rm = TRUE),
      deviation_percent = (Irradiance - group_mean) / group_mean * 100
    ) %>%
    ungroup()
  
  # Create summary for plot
  change_summary <- change_data %>%
    group_by(DirectionGroup, BandName) %>%
    summarise(
      cv_percent = sd(Irradiance, na.rm = TRUE) / mean(Irradiance, na.rm = TRUE) * 100,
      mean_abs_change = mean(abs(irr_change_percent), na.rm = TRUE),
      range_percent = (max(Irradiance) - min(Irradiance)) / mean(Irradiance) * 100,
      .groups = 'drop'
    ) %>%
    pivot_longer(cols = c(cv_percent, mean_abs_change, range_percent),
                 names_to = "Metric", values_to = "Percent") %>%
    mutate(
      Metric = case_when(
        Metric == "cv_percent" ~ "Coefficient of Variation",
        Metric == "mean_abs_change" ~ "Mean Absolute Change",
        Metric == "range_percent" ~ "Total Range"
      )
    )
  
  ggplot(change_summary, aes(x = DirectionGroup, y = Percent, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    facet_wrap(~BandName, scales = "free_y", ncol = 2) +
    scale_fill_brewer(palette = "Set2", name = "Metric Type") +
    labs(
      title = "Irradiance Variability Metrics",
      subtitle = "Comparing different measures of irradiance change",
      x = "Direction Group",
      y = "Percent (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "bottom"
    )
}

# ==============================================================================
# 7. ENHANCED MAPPING FUNCTIONS
# ==============================================================================

#' Create enhanced map with multiple visualization options
create_enhanced_map <- function(band_data, band_name, metric = "irradiance") {
  
  # Prepare data based on metric
  if (metric == "irradiance") {
    values <- band_data$Irradiance
    title <- "Irradiance"
    palette <- "RdYlGn"
  } else if (metric == "anomalies") {
    # Use anomaly flags if available
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
    palette = "RdBu"
  } else if (metric == "quantiles") {
    values <- band_data$Irradiance
    title <- "Irradiance Quantiles"
    palette = "Spectral"
  }
  
  # Create quantile breaks
  n_classes <- 5
  if (metric == "anomalies") {
    breaks <- c(0, 1)
    colors <- c("#27AE60", "#E74C3C")
  } else if (metric == "quantiles") {
    breaks <- quantile(values, probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE)
    breaks <- unique(breaks)
    if (length(breaks) < n_classes + 1) {
      breaks <- seq(min(values, na.rm = TRUE), max(values, na.rm = TRUE), length.out = n_classes + 1)
    }
    colors <- rev(brewer.pal(min(length(breaks) - 1, 11), palette))
  } else {
    colors <- rev(brewer.pal(min(n_classes, 11), palette))
  }
  
  # Assign classes
  if (metric == "quantiles") {
    band_data$value_class <- cut(values, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  }
  
  # Calculate center
  center_lat <- mean(band_data$Latitude, na.rm = TRUE)
  center_lon <- mean(band_data$Longitude, na.rm = TRUE)
  
  # Create base map
  map <- leaflet(band_data) %>%
    setView(lng = center_lon, lat = center_lat, zoom = 17) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
  
  # Add measurement tool if available
  if ("addMeasure" %in% ls("package:leaflet.extras")) {
    map <- map %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "hectares",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      )
  }
  
  # Add flight path lines
  for(dir in unique(band_data$DirectionGroup)) {
    dir_data <- band_data %>% 
      filter(DirectionGroup == dir) %>%
      arrange(TimeStamp)
    
    if(nrow(dir_data) > 1) {
      map <- map %>%
        addPolylines(
          data = dir_data,
          lng = ~Longitude, 
          lat = ~Latitude,
          color = "darkgray",
          weight = 1,
          opacity = 0.5,
          group = paste("Flight Path -", dir)
        )
    }
  }
  
  # Add data points
  if (metric == "anomalies") {
    # Color by anomaly status
    map <- map %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 8,
        color = "black",
        weight = 1,
        fillColor = ~ifelse(values == 1, "#E74C3C", "#27AE60"),
        fillOpacity = 0.8,
        group = "Data Points",
        popup = ~paste(
          "<strong>", band_name, " Band</strong><br/>",
          "<strong>Status:</strong>", ifelse(values == 1, "Anomaly", "Normal"), "<br/>",
          "<strong>Irradiance:</strong>", round(Irradiance, 4), "<br/>",
          "<strong>Direction:</strong>", FlightDirection, "<br/>",
          "<strong>Time:</strong>", format(TimeStamp, "%H:%M:%S"), "<br/>",
          "<strong>Image:</strong>", ImageIndex
        )
      )
  } else if (metric == "quantiles") {
    # Color by quantile
    pal <- colorBin(colors, domain = values, bins = breaks)
    
    map <- map %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 8,
        color = "black",
        weight = 1,
        fillColor = ~pal(values),
        fillOpacity = 0.8,
        group = "Data Points",
        popup = ~paste(
          "<strong>", band_name, " Band</strong><br/>",
          "<strong>Irradiance:</strong>", round(Irradiance, 4), "<br/>",
          "<strong>Quantile:</strong> Q", value_class, "<br/>",
          "<strong>Direction:</strong>", FlightDirection, "<br/>",
          "<strong>Time:</strong>", format(TimeStamp, "%H:%M:%S"), "<br/>",
          "<strong>Image:</strong>", ImageIndex
        )
      )
    
    # Add legend
    map <- map %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = values,
        title = paste(band_name, "<br>", title),
        opacity = 0.9
      )
  } else {
    # Standard continuous coloring
    pal <- colorNumeric(palette = colors, domain = values)
    
    map <- map %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 8,
        color = "black",
        weight = 1,
        fillColor = ~pal(values),
        fillOpacity = 0.8,
        group = "Data Points",
        popup = ~paste(
          "<strong>", band_name, " Band</strong><br/>",
          "<strong>", title, ":</strong>", round(values, 3), "<br/>",
          "<strong>Direction:</strong>", FlightDirection, "<br/>",
          "<strong>Time:</strong>", format(TimeStamp, "%H:%M:%S"), "<br/>",
          "<strong>Image:</strong>", ImageIndex
        )
      )
    
    # Add legend
    map <- map %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = values,
        title = paste(band_name, "<br>", title),
        opacity = 0.9
      )
  }
  
  # Add direction indicators (every 10th point)
  if ("FlightYaw" %in% names(band_data)) {
    arrow_data <- band_data %>%
      filter(row_number() %% 10 == 0)
    
    if (nrow(arrow_data) > 0) {
      for(i in 1:nrow(arrow_data)) {
        arrow_length <- 0.00005
        end_lat <- arrow_data$Latitude[i] + arrow_length * cos(arrow_data$FlightYaw[i] * pi / 180)
        end_lon <- arrow_data$Longitude[i] + arrow_length * sin(arrow_data$FlightYaw[i] * pi / 180)
        
        map <- map %>%
          addPolylines(
            lat = c(arrow_data$Latitude[i], end_lat),
            lng = c(arrow_data$Longitude[i], end_lon),
            color = "black",
            weight = 2,
            opacity = 0.7,
            group = "Direction Indicators"
          )
      }
    }
  }
  
  # Add summary statistics
  map <- map %>%
    addControl(
      html = paste(
        "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
        "<h4 style='margin: 0 0 10px 0;'>", band_name, " Band Analysis</h4>",
        "<table style='font-size: 12px;'>",
        "<tr><td><strong>Total Points:</strong></td><td>", nrow(band_data), "</td></tr>",
        "<tr><td><strong>Mean Irradiance:</strong></td><td>", round(mean(band_data$Irradiance), 4), "</td></tr>",
        "<tr><td><strong>Std Dev:</strong></td><td>", round(sd(band_data$Irradiance), 4), "</td></tr>",
        "<tr><td><strong>CV:</strong></td><td>", round(sd(band_data$Irradiance)/mean(band_data$Irradiance)*100, 1), "%</td></tr>",
        "</table>",
        "</div>",
        sep = ""
      ),
      position = "topright"
    )
  
  # Add layer controls
  overlay_groups <- c("Data Points")
  for(dir in unique(band_data$DirectionGroup)) {
    overlay_groups <- c(overlay_groups, paste("Flight Path -", dir))
  }
  if ("FlightYaw" %in% names(band_data)) {
    overlay_groups <- c(overlay_groups, "Direction Indicators")
  }
  
  map <- map %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite", "Topographic"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = FALSE),
      position = "topleft"
    )
  
  # Add mini map
  map <- map %>%
    addMiniMap(
      tiles = providers$CartoDB.Positron,
      toggleDisplay = TRUE,
      minimized = FALSE,
      position = "bottomleft",
      width = 150,
      height = 150
    )
  
  return(map)
}

# ==============================================================================
# 8. QUALITY METRICS CALCULATION
# ==============================================================================

#' Calculate comprehensive quality metrics
calculate_quality_metrics <- function(filtered_data) {
  
  # Calculate metrics by direction and band
  metrics <- filtered_data %>%
    group_by(DirectionGroup, BandName) %>%
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
    arrange(BandName, DirectionGroup)
  
  # Add temporal change metrics
  change_metrics <- filtered_data %>%
    group_by(DirectionGroup, BandName) %>%
    arrange(TimeStamp) %>%
    mutate(
      irr_change_percent = c(NA, diff(Irradiance) / head(Irradiance, -1) * 100)
    ) %>%
    summarise(
      mean_change_percent = mean(abs(irr_change_percent), na.rm = TRUE),
      max_change_percent = max(abs(irr_change_percent), na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Combine metrics
  metrics <- metrics %>%
    left_join(change_metrics, by = c("DirectionGroup", "BandName"))
  
  return(metrics)
}