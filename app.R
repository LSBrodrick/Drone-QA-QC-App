# ==============================================================================
# DRONE QA/QC SHINY APPLICATION - ENHANCED VERSION
# ==============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyFiles)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(viridis)
library(RColorBrewer)
library(exifr)
library(zoo)
library(purrr)
library(htmlwidgets)
library(scales)
library(rmarkdown)

source("R/analysis_functions_enhanced.R")

if (file.exists("R/config.R")) {
  source("R/config.R")
} else {
  app_config <- list(
    organization_name = "LandScan",
    organization_logo = "landscan_logo.png",
    app_title = "Drone QA/QC Analysis",
    app_subtitle = "Professional Multispectral Analysis Tool",
    perl_path = "",
    quality_thresholds = list(
      lighting_deviation_threshold = 20, exposure_change_threshold = 10,
      gps_jump_threshold = 5, cv_threshold = 15, min_direction_percent = 10,
      min_direction_images = 20, stability_cv_threshold = 15, direction_diff_threshold = 10
    )
  )
}

# Ensure rmarkdown can find pandoc (needed outside RStudio)
if (!rmarkdown::pandoc_available()) {
  pandoc_candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC"),
    file.path(getwd(), "..", "pandoc"),                # Standalone build
    file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"),   # System install
    "C:/Program Files/Pandoc"
  )
  for (p in pandoc_candidates) {
    if (nchar(p) > 0 && dir.exists(p) &&
        any(file.exists(file.path(p, c("pandoc.exe", "pandoc"))))) {
      Sys.setenv(RSTUDIO_PANDOC = p)
      break
    }
  }
}

exiftool_status <- validate_exiftool_setup(app_config$perl_path)

# Check if PDF report generation is possible
pdf_available <- rmarkdown::pandoc_available() &&
  requireNamespace("tinytex", quietly = TRUE) &&
  tinytex::is_tinytex()

# ==============================================================================
# UI
# ==============================================================================
ui <- dashboardPage(
  dashboardHeader(title = app_config$app_title),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Import", tabName = "import", icon = icon("upload")),
      menuItem("Batch Processing", tabName = "batch", icon = icon("layer-group")),
      menuItem("Flight Analysis", tabName = "flight", icon = icon("route")),
      menuItem("Band Analysis", tabName = "bands", icon = icon("chart-line")),
      menuItem("Sun Sensor Analysis", tabName = "sunsensor", icon = icon("sun")),
      menuItem("Lighting Consistency", tabName = "lighting", icon = icon("cloud-sun")),
      menuItem("Quality Assessment", tabName = "quality", icon = icon("check-circle")),
      menuItem("Interactive Maps", tabName = "maps", icon = icon("map")),
      br(),
      conditionalPanel(
        condition = "output.data_loaded == true",
        div(style = "padding: 15px;",
          h4("Quick Stats", style = "color: #fff;"),
          uiOutput("quick_stats"),
          hr(style = "border-color: rgba(255,255,255,0.2);"),
          actionButton("generate_report_sidebar", "Generate Report",
                       icon = icon("file-pdf"), class = "btn-primary btn-block",
                       style = "margin-top: 5px;")
        )
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("
      .skin-blue .main-header .navbar { background-color: #2d5f3f; }
      .skin-blue .main-header .logo { background-color: #1e4d2b; }
      .btn-primary { background-color: #27ae60; border-color: #229954; }
      .btn-success { background-color: #2ecc71; border-color: #27ae60; }
      .stability-score { font-size: 36px; font-weight: bold; text-align: center; padding: 15px; border-radius: 10px; margin: 10px 0; }
      .stability-good { background-color: #d4edda; color: #155724; }
      .stability-warning { background-color: #fff3cd; color: #856404; }
    "))),
    
    tabItems(
      # Import Tab
      tabItem(tabName = "import",
        fluidRow(
          box(title = "Data Import", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 6,
                h4("Select Image Folder"),
                p("Supports local (C:, D:) and network drives (Y:, Z:)", style = "color: #666;"),
                shinyDirButton("folder", "Browse Folder", "Select folder containing drone images",
                              icon = icon("folder-open"), class = "btn-primary btn-lg btn-block"),
                br(),
                verbatimTextOutput("folder_path"),
                br(),
                actionButton("load_data", "Load EXIF Data", icon = icon("download"), class = "btn-success btn-lg btn-block"),
                uiOutput("exiftool_warning")
              ),
              column(width = 6,
                h4("Import Status"),
                uiOutput("import_progress"),
                br(),
                uiOutput("exposure_validation"),
                br(),
                uiOutput("import_summary")
              )
            )
          )
        ),
        fluidRow(
          conditionalPanel(condition = "output.data_loaded == true",
            box(title = "Data Preview", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
              DTOutput("data_preview")
            )
          )
        )
      ),
      
      # Flight Tab
      tabItem(tabName = "flight",
        fluidRow(
          valueBoxOutput("total_images_box"),
          valueBoxOutput("flight_duration_box"),
          valueBoxOutput("mission_pattern_box")
        ),
        fluidRow(
          box(title = "Direction Distribution", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("direction_distribution_plot", height = "400px")
          ),
          box(title = "Direction Statistics", status = "primary", solidHeader = TRUE, width = 6,
            DTOutput("direction_stats_table"),
            br(),
            uiOutput("direction_filter_info")
          )
        ),
        fluidRow(
          box(title = "Flight Path Map", status = "info", solidHeader = TRUE, width = 12,
            leafletOutput("flight_path_map", height = "500px")
          )
        )
      ),
      
      # Bands Tab
      tabItem(tabName = "bands",
        fluidRow(
          box(title = "Band Selection", status = "primary", solidHeader = TRUE, width = 12,
            uiOutput("band_selector")
          )
        ),
        fluidRow(
          tabBox(title = "Band Analysis", width = 12,
            tabPanel("Irradiance Distribution", plotlyOutput("band_irradiance_plot", height = "500px")),
            tabPanel("Temporal Patterns", plotlyOutput("band_temporal_plot", height = "500px")),
            tabPanel("Direction Statistics", DTOutput("band_direction_stats_table")),
            tabPanel("Anomaly Detection", plotlyOutput("band_anomaly_plot", height = "500px"), br(), DTOutput("anomaly_summary_table"))
          )
        )
      ),
      
      # Sun Sensor Tab (KEY FEATURE)
      tabItem(tabName = "sunsensor",
        fluidRow(
          box(title = "Sun Sensor Stability Analysis", status = "primary", solidHeader = TRUE, width = 12,
            h4("Compares irradiance readings across different flight directions"),
            p("Consistent irradiance across opposite directions indicates good sun sensor calibration."),
            hr(),
            fluidRow(
              column(width = 4, h4("Overall Stability"), uiOutput("sun_sensor_overall_status")),
              column(width = 8, h4("Stability Scores by Band"), plotlyOutput("stability_score_plot", height = "250px"))
            )
          )
        ),
        fluidRow(
          box(title = "Irradiance by Direction", status = "info", solidHeader = TRUE, width = 12,
            plotlyOutput("sun_sensor_direction_plot", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Bidirectional Comparison", status = "warning", solidHeader = TRUE, width = 6,
            p("Compares opposite directions (North vs South, East vs West)"),
            plotlyOutput("bidirectional_comparison_plot", height = "400px")
          ),
          box(title = "Direction Differences", status = "warning", solidHeader = TRUE, width = 6,
            DTOutput("direction_difference_table")
          )
        ),
        fluidRow(
          box(title = "Temporal Trends", status = "info", solidHeader = TRUE, width = 6, DTOutput("temporal_trends_table")),
          box(title = "Band Pass/Fail", status = "info", solidHeader = TRUE, width = 6, DTOutput("band_pass_fail_table"))
        )
      ),

      # Lighting Consistency Tab
      tabItem(tabName = "lighting",
        fluidRow(
          box(title = "Lighting Consistency Analysis", status = "primary", solidHeader = TRUE, width = 12,
            h4("Determines whether the imagery was captured under consistent lighting"),
            p("Irradiance is detrended by solar elevation so benign solar drift is separated from clouds, haze, and sensor faults."),
            hr(),
            fluidRow(
              column(width = 4, h4("Overall Lighting"), uiOutput("lighting_overall_status")),
              column(width = 8, h4("Scores by Band"), plotlyOutput("lighting_score_plot", height = "250px"))
            )
          )
        ),
        fluidRow(
          box(title = "Irradiance Timeline vs Clear-Sky Reference", status = "info", solidHeader = TRUE, width = 12,
            uiOutput("lighting_band_selector"),
            plotlyOutput("lighting_timeline_plot", height = "450px")
          )
        ),
        fluidRow(
          box(title = "Two-Sensor Corroboration", status = "info", solidHeader = TRUE, width = 6,
            p("Sun sensor (looking up) vs camera auto-exposure (looking down). Simultaneous dips confirm real lighting events; disagreement suggests a sensor fault.",
              style = "color: #666;"),
            plotlyOutput("lighting_ev_plot", height = "380px"),
            uiOutput("lighting_ev_note")
          ),
          box(title = "Per-Band Lighting Metrics", status = "info", solidHeader = TRUE, width = 6,
            DTOutput("lighting_metrics_table")
          )
        ),
        fluidRow(
          box(title = "Deep Scan — Raw Imagery Check", status = "warning", solidHeader = TRUE, width = 12,
            collapsible = TRUE,
            p("Reads a time-stratified sample of the raw multispectral TIFFs and verifies that exposure-normalized image brightness divided by sun-sensor irradiance stays stable — direct confirmation from the pixels themselves. Takes roughly 10–30 seconds.",
              style = "color: #666;"),
            actionButton("run_deep_scan", "Run Deep Scan", icon = icon("microscope"), class = "btn-warning"),
            br(), br(),
            uiOutput("deep_scan_status"),
            DTOutput("deep_scan_table")
          )
        )
      ),

      # Quality Tab
      tabItem(tabName = "quality",
        fluidRow(
          box(title = "Radiometric Quality", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 3, h4("Lighting"), uiOutput("lighting_assessment")),
              column(width = 3, h4("Exposure"), uiOutput("exposure_assessment")),
              column(width = 3, h4("GPS"), uiOutput("gps_assessment")),
              column(width = 3, h4("Sun Sensor"), uiOutput("sun_sensor_assessment"))
            )
          )
        ),
        fluidRow(
          box(title = "Flight & Sensor Diagnostics", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 3, h4("Altitude (AGL)"), uiOutput("altitude_assessment")),
              column(width = 3, h4("Gimbal (Nadir)"), uiOutput("gimbal_assessment")),
              column(width = 3, h4("Capture Rate"), uiOutput("capture_assessment")),
              column(width = 3, h4("Band Ratios"), uiOutput("band_ratio_assessment"))
            )
          )
        ),
        fluidRow(
          box(title = "Overall Verdict", status = "primary", solidHeader = TRUE, width = 6,
            uiOutput("overall_assessment")
          ),
          box(title = "Robust Outlier Summary (MAD-based)", status = "primary", solidHeader = TRUE, width = 6,
            DTOutput("outlier_summary_table"),
            uiOutput("coverage_info")
          )
        ),
        fluidRow(
          box(title = "Flagged Image Review", status = "warning", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = FALSE,
            p("Images flagged as statistical outliers (|Modified Z-Score| > 3.5).",
              "Click a row to view details and open the image file.",
              style = "color: #666; margin-bottom: 10px;"),
            DTOutput("flagged_images_table"),
            uiOutput("thumbnail_status")
          )
        ),
        fluidRow(
          box(title = "CV Heatmap", status = "info", solidHeader = TRUE, width = 6, plotlyOutput("cv_heatmap", height = "400px")),
          box(title = "Change Analysis", status = "info", solidHeader = TRUE, width = 6, plotlyOutput("change_analysis_plot", height = "400px"))
        ),
        fluidRow(
          box(title = "Detailed Metrics", status = "warning", solidHeader = TRUE, width = 12, DTOutput("quality_metrics_table"))
        ),
        fluidRow(
          box(width = 12,
            div(style = "text-align: center; padding: 15px;",
              actionButton("generate_report", "Generate PDF Report",
                           icon = icon("file-pdf"), class = "btn-success btn-lg"),
              p("Generates a single-page PDF summary of this analysis.",
                style = "color: #666; margin-top: 8px;")
            )
          )
        )
      ),

      # Maps Tab
      tabItem(tabName = "maps",
        fluidRow(
          box(title = "Map Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 4, selectInput("map_band", "Band:", choices = NULL)),
              column(width = 4, selectInput("map_metric", "Metric:",
                choices = c("Irradiance" = "irradiance", "Anomalies" = "anomalies", "Deviation" = "deviation"))),
              column(width = 4, br(), downloadButton("download_map", "Save Map", class = "btn-primary btn-block"))
            )
          )
        ),
        fluidRow(
          box(title = "Interactive Map", status = "info", solidHeader = TRUE, width = 12, leafletOutput("interactive_map", height = "600px"))
        )
      ),

      # Batch Processing Tab
      tabItem(tabName = "batch",
        fluidRow(
          box(title = "Batch Flight Processing", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 6,
                h4("Select Parent Folder"),
                p("Choose a folder containing multiple flight subfolders.",
                  style = "color: #666;"),
                shinyDirButton("batch_folder", "Browse Parent Folder",
                              "Select parent folder with flight subfolders",
                              icon = icon("folder-open"), class = "btn-primary btn-lg btn-block"),
                br(),
                verbatimTextOutput("batch_folder_path")
              ),
              column(width = 6,
                h4("Batch Controls"),
                actionButton("batch_scan", "Scan for Flights",
                             icon = icon("search"), class = "btn-primary btn-block",
                             style = "margin-bottom: 10px;"),
                actionButton("batch_process", "Process All Flights",
                             icon = icon("play-circle"), class = "btn-success btn-block",
                             style = "margin-bottom: 10px;"),
                uiOutput("batch_progress_info")
              )
            )
          )
        ),
        fluidRow(
          conditionalPanel(condition = "output.batch_has_flights == true",
            box(title = "Detected Flights", status = "info", solidHeader = TRUE, width = 12,
              DTOutput("batch_flights_table")
            )
          )
        ),
        fluidRow(
          conditionalPanel(condition = "output.batch_has_results == true",
            box(title = "Processing Results", status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("batch_results_table"),
              hr(),
              div(style = "text-align: center; padding: 10px;",
                actionButton("batch_generate_reports", "Generate All Reports",
                             icon = icon("file-pdf"), class = "btn-success btn-lg",
                             style = "margin-right: 10px;"),
                downloadButton("batch_download_zip", "Download All PDFs (ZIP)",
                               class = "btn-primary btn-lg")
              )
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  values <- reactiveValues(
    exif_data = NULL, processed_data = NULL, filtered_data = NULL,
    direction_analysis = NULL, band_results = NULL, quality_results = NULL,
    stability_analysis = NULL, advanced_qa = NULL,
    lighting_analysis = NULL, lighting_deep = NULL,
    data_loaded = FALSE, exposure_validation = NULL,
    folder_path = NULL, selected_flagged_path = NULL,
    # Batch processing
    batch_flights = NULL, batch_results = NULL, batch_processed = FALSE
  )
  
  # Auto-detect mounted drives (local + network)
  volumes <- c(Home = "~", shinyFiles::getVolumes()())
  
  shinyDirChoose(input, "folder", roots = volumes)
  
  output$folder_path <- renderPrint({
    if (!is.null(input$folder) && !is.integer(input$folder)) {
      parseDirPath(volumes, input$folder)
    } else {
      "No folder selected"
    }
  })
  
  observeEvent(input$load_data, {
    req(input$folder)
    
    withProgress(message = 'Loading...', value = 0, {
      folder_path <- parseDirPath(volumes, input$folder)
      
      tryCatch({
        if (!exiftool_status$ok) {
          stop(exiftool_status$message)
        }

        values$folder_path <- as.character(folder_path)

        incProgress(0.2, detail = "Reading EXIF...")
        exif_data <- read_exif(folder_path, recursive = TRUE, tags = REQUIRED_EXIF_TAGS)
        if (nrow(exif_data) == 0) stop("No images found")
        values$exif_data <- exif_data

        incProgress(0.4, detail = "Processing...")
        values$processed_data <- process_exif_data_enhanced(exif_data)
        values$exposure_validation <- validate_exposure_settings(values$processed_data)

        incProgress(0.5, detail = "Filtering...")
        filter_result <- filter_quality_data_enhanced(values$processed_data)
        values$filtered_data <- filter_result$data

        incProgress(0.6, detail = "Analyzing directions...")
        values$direction_analysis <- analyze_flight_directions_enhanced(values$filtered_data)
        values$filtered_data <- values$direction_analysis$data

        incProgress(0.7, detail = "Analyzing bands...")
        values$band_results <- analyze_all_bands_enhanced(values$filtered_data)

        incProgress(0.8, detail = "Sun sensor analysis...")
        values$stability_analysis <- analyze_sun_sensor_stability(values$filtered_data)

        incProgress(0.85, detail = "Lighting consistency...")
        values$lighting_analysis <- analyze_lighting_consistency(values$filtered_data)

        incProgress(0.9, detail = "Quality assessment...")
        values$quality_results <- assess_overall_quality(
          values$filtered_data, values$band_results, values$stability_analysis,
          values$lighting_analysis
        )

        values$advanced_qa <- NULL
        values$lighting_deep <- NULL
        values$data_loaded <- TRUE
        incProgress(1, detail = "Complete!")
        showNotification("Data loaded!", type = "message")

      }, error = function(e) {
        msg <- e$message
        if (grepl("configure_exiftool", msg, fixed = TRUE)) {
          msg <- paste0(msg, "\n\nCheck that ExifTool and Perl are installed, ",
                        "and perl_path in R/config.R is correct.")
        }
        showNotification(paste("Error:", msg), type = "error", duration = NULL)
      })
    })
  })
  
  output$exiftool_warning <- renderUI({
    if (!exiftool_status$ok) {
      div(class = "alert alert-danger", style = "margin-top: 10px;",
          icon("exclamation-triangle"), " ", exiftool_status$message)
    }
  })

  # Lazy-load advanced QA when Quality Assessment tab is visited
  observeEvent(input$sidebar, {
    if (input$sidebar == "quality" && is.null(values$advanced_qa) && values$data_loaded) {
      withProgress(message = "Running advanced diagnostics...", value = 0.5, {
        values$advanced_qa <- perform_advanced_qa(values$filtered_data)
      })
    }
  })

  output$data_loaded <- reactive({ values$data_loaded })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  output$exposure_validation <- renderUI({
    if (!is.null(values$exposure_validation)) {
      if (values$exposure_validation$all_valid) {
        div(class = "alert alert-success", icon("check"), " Exposure settings OK")
      } else {
        div(class = "alert alert-warning", icon("exclamation-triangle"), " Exposure issues: ",
            paste(values$exposure_validation$issues, collapse = "; "))
      }
    }
  })
  
  output$import_summary <- renderUI({
    if (values$data_loaded) {
      tagList(
        h4("Summary:"),
        tags$ul(
          tags$li(paste("Total:", nrow(values$exif_data), "images")),
          tags$li(paste("Valid:", nrow(values$filtered_data), "images")),
          tags$li(paste("Bands:", paste(unique(values$filtered_data$BandName), collapse=", "))),
          tags$li(paste("Directions:", paste(values$direction_analysis$main_directions, collapse=", ")))
        )
      )
    }
  })
  
  output$data_preview <- renderDT({
    req(values$filtered_data)
    values$filtered_data %>%
      select(ImageIndex, BandName, TimeStamp, Irradiance, FlightDirection, Latitude, Longitude) %>%
      mutate(TimeStamp = format(TimeStamp, "%H:%M:%S"), Irradiance = round(Irradiance, 4),
             Latitude = round(Latitude, 6), Longitude = round(Longitude, 6))
  }, server = TRUE, options = list(pageLength = 10, scrollX = TRUE))
  
  output$quick_stats <- renderUI({
    if (values$data_loaded) {
      tagList(
        tags$p(paste("Images:", nrow(values$filtered_data))),
        tags$p(paste("Bands:", length(unique(values$filtered_data$BandName)))),
        tags$p(paste("Stability:", ifelse(values$stability_analysis$overall_pass, "PASS", "REVIEW"))),
        tags$p(paste("Lighting:", if (isTRUE(values$lighting_analysis$available))
          values$lighting_analysis$overall_class else "N/A"))
      )
    }
  })
  
  # Value boxes
  output$total_images_box <- renderValueBox({
    valueBox(ifelse(values$data_loaded, nrow(values$filtered_data), 0), "Images", icon = icon("images"), color = "green")
  })
  
  output$flight_duration_box <- renderValueBox({
    duration <- 0
    if (values$data_loaded) {
      duration <- round(as.numeric(difftime(max(values$filtered_data$TimeStamp), min(values$filtered_data$TimeStamp), units = "mins")), 1)
    }
    valueBox(paste(duration, "min"), "Duration", icon = icon("clock"), color = "blue")
  })
  
  output$mission_pattern_box <- renderValueBox({
    pattern <- "N/A"
    if (values$data_loaded) pattern <- unique(values$filtered_data$MissionPattern)[1]
    valueBox(pattern, "Pattern", icon = icon("route"), color = "yellow")
  })
  
  # Direction plots (native plotly — no ggplotly conversion needed)
  output$direction_distribution_plot <- renderPlotly({
    req(values$direction_analysis)
    create_direction_distribution_plot(values$direction_analysis)
  })
  
  output$direction_stats_table <- renderDT({
    req(values$direction_analysis)
    stats <- create_direction_stats_table(values$direction_analysis)
    datatable(stats, options = list(pageLength = 8, dom = 't'), rownames = FALSE) %>%
      formatStyle('Status', backgroundColor = styleEqual(c('Main', 'Minor'), c('#D5F4E6', '#FADBD8')))
  })
  
  output$direction_filter_info <- renderUI({
    req(values$direction_analysis)
    div(class = "alert alert-info", icon("info-circle"),
        sprintf(" %d main directions (%.1f%% coverage)",
                length(values$direction_analysis$main_directions),
                values$direction_analysis$main_coverage))
  })
  
  output$flight_path_map <- renderLeaflet({
    req(values$filtered_data)
    create_flight_path_map(values$filtered_data)
  })
  
  # Band selector
  output$band_selector <- renderUI({
    req(values$filtered_data)
    bands <- unique(values$filtered_data$BandName)
    radioButtons("selected_band", "Select Band:", choices = bands, selected = bands[1], inline = TRUE)
  })
  
  # Single shared filter for all per-band outputs
  selected_band_data <- reactive({
    req(values$filtered_data, input$selected_band)
    values$filtered_data %>% filter(BandName == input$selected_band)
  })

  output$band_irradiance_plot <- renderPlotly({
    create_band_irradiance_plot(selected_band_data(), input$selected_band)
  })

  output$band_temporal_plot <- renderPlotly({
    create_band_temporal_plot(selected_band_data(), input$selected_band)
  })
  
  output$band_direction_stats_table <- renderDT({
    req(values$band_results, input$selected_band)
    if (!is.null(values$band_results[[input$selected_band]])) {
      stats <- values$band_results[[input$selected_band]]$direction_stats
      datatable(stats %>% mutate(across(where(is.numeric), ~round(., 4))),
                options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
        formatStyle('cv_irr', backgroundColor = styleInterval(c(10, 15), c('#d4edda', '#fff3cd', '#f8d7da')))
    }
  })
  
  output$band_anomaly_plot <- renderPlotly({
    req(values$band_results, input$selected_band)
    if (!is.null(values$band_results[[input$selected_band]])) {
      create_anomaly_plot(values$band_results[[input$selected_band]]$processed_data, input$selected_band)
    }
  })
  
  output$anomaly_summary_table <- renderDT({
    req(values$band_results, input$selected_band)
    if (!is.null(values$band_results[[input$selected_band]])) {
      datatable(values$band_results[[input$selected_band]]$anomaly_stats %>%
                  mutate(percent_flagged = round(percent_flagged, 1)),
                options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
        formatStyle('percent_flagged', backgroundColor = styleInterval(c(5, 10), c('#d4edda', '#fff3cd', '#f8d7da')))
    }
  })
  
  # Sun Sensor Analysis outputs
  output$sun_sensor_overall_status <- renderUI({
    req(values$stability_analysis)
    if (values$stability_analysis$overall_pass) {
      div(class = "stability-score stability-good", icon("check-circle"), br(), "STABLE")
    } else {
      div(class = "stability-score stability-warning", icon("exclamation-triangle"), br(), "REVIEW")
    }
  })
  
  output$stability_score_plot <- renderPlotly({
    req(values$stability_analysis)
    create_stability_gauge(values$stability_analysis)
  })

  output$sun_sensor_direction_plot <- renderPlotly({
    req(values$stability_analysis)
    create_sun_sensor_stability_plot(values$stability_analysis)
  })

  output$bidirectional_comparison_plot <- renderPlotly({
    req(values$stability_analysis)
    create_bidirectional_plot(values$stability_analysis)
  })
  
  output$direction_difference_table <- renderDT({
    req(values$stability_analysis)
    diff_data <- values$stability_analysis$direction_differences
    if (nrow(diff_data) > 0) {
      datatable(diff_data %>% mutate(across(where(is.numeric), ~round(., 3))),
                options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
        formatStyle('mean_diff_percent', backgroundColor = styleInterval(c(5, 10), c('#d4edda', '#fff3cd', '#f8d7da')))
    }
  })
  
  output$temporal_trends_table <- renderDT({
    req(values$stability_analysis)
    datatable(values$stability_analysis$temporal_trends %>% mutate(temporal_correlation = round(temporal_correlation, 3)),
              options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
      formatStyle('trend_direction', backgroundColor = styleEqual(c('Stable', 'Increasing', 'Decreasing'), c('#d4edda', '#fff3cd', '#fff3cd')))
  })
  
  output$band_pass_fail_table <- renderDT({
    req(values$stability_analysis)
    datatable(values$stability_analysis$band_pass_fail %>%
                select(BandName, mean_cv, stability_score, status) %>%
                mutate(mean_cv = round(mean_cv, 1), stability_score = round(stability_score, 0)),
              options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
      formatStyle('status', backgroundColor = styleEqual(c('PASS', 'REVIEW'), c('#d4edda', '#f8d7da')))
  })

  # Lighting Consistency outputs
  output$lighting_overall_status <- renderUI({
    req(values$lighting_analysis)
    la <- values$lighting_analysis
    if (!isTRUE(la$available)) {
      return(div(class = "alert alert-warning", la$message))
    }
    cls <- if (la$overall_pass) "stability-score stability-good" else "stability-score stability-warning"
    solar_note <- if (isTRUE(la$solar$ok)) {
      sprintf("Solar elevation %.0f° → %.0f° during flight", la$solar$elev_start, la$solar$elev_end)
    } else {
      "Solar detrending unavailable — raw variability used"
    }
    tagList(
      div(class = cls, la$overall_class, br(),
          span(paste("Score:", la$overall_score), style = "font-size: 18px;")),
      p(solar_note, style = "text-align: center; color: #666;")
    )
  })

  output$lighting_score_plot <- renderPlotly({
    req(isTRUE(values$lighting_analysis$available))
    create_lighting_score_plot(values$lighting_analysis)
  })

  output$lighting_band_selector <- renderUI({
    req(isTRUE(values$lighting_analysis$available))
    bands <- unique(values$lighting_analysis$per_band$BandName)
    radioButtons("lighting_band", NULL, choices = bands, selected = bands[1], inline = TRUE)
  })

  output$lighting_timeline_plot <- renderPlotly({
    req(isTRUE(values$lighting_analysis$available), input$lighting_band)
    create_lighting_timeline_plot(values$lighting_analysis, input$lighting_band)
  })

  output$lighting_ev_plot <- renderPlotly({
    req(isTRUE(values$lighting_analysis$available), input$lighting_band)
    p <- create_lighting_ev_plot(values$lighting_analysis, input$lighting_band)
    req(p)
    p
  })

  output$lighting_ev_note <- renderUI({
    req(isTRUE(values$lighting_analysis$available), input$lighting_band)
    pb <- values$lighting_analysis$per_band
    pb <- pb[pb$BandName == input$lighting_band, ]
    if (nrow(pb) > 0 && !isTRUE(pb$ev_informative)) {
      div(class = "alert alert-info", icon("info-circle"),
          " Exposure is fixed for this band, so camera-side corroboration is not applicable (expected with the standard M3M exposure SOP). The sun-sensor analysis above stands alone.")
    }
  })

  output$lighting_metrics_table <- renderDT({
    req(isTRUE(values$lighting_analysis$available))
    d <- values$lighting_analysis$per_band %>%
      transmute(
        Band = BandName,
        `Raw CV %` = round(raw_cv, 1),
        `Detrended CV %` = round(detrended_cv, 1),
        `Cloud Events` = n_events,
        `Affected %` = round(affected_pct, 1),
        `Heading R²` = round(yaw_r2, 2),
        Score = score,
        Classification = classification
      )
    datatable(d, options = list(pageLength = 10, dom = 't', scrollX = TRUE), rownames = FALSE) %>%
      formatStyle('Score', backgroundColor = styleInterval(c(50, 70), c('#f8d7da', '#fff3cd', '#d4edda')))
  })

  observeEvent(input$run_deep_scan, {
    req(values$filtered_data)
    withProgress(message = "Deep scan: reading sampled TIFFs...", value = 0, {
      values$lighting_deep <- deep_scan_reflectance_proxy(
        values$filtered_data,
        progress = function(p) setProgress(value = p)
      )
    })
  })

  output$deep_scan_status <- renderUI({
    ds <- values$lighting_deep
    if (is.null(ds)) return(NULL)
    if (!isTRUE(ds$available) || !isTRUE(ds$success)) {
      div(class = "alert alert-warning", icon("exclamation-triangle"), " ", ds$message)
    } else {
      div(class = if (ds$overall_pass) "alert alert-success" else "alert alert-warning",
          icon(if (ds$overall_pass) "check-circle" else "exclamation-triangle"), " ", ds$message)
    }
  })

  output$deep_scan_table <- renderDT({
    ds <- values$lighting_deep
    req(ds, isTRUE(ds$success))
    d <- ds$results %>%
      transmute(
        Band = BandName,
        `Images Sampled` = n_sampled,
        `Reflectance Proxy CV %` = round(reflect_cv, 1),
        `DLS Tracking (corr)` = round(dls_tracking, 2),
        Status = ifelse(pass, "PASS", "REVIEW")
      )
    datatable(d, options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
      formatStyle('Status', backgroundColor = styleEqual(c('PASS', 'REVIEW'), c('#d4edda', '#f8d7da')))
  })
  
  # Quality outputs (native plotly — no ggplotly conversion)
  output$cv_heatmap <- renderPlotly({ req(values$filtered_data); create_cv_heatmap(values$filtered_data) })
  output$change_analysis_plot <- renderPlotly({ req(values$filtered_data); create_change_analysis_plot(values$filtered_data) })
  
  output$lighting_assessment <- renderUI({
    req(values$quality_results)
    result <- values$quality_results$lighting_consistency
    status <- ifelse(result$pass, "PASS", "REVIEW")
    color <- ifelse(result$pass, "#27AE60", "#F39C12")
    detail <- if (!is.null(result$classification)) {
      result$classification
    } else {
      sprintf("%.1f%% deviation", result$high_deviation_rate)
    }
    tagList(h2(status, style = paste0("color: ", color)), p(detail),
            actionLink("go_to_lighting", "View details ->"))
  })

  observeEvent(input$go_to_lighting, { updateTabItems(session, "sidebar", "lighting") })
  
  output$exposure_assessment <- renderUI({
    req(values$quality_results)
    result <- values$quality_results$exposure_consistency
    status <- ifelse(result$pass, "PASS", "FAIL")
    color <- ifelse(result$pass, "#27AE60", "#E74C3C")
    tagList(h2(status, style = paste0("color: ", color)), p(sprintf("%.1f%% changes", result$change_rate)))
  })
  
  output$gps_assessment <- renderUI({
    req(values$quality_results)
    result <- values$quality_results$gps_consistency
    tagList(h2("INFO", style = "color: #3498DB"),
            p(sprintf("%.1f%% jumps", result$jump_rate)),
            p("Georeferenced downstream — not part of pass/fail",
              style = "color: #888; font-size: 11px;"))
  })
  
  output$sun_sensor_assessment <- renderUI({
    req(values$quality_results)
    stability <- values$quality_results$stability_analysis
    status <- ifelse(stability$overall_pass, "PASS", "REVIEW")
    color <- ifelse(stability$overall_pass, "#27AE60", "#F39C12")
    tagList(h2(status, style = paste0("color: ", color)), actionLink("go_to_sunsensor", "View details ->"))
  })
  
  observeEvent(input$go_to_sunsensor, { updateTabItems(session, "sidebar", "sunsensor") })

  # --- Advanced QA Assessment Boxes ---
  output$altitude_assessment <- renderUI({
    req(values$advanced_qa)
    r <- values$advanced_qa$altitude
    status <- ifelse(r$pass, "PASS", "FAIL")
    color <- ifelse(r$pass, "#27AE60", "#E74C3C")
    tagList(h2(status, style = paste0("color: ", color)),
            p(if (!is.na(r$cv)) sprintf("CV: %.1f%%, Range: %s ft", r$cv, r$range_ft) else r$detail))
  })

  output$gimbal_assessment <- renderUI({
    req(values$advanced_qa)
    r <- values$advanced_qa$gimbal
    status <- ifelse(r$pass, "PASS", "FAIL")
    color <- ifelse(r$pass, "#27AE60", "#E74C3C")
    tagList(h2(status, style = paste0("color: ", color)),
            p(if (!is.na(r$pct_off_nadir)) sprintf("%.1f%% off-nadir", r$pct_off_nadir) else r$detail))
  })

  output$capture_assessment <- renderUI({
    req(values$advanced_qa)
    r <- values$advanced_qa$capture_interval
    status <- ifelse(r$pass, "PASS", "FAIL")
    color <- ifelse(r$pass, "#27AE60", "#E74C3C")
    tagList(h2(status, style = paste0("color: ", color)),
            p(if (!is.na(r$median_interval)) sprintf("%d gaps, Median: %.1fs", r$n_gaps, r$median_interval) else r$detail))
  })

  output$band_ratio_assessment <- renderUI({
    req(values$advanced_qa)
    r <- values$advanced_qa$band_ratios
    status <- ifelse(r$pass, "PASS", "REVIEW")
    color <- ifelse(r$pass, "#27AE60", "#F39C12")
    detail_text <- if (length(r$ratios) > 0) {
      cvs <- sapply(r$ratios, function(x) x$cv)
      sprintf("Max ratio CV: %.1f%%", max(cvs))
    } else {
      r$detail
    }
    tagList(h2(status, style = paste0("color: ", color)), p(detail_text))
  })

  output$outlier_summary_table <- renderDT({
    req(values$advanced_qa)
    dt <- values$advanced_qa$outliers$per_band
    datatable(dt, options = list(pageLength = 10, dom = 't'), rownames = FALSE,
              colnames = c("Band", "Total", "Outliers", "% Outliers")) %>%
      formatStyle('pct_outliers', backgroundColor = styleInterval(c(2, 5), c('#d4edda', '#fff3cd', '#f8d7da')))
  })

  output$coverage_info <- renderUI({
    req(values$advanced_qa)
    r <- values$advanced_qa$coverage
    o <- values$advanced_qa$outliers
    tagList(
      hr(),
      p(strong("Spatial Coverage: "), r$detail),
      p(strong("Total Outlier Rate: "),
        span(sprintf("%.1f%%", o$total_pct),
             style = paste0("color: ", ifelse(o$pass, "#27AE60", "#E74C3C"), "; font-weight: bold;")),
        " (MAD-based Modified Z-Score, threshold: |Z| > 3.5)")
    )
  })

  # --- Combined Overall Assessment ---
  output$overall_assessment <- renderUI({
    req(values$quality_results)
    basic_pass <- values$quality_results$overall_pass
    adv_pass <- if (!is.null(values$advanced_qa)) values$advanced_qa$overall_pass else TRUE
    combined_pass <- basic_pass & adv_pass
    all_issues <- values$quality_results$issues
    if (!is.null(values$advanced_qa)) all_issues <- c(all_issues, values$advanced_qa$issues)

    if (combined_pass) {
      div(class = "alert alert-success",
          h3(icon("check-circle"), " Quality: GOOD"),
          p("All radiometric and flight diagnostics within acceptable limits."))
    } else {
      div(class = "alert alert-warning",
          h3(icon("exclamation-triangle"), " Quality: REVIEW"),
          p("Issues:"), tags$ul(lapply(all_issues, tags$li)))
    }
  })

  output$quality_metrics_table <- renderDT({
    req(values$filtered_data)
    metrics <- calculate_quality_metrics(values$filtered_data)
    datatable(metrics %>% mutate(across(where(is.numeric), ~round(., 2))),
              options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle('cv_percent', backgroundColor = styleInterval(c(10, 15), c('#d4edda', '#fff3cd', '#f8d7da')))
  })
  
  # Maps — update band choices only when data loads (not on every reactive flush)
  observeEvent(values$data_loaded, {
    req(values$data_loaded)
    updateSelectInput(session, "map_band", choices = unique(values$filtered_data$BandName))
  })
  
  # Render the base map once; use leafletProxy to update markers on band/metric change
  output$interactive_map <- renderLeaflet({
    req(values$filtered_data)
    center_lat <- mean(values$filtered_data$Latitude, na.rm = TRUE)
    center_lon <- mean(values$filtered_data$Longitude, na.rm = TRUE)
    leaflet() %>%
      setView(lng = center_lon, lat = center_lat, zoom = 17) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
        overlayGroups = "Data Points",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addMiniMap(tiles = providers$CartoDB.Positron, toggleDisplay = TRUE,
                 minimized = FALSE, position = "bottomleft", width = 150, height = 150)
  })

  # Update markers via proxy when band or metric changes (avoids full map rebuild)
  observe({
    req(values$filtered_data, input$map_band, input$map_metric)
    band_data <- values$filtered_data %>% filter(BandName == input$map_band)
    metric <- input$map_metric

    if (metric == "irradiance") {
      vals <- band_data$Irradiance
      title_label <- "Irradiance"
    } else if (metric == "anomalies") {
      if ("IrrFlag" %in% names(band_data)) {
        vals <- ifelse(band_data$IrrFlag, 1, 0)
      } else {
        mean_val <- mean(band_data$Irradiance, na.rm = TRUE)
        sd_val <- sd(band_data$Irradiance, na.rm = TRUE)
        vals <- ifelse(abs(band_data$Irradiance - mean_val) > 2 * sd_val, 1, 0)
      }
      title_label <- "Anomalies"
    } else {
      mean_val <- mean(band_data$Irradiance, na.rm = TRUE)
      vals <- (band_data$Irradiance - mean_val) / mean_val * 100
      title_label <- "Deviation %"
    }

    proxy <- leafletProxy("interactive_map") %>% clearMarkers() %>% clearControls()

    # Cluster very large flights to keep map interaction responsive
    cluster_opts <- if (nrow(band_data) > 1500) markerClusterOptions() else NULL

    if (metric == "anomalies") {
      proxy %>% addCircleMarkers(
        data = band_data, lng = ~Longitude, lat = ~Latitude,
        radius = 8, color = "black", weight = 1,
        fillColor = ifelse(vals == 1, "#E74C3C", "#27AE60"),
        fillOpacity = 0.8, group = "Data Points",
        clusterOptions = cluster_opts,
        popup = ~paste("<strong>", input$map_band, "</strong><br>",
                       "Status:", ifelse(vals == 1, "Anomaly", "Normal"), "<br>",
                       "Irradiance:", round(Irradiance, 4), "<br>",
                       "Direction:", FlightDirection)
      )
    } else {
      palette_name <- if (metric == "irradiance") "RdYlGn" else "RdBu"
      colors <- rev(RColorBrewer::brewer.pal(5, palette_name))
      pal <- colorNumeric(palette = colors, domain = vals, na.color = "gray")
      proxy %>% addCircleMarkers(
        data = band_data, lng = ~Longitude, lat = ~Latitude,
        radius = 8, color = "black", weight = 1,
        fillColor = pal(vals), fillOpacity = 0.8, group = "Data Points",
        clusterOptions = cluster_opts,
        popup = ~paste("<strong>", input$map_band, "</strong><br>",
                       title_label, ":", round(vals, 3), "<br>",
                       "Direction:", FlightDirection)
      ) %>%
        addLegend(position = "bottomright", pal = pal, values = vals,
                  title = paste(input$map_band, "<br>", title_label), opacity = 0.9)
    }
  })
  
  output$download_map <- downloadHandler(
    filename = function() { paste0("map_", input$map_band, "_", Sys.Date(), ".html") },
    content = function(file) {
      map <- create_enhanced_map(values$filtered_data %>% filter(BandName == input$map_band), input$map_band, input$map_metric)
      htmlwidgets::saveWidget(map, file, selfcontained = TRUE)
    }
  )

  # ===========================================================================
  # REPORT GENERATION
  # ===========================================================================

  show_report_modal <- function() {
    if (!pdf_available) {
      showNotification(
        "PDF reports require Pandoc and TinyTeX. Install TinyTeX with: tinytex::install_tinytex()",
        type = "error", duration = 15)
      return()
    }
    showModal(modalDialog(
      title = "Generate QA/QC Report",
      size = "m",
      easyClose = FALSE,

      textInput("report_operator", "Operator Name", placeholder = "e.g., John Smith"),
      textInput("report_ranch", "Ranch", placeholder = "e.g., Willow Creek Ranch"),
      textInput("report_blocks", "Block(s) Flown", placeholder = "e.g., Block 12, Block 14"),
      textInput("report_wind", "Wind Speed", placeholder = "e.g., 5-10 mph SW"),
      textAreaInput("report_notes", "Additional Notes", rows = 3,
                    placeholder = "Any observations about flight conditions, equipment, etc."),

      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_report", "Download PDF", class = "btn-success")
      )
    ))
  }

  observeEvent(input$generate_report, { show_report_modal() })
  observeEvent(input$generate_report_sidebar, { show_report_modal() })

  output$download_report <- downloadHandler(
    filename = function() {
      sanitize <- function(x) gsub("[^A-Za-z0-9_-]", "", gsub("\\s+", "_", trimws(x)))
      ranch_part  <- sanitize(input$report_ranch)
      blocks_part <- sanitize(input$report_blocks)
      date_part   <- format(Sys.Date(), "%Y-%m-%d")
      time_part   <- format(Sys.time(), "%H%M%S")

      if (nchar(ranch_part) > 0 && nchar(blocks_part) > 0) {
        sprintf("DroneQAQC_%s_%s_%s_%s.pdf", ranch_part, blocks_part, date_part, time_part)
      } else if (nchar(ranch_part) > 0) {
        sprintf("DroneQAQC_%s_%s_%s.pdf", ranch_part, date_part, time_part)
      } else {
        sprintf("DroneQAQC_Report_%s_%s.pdf", date_part, time_part)
      }
    },
    content = function(file) {
      withProgress(message = "Generating PDF report...", value = 0.3, {

        # Collect operator info from modal inputs
        operator_info <- list(
          operator_name = ifelse(nchar(trimws(input$report_operator)) > 0,
                                 input$report_operator, "Not specified"),
          ranch         = ifelse(nchar(trimws(input$report_ranch)) > 0,
                                 input$report_ranch, "Not specified"),
          blocks        = ifelse(nchar(trimws(input$report_blocks)) > 0,
                                 input$report_blocks, "Not specified"),
          wind_speed    = ifelse(nchar(trimws(input$report_wind)) > 0,
                                 input$report_wind, "Not recorded"),
          notes         = trimws(input$report_notes)
        )

        incProgress(0.2, detail = "Compiling data...")

        # Compute advanced QA if not yet run
        if (is.null(values$advanced_qa) && values$data_loaded) {
          values$advanced_qa <- perform_advanced_qa(values$filtered_data)
        }

        # Compile all report data (includes advanced QA if available)
        report_data <- compile_report_data(
          filtered_data      = values$filtered_data,
          quality_results    = values$quality_results,
          stability_analysis = values$stability_analysis,
          band_results       = values$band_results,
          operator_info      = operator_info,
          advanced_qa        = values$advanced_qa,
          folder_path        = values$folder_path,
          lighting_analysis  = values$lighting_analysis
        )

        incProgress(0.3, detail = "Rendering PDF...")

        # Copy template to tempdir and render there
        temp_rmd <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", temp_rmd, overwrite = TRUE)

        tryCatch({
          rmarkdown::render(
            input       = temp_rmd,
            output_file = file,
            params      = list(report_data = report_data),
            envir       = new.env(parent = globalenv()),
            quiet       = TRUE
          )
          incProgress(0.2, detail = "Done!")
          removeModal()
        }, error = function(e) {
          msg <- e$message
          if (grepl("pandoc", msg, ignore.case = TRUE)) {
            msg <- paste0(msg, "\n\nInstall pandoc (https://pandoc.org/installing.html) ",
                          "or run the app from within RStudio.")
          } else if (grepl("LaTeX|colortbl|tinytex", msg, ignore.case = TRUE)) {
            msg <- paste0(msg, "\n\nTry running in R console:\n",
                          "tinytex::reinstall_tinytex(repository = 'illinois')")
          }
          showNotification(
            paste("PDF generation failed:", msg),
            type = "error", duration = NULL
          )
        })
      })
    }
  )

  # ===========================================================================
  # FLAGGED IMAGE THUMBNAILS
  # ===========================================================================

  # Flagged images table (always visible — shows outlier details even without thumbnails)
  output$flagged_images_table <- renderDT({
    req(values$advanced_qa)
    flagged <- values$advanced_qa$outliers$flagged_images
    if (is.null(flagged) || nrow(flagged) == 0) return(NULL)

    display_df <- flagged %>%
      transmute(
        Image = basename(FileName),
        Band = BandName,
        Irradiance = round(Irradiance, 4),
        Z_Score = round(mod_z, 2),
        Direction = FlightDirection,
        Lat = round(Latitude, 6),
        Lon = round(Longitude, 6)
      )

    datatable(display_df,
              selection = "single",
              options = list(pageLength = 10, scrollX = TRUE,
                             order = list(list(3, "desc"))),
              rownames = FALSE) %>%
      formatStyle("Z_Score",
                  backgroundColor = styleInterval(c(-5, -3.5, 3.5, 5),
                    c('#f8d7da', '#fff3cd', '#d4edda', '#fff3cd', '#f8d7da')))
  })

  # Status message for flagged images
  output$thumbnail_status <- renderUI({
    req(values$advanced_qa)
    flagged <- values$advanced_qa$outliers$flagged_images
    if (is.null(flagged) || nrow(flagged) == 0) {
      div(class = "alert alert-success", icon("check-circle"),
          " No outlier images detected. All captures within normal range.")
    } else {
      div(class = "alert alert-info", icon("info-circle"),
          sprintf(" %d flagged images found. Click a row for details.",
                  nrow(flagged)))
    }
  })

  # Modal popup when clicking a flagged image row
  observeEvent(input$flagged_images_table_rows_selected, {
    req(values$advanced_qa)
    row_idx <- input$flagged_images_table_rows_selected
    flagged <- values$advanced_qa$outliers$flagged_images
    if (is.null(row_idx) || row_idx > nrow(flagged)) return()

    img_row <- flagged[row_idx, ]

    # SourceFile carries the full path; fall back to a folder scan for legacy data
    full_path <- if (!is.null(img_row$SourceFile) && !is.na(img_row$SourceFile) &&
                     file.exists(img_row$SourceFile)) {
      normalizePath(img_row$SourceFile, winslash = "/")
    } else {
      all_files <- list.files(values$folder_path, recursive = TRUE, full.names = TRUE)
      match_idx <- match(img_row$FileName, basename(all_files))
      if (!is.na(match_idx)) normalizePath(all_files[match_idx], winslash = "/") else NULL
    }
    values$selected_flagged_path <- full_path

    showModal(modalDialog(
      title = paste("Outlier Detail:", basename(img_row$FileName)),
      size = "l",
      fluidRow(
        column(6,
          h4("Image Details"),
          tags$table(class = "table table-condensed",
            tags$tr(tags$td(strong("File:")), tags$td(basename(img_row$FileName))),
            tags$tr(tags$td(strong("Band:")), tags$td(img_row$BandName)),
            tags$tr(tags$td(strong("Irradiance:")), tags$td(round(img_row$Irradiance, 4))),
            tags$tr(tags$td(strong("Modified Z-Score:")),
                    tags$td(span(round(img_row$mod_z, 2),
                                 style = paste0("color:", ifelse(abs(img_row$mod_z) > 5, "#E74C3C", "#F39C12"),
                                                "; font-weight:bold;"))))
          )
        ),
        column(6,
          h4("Location & Flight"),
          tags$table(class = "table table-condensed",
            tags$tr(tags$td(strong("Direction:")), tags$td(img_row$FlightDirection)),
            tags$tr(tags$td(strong("Latitude:")), tags$td(round(img_row$Latitude, 6))),
            tags$tr(tags$td(strong("Longitude:")), tags$td(round(img_row$Longitude, 6))),
            if ("Altitude" %in% names(img_row))
              tags$tr(tags$td(strong("Altitude:")),
                      tags$td(sprintf("%.0f ft", as.numeric(img_row$Altitude) * 3.28084)))
          )
        )
      ),
      if (!is.null(full_path))
        div(style = "margin-top: 10px; padding: 8px; background: #f5f5f5; border-radius: 4px; font-size: 12px; color: #666;",
            icon("folder-open"), " ", full_path),
      footer = tagList(
        if (!is.null(full_path)) actionButton("open_flagged_image", "Open Image",
                                               icon = icon("external-link-alt"), class = "btn-primary"),
        if (!is.null(full_path)) actionButton("open_flagged_folder", "Show in Explorer",
                                               icon = icon("folder-open"), class = "btn-info"),
        modalButton("Close")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$open_flagged_image, {
    req(values$selected_flagged_path)
    tryCatch(
      shell.exec(values$selected_flagged_path),
      error = function(e) showNotification(paste("Cannot open file:", e$message), type = "error")
    )
  })

  observeEvent(input$open_flagged_folder, {
    req(values$selected_flagged_path)
    tryCatch(
      shell(sprintf('explorer /select,"%s"', normalizePath(values$selected_flagged_path, winslash = "\\")), wait = FALSE),
      error = function(e) showNotification(paste("Cannot open Explorer:", e$message), type = "error")
    )
  })

  # ===========================================================================
  # BATCH PROCESSING
  # ===========================================================================

  # Batch folder browser (reuse same volumes)
  shinyDirChoose(input, "batch_folder", roots = volumes,
                 filetypes = c("", "jpg", "jpeg", "tif", "tiff"))

  output$batch_folder_path <- renderPrint({
    if (!is.null(input$batch_folder) && !is.integer(input$batch_folder)) {
      parseDirPath(volumes, input$batch_folder)
    } else {
      "No folder selected"
    }
  })

  output$batch_has_flights <- reactive({
    !is.null(values$batch_flights) && nrow(values$batch_flights) > 0
  })
  outputOptions(output, "batch_has_flights", suspendWhenHidden = FALSE)

  output$batch_has_results <- reactive({
    values$batch_processed
  })
  outputOptions(output, "batch_has_results", suspendWhenHidden = FALSE)

  # Scan for flight subfolders
  observeEvent(input$batch_scan, {
    req(input$batch_folder)
    parent_path <- parseDirPath(volumes, input$batch_folder)

    withProgress(message = "Scanning for flights...", value = 0.5, {
      flights <- scan_flight_folders(as.character(parent_path))
      if (nrow(flights) == 0) {
        showNotification("No image folders found in the selected directory.", type = "warning")
      } else {
        showNotification(sprintf("Found %d flight folders.", nrow(flights)), type = "message")
      }
      values$batch_flights <- flights
      values$batch_results <- NULL
      values$batch_processed <- FALSE
    })
  })

  output$batch_flights_table <- renderDT({
    req(values$batch_flights)
    datatable(values$batch_flights %>%
                select(folder_name, image_count, file_types) %>%
                rename(Folder = folder_name, Images = image_count, Types = file_types),
              options = list(pageLength = 20, dom = 't'), rownames = FALSE)
  })

  # Process all flights
  observeEvent(input$batch_process, {
    req(values$batch_flights)
    flights <- values$batch_flights
    n_flights <- nrow(flights)

    if (n_flights == 0) {
      showNotification("No flights to process.", type = "warning")
      return()
    }

    withProgress(message = "Batch processing...", value = 0, {
      results <- list()
      perl_path <- if (!is.null(app_config$perl_path)) app_config$perl_path else NULL

      for (i in seq_len(n_flights)) {
        incProgress(1 / n_flights,
                    detail = sprintf("Flight %d/%d: %s", i, n_flights, flights$folder_name[i]))
        results[[i]] <- process_single_flight(flights$folder_path[i], perl_path)
      }

      values$batch_results <- results
      values$batch_processed <- TRUE

      n_ok <- sum(vapply(results, function(r) r$success, logical(1)))
      showNotification(
        sprintf("Processed %d/%d flights successfully.", n_ok, n_flights),
        type = if (n_ok == n_flights) "message" else "warning"
      )
    })
  })

  output$batch_results_table <- renderDT({
    req(values$batch_results)
    summaries <- do.call(rbind, lapply(values$batch_results, function(r) r$summary))

    datatable(summaries,
              options = list(pageLength = 20, scrollX = TRUE, dom = 'ft'),
              rownames = FALSE) %>%
      formatStyle("Pass",
                  backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da")),
                  fontWeight = "bold") %>%
      formatStyle("Outlier_Pct",
                  backgroundColor = styleInterval(c(2, 5), c("#d4edda", "#fff3cd", "#f8d7da")))
  })

  output$batch_progress_info <- renderUI({
    if (values$batch_processed && !is.null(values$batch_results)) {
      n_total <- length(values$batch_results)
      n_pass <- sum(vapply(values$batch_results, function(r) {
        r$success && r$summary$Pass
      }, logical(1)))
      n_fail <- n_total - n_pass
      tagList(
        div(class = if (n_fail == 0) "alert alert-success" else "alert alert-warning",
          icon(if (n_fail == 0) "check-circle" else "exclamation-triangle"),
          sprintf(" %d/%d flights passed QA", n_pass, n_total)
        )
      )
    }
  })

  # Batch report generation modal
  observeEvent(input$batch_generate_reports, {
    req(values$batch_results)
    showModal(modalDialog(
      title = "Generate Batch Reports",
      size = "m",
      easyClose = FALSE,
      textInput("batch_operator", "Operator Name", placeholder = "e.g., John Smith"),
      textInput("batch_ranch", "Ranch", placeholder = "e.g., Willow Creek Ranch"),
      textInput("batch_wind", "Wind Speed", placeholder = "e.g., 5-10 mph SW"),
      textAreaInput("batch_notes", "Additional Notes", rows = 2,
                    placeholder = "Notes applied to all reports"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("batch_confirm_reports", "Generate All PDFs",
                     class = "btn-success", icon = icon("file-pdf"))
      )
    ))
  })

  # Actually generate batch PDFs
  observeEvent(input$batch_confirm_reports, {
    req(values$batch_results)
    removeModal()

    withProgress(message = "Generating batch reports...", value = 0, {
      results <- values$batch_results
      n_results <- length(results)
      output_dir <- file.path(tempdir(), "batch_reports")
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

      operator_info <- list(
        operator_name = ifelse(nchar(trimws(input$batch_operator)) > 0,
                               input$batch_operator, "Not specified"),
        ranch = ifelse(nchar(trimws(input$batch_ranch)) > 0,
                       input$batch_ranch, "Not specified"),
        blocks = "",
        wind_speed = ifelse(nchar(trimws(input$batch_wind)) > 0,
                            input$batch_wind, "Not recorded"),
        notes = trimws(input$batch_notes)
      )

      temp_rmd <- file.path(tempdir(), "report_template.Rmd")
      file.copy("report_template.Rmd", temp_rmd, overwrite = TRUE)

      n_generated <- 0
      for (i in seq_len(n_results)) {
        r <- results[[i]]
        if (!r$success) next

        incProgress(1 / n_results,
                    detail = sprintf("Report %d/%d: %s", i, n_results, r$summary$Folder))

        # Each flight folder is one block
        op <- operator_info
        op$blocks <- r$summary$Folder

        tryCatch({
          rd <- compile_report_data(
            filtered_data = r$filtered_data,
            quality_results = r$quality_results,
            stability_analysis = r$stability_analysis,
            band_results = r$band_results,
            operator_info = op,
            advanced_qa = r$advanced_qa,
            folder_path = r$folder_path,
            lighting_analysis = r$lighting_analysis
          )

          sanitize <- function(x) gsub("[^A-Za-z0-9_-]", "", gsub("\\s+", "_", trimws(x)))
          pdf_name <- sprintf("DroneQAQC_%s_%s.pdf",
                              sanitize(r$summary$Folder),
                              format(Sys.Date(), "%Y-%m-%d"))
          pdf_path <- file.path(output_dir, pdf_name)

          rmarkdown::render(
            input = temp_rmd,
            output_file = pdf_path,
            params = list(report_data = rd),
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
          n_generated <- n_generated + 1
        }, error = function(e) {
          showNotification(
            sprintf("Report failed for %s: %s", r$summary$Folder, e$message),
            type = "warning", duration = 5
          )
        })
      }

      # Store output dir for ZIP download
      values$batch_report_dir <- output_dir
      showNotification(
        sprintf("Generated %d/%d reports. Use 'Download All PDFs' to save.",
                n_generated, sum(vapply(results, function(r) r$success, logical(1)))),
        type = "message"
      )
    })
  })

  # ZIP download handler for batch reports
  output$batch_download_zip <- downloadHandler(
    filename = function() {
      sprintf("DroneQAQC_Batch_%s.zip", format(Sys.Date(), "%Y-%m-%d"))
    },
    content = function(file) {
      req(values$batch_report_dir)
      report_dir <- values$batch_report_dir
      pdf_files <- list.files(report_dir, pattern = "\\.pdf$", full.names = TRUE)

      if (length(pdf_files) == 0) {
        showNotification("No reports generated yet. Click 'Generate All Reports' first.",
                         type = "warning")
        return()
      }

      # Create ZIP in tempdir
      old_wd <- setwd(report_dir)
      on.exit(setwd(old_wd))
      utils::zip(file, files = basename(pdf_files))
    }
  )
}

shinyApp(ui = ui, server = server)
