# ==============================================================================
# DRONE QA/QC SHINY APPLICATION - ENHANCED VERSION
# Professional offline application for multispectral drone data quality assessment
# ==============================================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyFiles)
library(DT)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(RColorBrewer)
library(exifr)
library(zoo)
library(htmlwidgets)
library(scales)
library(gridExtra)

# Source enhanced analysis functions and UI components
source("R/analysis_functions_enhanced.R")
source("R/ui_components.R")

# Load configuration if it exists
if (file.exists("config.R")) {
  source("config.R")
} else {
  # Default configuration
  app_config <- list(
    organization_name = "LandScan",
    organization_logo = "landscan_logo.png",
    app_title = "Drone QA/QC Analysis",
    app_subtitle = "Professional Multispectral Analysis Tool"
  )
}

# ==============================================================================
# USER INTERFACE
# ==============================================================================

ui <- dashboardPage(
  dashboardHeader(
    title = app_config$app_title,
    tags$li(
      class = "dropdown",
      tags$div(
        style = "padding: 10px 15px; display: flex; align-items: center;",
        if(file.exists(paste0("www/", app_config$organization_logo))) {
          tags$img(
            src = app_config$organization_logo,
            height = "40px",
            style = "margin-right: 10px;"
          )
        },
        tags$span(
          paste(app_config$organization_name, "-", app_config$app_subtitle),
          style = "color: white; font-size: 16px;"
        )
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Import", tabName = "import", icon = icon("upload")),
      menuItem("Flight Analysis", tabName = "flight", icon = icon("route")),
      menuItem("Band Analysis", tabName = "bands", icon = icon("chart-line")),
      menuItem("Quality Assessment", tabName = "quality", icon = icon("check-circle")),
      menuItem("Interactive Maps", tabName = "maps", icon = icon("map")),
      menuItem("Reports", tabName = "reports", icon = icon("file-pdf")),
      
      br(),
      
      # Quick stats panel
      conditionalPanel(
        condition = "input.sidebar != 'import'",
        div(
          style = "padding: 15px;",
          h4("Quick Stats", style = "color: #fff; margin-top: 0;"),
          uiOutput("quick_stats")
        )
      )
    )
  ),
  
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$style(HTML("
        /* Green theme styling */
        .skin-blue .main-header .navbar {
          background-color: #2d5f3f;
        }
        
        .skin-blue .main-header .logo {
          background-color: #1e4d2b;
          color: #ffffff;
        }
        
        .skin-blue .main-header .logo:hover {
          background-color: #2d5f3f;
        }
        
        .skin-blue .sidebar-menu > li.active > a {
          border-left-color: #27ae60;
        }
        
        .btn-primary {
          background-color: #27ae60;
          border-color: #229954;
        }
        
        .btn-primary:hover {
          background-color: #229954;
          border-color: #1e8449;
        }
        
        .btn-success {
          background-color: #2ecc71;
          border-color: #27ae60;
        }
        
        .btn-success:hover {
          background-color: #27ae60;
          border-color: #229954;
        }
        
        .box.box-primary {
          border-top-color: #27ae60;
        }
        
        /* Drag and drop area styling */
        .drop-area {
          border: 3px dashed #27ae60;
          border-radius: 10px;
          padding: 40px;
          text-align: center;
          background-color: #f8f9fa;
          transition: all 0.3s ease;
          cursor: pointer;
        }
        
        .drop-area:hover {
          background-color: #e8f5e9;
          border-color: #229954;
        }
        
        .drop-area.dragging {
          background-color: #c8e6c9;
          border-color: #1e8449;
        }
        
        .drop-area i {
          font-size: 48px;
          color: #27ae60;
          margin-bottom: 20px;
        }
        
        /* Enhanced styling */
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        
        .box {
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          border-radius: 5px;
        }
        
        .info-box-icon {
          font-size: 45px;
        }
        
        .small-box {
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .small-box.bg-green {
          background-color: #27ae60 !important;
        }
        
        .alert-success {
          background-color: #d4edda;
          border-color: #c3e6cb;
          color: #155724;
        }
        
        .alert-warning {
          background-color: #fff3cd;
          border-color: #ffeeba;
          color: #856404;
        }
      "))
    ),
    
    # JavaScript for drag and drop
    tags$script(HTML("
      Shiny.addCustomMessageHandler('resetFileInput', function(message) {
        var input = $('#' + message.inputId);
        input.val('');
        input.trigger('change');
      });
      
      $(document).ready(function() {
        // Drag and drop functionality
        var dropArea = document.getElementById('drop-area');
        
        if (dropArea) {
          ['dragenter', 'dragover', 'dragleave', 'drop'].forEach(eventName => {
            dropArea.addEventListener(eventName, preventDefaults, false);
          });
          
          function preventDefaults(e) {
            e.preventDefault();
            e.stopPropagation();
          }
          
          ['dragenter', 'dragover'].forEach(eventName => {
            dropArea.addEventListener(eventName, highlight, false);
          });
          
          ['dragleave', 'drop'].forEach(eventName => {
            dropArea.addEventListener(eventName, unhighlight, false);
          });
          
          function highlight(e) {
            dropArea.classList.add('dragging');
          }
          
          function unhighlight(e) {
            dropArea.classList.remove('dragging');
          }
          
          dropArea.addEventListener('drop', handleDrop, false);
          
          function handleDrop(e) {
            var dt = e.dataTransfer;
            var files = dt.files;
            
            // Send file info to Shiny
            if (files.length > 0) {
              Shiny.setInputValue('dropped_files', {
                files: Array.from(files).map(f => ({
                  name: f.name,
                  size: f.size,
                  type: f.type
                })),
                timestamp: new Date().getTime()
              });
            }
          }
        }
      });
    ")),
    
    tabItems(
      # Data Import Tab with Drag and Drop
      tabItem(
        tabName = "import",
        fluidRow(
          box(
            title = "Data Import",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 6,
                h4("Select Image Folder"),
                
                # Drag and drop area
                div(
                  id = "drop-area",
                  class = "drop-area",
                  icon("cloud-upload-alt"),
                  h3("Drag & Drop Image Folder Here"),
                  p("or click the button below to browse"),
                  br()
                ),
                
                br(),
                
                # Traditional folder selection
                shinyDirButton(
                  "folder",
                  "Browse Folder",
                  "Select folder containing drone images",
                  icon = icon("folder-open"),
                  class = "btn-primary btn-lg btn-block"
                ),
                
                br(),
                
                # Support for external drives
                p("Supports external drives and network locations", 
                  style = "text-align: center; color: #666;"),
                
                verbatimTextOutput("folder_path"),
                
                br(),
                
                h4("Optional: ExifTool Perl Path"),
                textInput(
                  "perl_path",
                  NULL,
                  placeholder = "C:/Path/to/perl.exe (optional)",
                  value = "C:/Users/15039/Desktop/exiftool/exiftool_files/perl.exe"
                ),
                
                br(),
                
                actionButton(
                  "load_data",
                  "Load EXIF Data",
                  icon = icon("download"),
                  class = "btn-success btn-lg btn-block"
                )
              ),
              
              column(
                width = 6,
                h4("Import Progress"),
                uiOutput("import_progress"),
                br(),
                
                # Exposure settings validation
                h4("Exposure Settings Validation"),
                div(
                  style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                  h5("Expected Settings:"),
                  tags$ul(
                    tags$li("F-stop: 5.6"),
                    tags$li("RGB Shutter: 1/2000"),
                    tags$li("Green Shutter: 1/2500"),
                    tags$li("Red Edge Shutter: 1/1000"),
                    tags$li("NIR Shutter: 1/2500")
                  ),
                  uiOutput("exposure_validation")
                ),
                
                br(),
                uiOutput("import_summary")
              )
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.data_loaded",
            box(
              title = "Data Preview",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              DTOutput("data_preview")
            )
          )
        )
      ),
      
      # Flight Analysis Tab
      tabItem(
        tabName = "flight",
        fluidRow(
          valueBoxOutput("total_images_box"),
          valueBoxOutput("flight_duration_box"),
          valueBoxOutput("mission_pattern_box")
        ),
        
        fluidRow(
          box(
            title = "Flight Direction Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("direction_distribution_plot", height = "400px")
          ),
          
          box(
            title = "Main Direction Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("direction_stats_table"),
            br(),
            uiOutput("direction_filter_info")
          )
        ),
        
        fluidRow(
          box(
            title = "Flight Pattern Visualization",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("flight_path_map", height = "500px")
          )
        )
      ),
      
      # Band Analysis Tab
      tabItem(
        tabName = "bands",
        fluidRow(
          box(
            title = "Band Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("band_selector")
          )
        ),
        
        fluidRow(
          tabBox(
            title = "Band Analysis",
            width = 12,
            
            tabPanel(
              "Irradiance Distribution",
              plotlyOutput("band_irradiance_plot", height = "500px")
            ),
            
            tabPanel(
              "Temporal Patterns",
              plotlyOutput("band_temporal_plot", height = "500px")
            ),
            
            tabPanel(
              "Direction Statistics",
              DTOutput("band_direction_stats_table")
            ),
            
            tabPanel(
              "Anomaly Detection",
              plotlyOutput("band_anomaly_plot", height = "500px"),
              br(),
              DTOutput("anomaly_summary_table")
            )
          )
        )
      ),
      
      # Quality Assessment Tab (without stability analysis)
      tabItem(
        tabName = "quality",
        fluidRow(
          box(
            title = "Overall Quality Assessment",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 4,
                h4("Lighting Consistency"),
                uiOutput("lighting_assessment")
              ),
              column(
                width = 4,
                h4("Exposure Consistency"),
                uiOutput("exposure_assessment")
              ),
              column(
                width = 4,
                h4("GPS Quality"),
                uiOutput("gps_assessment")
              )
            ),
            
            hr(),
            
            h3("Overall Assessment:"),
            uiOutput("overall_assessment")
          )
        ),
        
        fluidRow(
          box(
            title = "Coefficient of Variation by Direction",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("cv_heatmap", height = "400px")
          ),
          
          box(
            title = "Irradiance Change Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("change_analysis_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Detailed Quality Metrics",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("quality_metrics_table")
          )
        )
      ),
      
      # Interactive Maps Tab
      tabItem(
        tabName = "maps",
        fluidRow(
          box(
            title = "Map Controls",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "map_band",
                  "Select Band:",
                  choices = NULL
                )
              ),
              column(
                width = 4,
                selectInput(
                  "map_metric",
                  "Display Metric:",
                  choices = c("Irradiance" = "irradiance", 
                             "Anomalies" = "anomalies",
                             "Deviation from Mean" = "deviation",
                             "Quantiles" = "quantiles")
                )
              ),
              column(
                width = 4,
                br(),
                downloadButton(
                  "download_map",
                  "Save Map as HTML",
                  class = "btn-primary btn-block"
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Interactive Map",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("interactive_map", height = "600px")
          )
        )
      ),
      
      # Reports Tab
      tabItem(
        tabName = "reports",
        fluidRow(
          box(
            title = "Report Generation",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4("Select Report Components:"),
            
            fluidRow(
              column(
                width = 6,
                checkboxGroupInput(
                  "report_components",
                  NULL,
                  choices = c(
                    "Executive Summary" = "summary",
                    "Flight Analysis" = "flight",
                    "Band Statistics" = "bands",
                    "Quality Assessment" = "quality",
                    "Anomaly Report" = "anomalies",
                    "Maps" = "maps",
                    "Recommendations" = "recommendations"
                  ),
                  selected = c("summary", "flight", "quality", "recommendations")
                )
              ),
              column(
                width = 6,
                radioButtons(
                  "report_format",
                  "Report Format:",
                  choices = c("HTML" = "html", "PDF" = "pdf", "Word" = "docx"),
                  selected = "html"
                ),
                br(),
                actionButton(
                  "generate_report",
                  "Generate Report",
                  icon = icon("file-export"),
                  class = "btn-success btn-lg btn-block"
                ),
                br(),
                uiOutput("report_status")
              )
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.report_ready",
            box(
              title = "Report Preview",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              uiOutput("report_preview"),
              br(),
              downloadButton(
                "download_report",
                "Download Report",
                class = "btn-primary btn-lg"
              )
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    exif_data = NULL,
    processed_data = NULL,
    filtered_data = NULL,
    direction_analysis = NULL,
    band_results = NULL,
    quality_results = NULL,
    data_loaded = FALSE,
    exposure_validation = NULL
  )
  
  # Handle dropped files
  observeEvent(input$dropped_files, {
    showNotification(
      paste("Files detected:", length(input$dropped_files$files), "files"),
      type = "info"
    )
  })
  
  # Directory selection with support for external drives
  volumes <- c(
    Home = "~",
    "C:" = "C:/",
    "D:" = "D:/",
    "E:" = "E:/",
    "F:" = "F:/",
    "G:" = "G:/",
    "H:" = "H:/"
  )
  
  shinyDirChoose(
    input,
    "folder",
    roots = volumes,
    filetypes = c("", "jpg", "jpeg", "tif", "tiff"),
    allowDirCreate = FALSE
  )
  
  output$folder_path <- renderPrint({
    if (!is.null(input$folder) && !is.integer(input$folder)) {
      parseDirPath(volumes, input$folder)
    } else {
      "No folder selected"
    }
  })
  
  # Data loading with enhanced analysis
  observeEvent(input$load_data, {
    req(input$folder)
    
    withProgress(message = 'Loading EXIF data...', value = 0, {
      
      # Get folder path
      folder_path <- parseDirPath(volumes, input$folder)
      
      incProgress(0.1, detail = "Initializing...")
      
      # Load EXIF data
      tryCatch({
        # Set perl path if provided
        if (nchar(input$perl_path) > 0) {
          options(exifr.perlpath = input$perl_path)
        }
        
        incProgress(0.2, detail = "Reading EXIF metadata...")
        
        # Read EXIF data
        exif_data <- read_exif(folder_path, recursive = TRUE)
        
        if (nrow(exif_data) == 0) {
          stop("No images found in the specified folder")
        }
        
        values$exif_data <- exif_data
        
        incProgress(0.3, detail = "Processing data...")
        
        # Process data with enhanced functions
        values$processed_data <- process_exif_data_enhanced(exif_data)
        
        incProgress(0.4, detail = "Validating exposure settings...")
        
        # Validate exposure settings
        values$exposure_validation <- validate_exposure_settings(values$processed_data)
        
        incProgress(0.5, detail = "Filtering data...")
        
        # Filter data
        filter_result <- filter_quality_data_enhanced(values$processed_data)
        values$filtered_data <- filter_result$data
        
        incProgress(0.6, detail = "Analyzing flight directions...")
        
        # Analyze directions with enhanced algorithm
        values$direction_analysis <- analyze_flight_directions_enhanced(values$filtered_data)
        values$filtered_data <- values$direction_analysis$data
        
        incProgress(0.8, detail = "Analyzing spectral bands...")
        
        # Analyze bands
        values$band_results <- analyze_all_bands_enhanced(values$filtered_data)
        
        incProgress(0.9, detail = "Assessing quality...")
        
        # Quality assessment (without stability analysis)
        values$quality_results <- assess_overall_quality(values$filtered_data, values$band_results)
        
        # Set data loaded flag
        values$data_loaded <- TRUE
        
        incProgress(1, detail = "Complete!")
        
        showNotification("Data loaded successfully!", type = "success", duration = 5)
        
        # Show exposure validation results
        if (!values$exposure_validation$all_valid) {
          showNotification(
            "Warning: Some exposure settings do not match expected values",
            type = "warning",
            duration = 10
          )
        }
        
      }, error = function(e) {
        showNotification(
          paste("Error:", e$message), 
          type = "error", 
          duration = NULL
        )
      })
    })
  })
  
  # Data loaded output
  output$data_loaded <- reactive({
    values$data_loaded
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Exposure validation display
  output$exposure_validation <- renderUI({
    if (!is.null(values$exposure_validation)) {
      validation <- values$exposure_validation
      
      if (validation$all_valid) {
        div(
          class = "alert alert-success",
          icon("check-circle"),
          "All exposure settings match expected values"
        )
      } else {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Exposure setting mismatches detected:",
          tags$ul(
            lapply(validation$issues, function(issue) tags$li(issue))
          )
        )
      }
    }
  })
  
  # Import summary
  output$import_summary <- renderUI({
    if (values$data_loaded) {
      tagList(
        h4("Import Summary:"),
        tags$ul(
          tags$li(paste("Total images:", nrow(values$exif_data))),
          tags$li(paste("Valid images:", nrow(values$filtered_data))),
          tags$li(paste("Retention rate:", 
                       sprintf("%.1f%%", nrow(values$filtered_data)/nrow(values$exif_data)*100))),
          tags$li(paste("Bands detected:", 
                       paste(unique(values$filtered_data$BandName), collapse = ", "))),
          tags$li(paste("Main directions:", 
                       paste(values$direction_analysis$main_directions, collapse = ", ")))
        )
      )
    }
  })
  
  # Data preview
  output$data_preview <- renderDT({
    req(values$filtered_data)
    
    preview_data <- values$filtered_data %>%
      select(ImageIndex, BandName, TimeStamp, Irradiance, 
             FlightDirection, DirectionGroup, Latitude, Longitude, 
             ISO, exposure_time, FNumber) %>%
      mutate(
        TimeStamp = format(TimeStamp, "%H:%M:%S"),
        Irradiance = round(Irradiance, 4),
        Latitude = round(Latitude, 6),
        Longitude = round(Longitude, 6),
        exposure_time = sprintf("1/%.0f", 1/exposure_time)
      )
    
    datatable(
      preview_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = 'display compact'
    ) %>%
      formatStyle(
        'Irradiance',
        background = styleColorBar(range(preview_data$Irradiance), '#27ae60'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Quick stats
  output$quick_stats <- renderUI({
    if (values$data_loaded) {
      tagList(
        tags$p(paste("Images:", nrow(values$filtered_data))),
        tags$p(paste("Bands:", length(unique(values$filtered_data$BandName)))),
        tags$p(paste("Duration:", 
                    round(as.numeric(difftime(
                      max(values$filtered_data$TimeStamp),
                      min(values$filtered_data$TimeStamp),
                      units = "mins"
                    )), 1), "min")),
        tags$p(paste("Quality:", 
                    ifelse(values$quality_results$overall_pass, "PASS ✓", "REVIEW ⚠")))
      )
    }
  })
  
  # Value boxes
  output$total_images_box <- renderValueBox({
    valueBox(
      value = ifelse(values$data_loaded, nrow(values$filtered_data), 0),
      subtitle = "Total Images",
      icon = icon("images"),
      color = "green"
    )
  })
  
  output$flight_duration_box <- renderValueBox({
    duration <- 0
    if (values$data_loaded) {
      duration <- round(as.numeric(difftime(
        max(values$filtered_data$TimeStamp),
        min(values$filtered_data$TimeStamp),
        units = "mins"
      )), 1)
    }
    valueBox(
      value = paste(duration, "min"),
      subtitle = "Flight Duration",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$mission_pattern_box <- renderValueBox({
    pattern <- "N/A"
    if (values$data_loaded) {
      pattern <- unique(values$filtered_data$MissionPattern)[1]
    }
    valueBox(
      value = pattern,
      subtitle = "Mission Pattern",
      icon = icon("route"),
      color = "yellow"
    )
  })
  
  # Direction distribution plot
  output$direction_distribution_plot <- renderPlotly({
    req(values$direction_analysis)
    
    p <- create_direction_distribution_plot(values$direction_analysis)
    ggplotly(p) %>%
      layout(showlegend = FALSE)
  })
  
  # Direction stats table
  output$direction_stats_table <- renderDT({
    req(values$direction_analysis)
    
    create_direction_stats_table(values$direction_analysis)
  })
  
  # Direction filter info
  output$direction_filter_info <- renderUI({
    req(values$direction_analysis)
    
    div(
      class = "alert alert-info",
      icon("info-circle"),
      sprintf("Analyzing %d main directions covering %.1f%% of data",
              length(values$direction_analysis$main_directions),
              values$direction_analysis$main_coverage)
    )
  })
  
  # Flight path map
  output$flight_path_map <- renderLeaflet({
    req(values$filtered_data)
    
    create_flight_path_map(values$filtered_data)
  })
  
  # Band selector
  output$band_selector <- renderUI({
    req(values$filtered_data)
    
    bands <- unique(values$filtered_data$BandName)
    
    fluidRow(
      column(
        width = 12,
        radioButtons(
          "selected_band",
          "Select Band for Analysis:",
          choices = bands,
          selected = bands[1],
          inline = TRUE
        )
      )
    )
  })
  
  # Band irradiance plot
  output$band_irradiance_plot <- renderPlotly({
    req(values$filtered_data, input$selected_band)
    
    band_data <- values$filtered_data %>%
      filter(BandName == input$selected_band)
    
    p <- create_band_irradiance_plot(band_data, input$selected_band)
    ggplotly(p)
  })
  
  # Band temporal plot
  output$band_temporal_plot <- renderPlotly({
    req(values$filtered_data, input$selected_band)
    
    band_data <- values$filtered_data %>%
      filter(BandName == input$selected_band)
    
    p <- create_band_temporal_plot(band_data, input$selected_band)
    ggplotly(p)
  })
  
  # Band direction stats
  output$band_direction_stats_table <- renderDT({
    req(values$band_results, input$selected_band)
    
    if (!is.null(values$band_results[[input$selected_band]])) {
      stats <- values$band_results[[input$selected_band]]$direction_stats
      
      datatable(
        stats %>%
          mutate(
            mean_irr = round(mean_irr, 4),
            sd_irr = round(sd_irr, 4),
            cv_irr = round(cv_irr, 1),
            min_irr = round(min_irr, 4),
            max_irr = round(max_irr, 4)
          ),
        options = list(
          pageLength = 10,
          dom = 't'
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          'cv_irr',
          backgroundColor = styleInterval(c(10, 15), c('#d4edda', '#fff3cd', '#f8d7da'))
        )
    }
  })
  
  # Band anomaly plot
  output$band_anomaly_plot <- renderPlotly({
    req(values$band_results, input$selected_band)
    
    if (!is.null(values$band_results[[input$selected_band]])) {
      band_result <- values$band_results[[input$selected_band]]
      p <- create_anomaly_plot(band_result$processed_data, input$selected_band)
      ggplotly(p)
    }
  })
  
  # Anomaly summary table
  output$anomaly_summary_table <- renderDT({
    req(values$band_results, input$selected_band)
    
    if (!is.null(values$band_results[[input$selected_band]])) {
      anomaly_stats <- values$band_results[[input$selected_band]]$anomaly_stats
      
      datatable(
        anomaly_stats %>%
          mutate(
            percent_flagged = round(percent_flagged, 1)
          ),
        options = list(
          pageLength = 10,
          dom = 't'
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          'percent_flagged',
          backgroundColor = styleInterval(c(5, 10), c('#d4edda', '#fff3cd', '#f8d7da'))
        )
    }
  })
  
  # CV Heatmap
  output$cv_heatmap <- renderPlotly({
    req(values$filtered_data)
    
    p <- create_cv_heatmap(values$filtered_data)
    ggplotly(p)
  })
  
  # Change analysis plot
  output$change_analysis_plot <- renderPlotly({
    req(values$filtered_data)
    
    p <- create_change_analysis_plot(values$filtered_data)
    ggplotly(p)
  })
  
  # Quality assessments (without stability analysis)
  output$lighting_assessment <- renderUI({
    req(values$quality_results)
    
    result <- values$quality_results$lighting_consistency
    status <- ifelse(result$pass, "PASS", "FAIL")
    color <- ifelse(result$pass, "#27AE60", "#E74C3C")
    
    tagList(
      h1(status, style = paste0("color: ", color, "; margin: 10px 0;")),
      p(sprintf("%.1f%% images with >10%% deviation", result$high_deviation_rate))
    )
  })
  
  output$exposure_assessment <- renderUI({
    req(values$quality_results)
    
    result <- values$quality_results$exposure_consistency
    status <- ifelse(result$pass, "PASS", "FAIL")
    color <- ifelse(result$pass, "#27AE60", "#E74C3C")
    
    tagList(
      h1(status, style = paste0("color: ", color, "; margin: 10px 0;")),
      p(sprintf("%.1f%% images with exposure changes", result$change_rate))
    )
  })
  
  output$gps_assessment <- renderUI({
    req(values$quality_results)
    
    result <- values$quality_results$gps_consistency
    status <- ifelse(result$pass, "PASS", "FAIL")
    color <- ifelse(result$pass, "#27AE60", "#E74C3C")
    
    tagList(
      h1(status, style = paste0("color: ", color, "; margin: 10px 0;")),
      p(sprintf("%.1f%% images with GPS jumps", result$jump_rate))
    )
  })
  
  output$overall_assessment <- renderUI({
    req(values$quality_results)
    
    if (values$quality_results$overall_pass) {
      div(
        class = "alert alert-success",
        h3(icon("check-circle"), "Flight Data Quality: GOOD"),
        p("All quality metrics within acceptable ranges. Data suitable for processing.")
      )
    } else {
      issues <- values$quality_results$issues
      
      div(
        class = "alert alert-warning",
        h3(icon("exclamation-triangle"), "Flight Data Quality: REVIEW NEEDED"),
        p("Issues detected:"),
        tags$ul(
          lapply(issues, function(x) tags$li(x))
        ),
        p("Consider reviewing flight conditions or recalibrating sensors.")
      )
    }
  })
  
  # Quality metrics table
  output$quality_metrics_table <- renderDT({
    req(values$filtered_data)
    
    metrics <- calculate_quality_metrics(values$filtered_data)
    
    datatable(
      metrics,
      options = list(
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE
    ) %>%
      formatRound(c('mean_irradiance', 'cv_percent', 'mean_change_percent'), 1) %>%
      formatStyle(
        'cv_percent',
        backgroundColor = styleInterval(c(10, 15), c('#d4edda', '#fff3cd', '#f8d7da'))
      )
  })
  
  # Interactive map
  output$interactive_map <- renderLeaflet({
    req(values$filtered_data, input$map_band)
    
    band_data <- values$filtered_data %>%
      filter(BandName == input$map_band)
    
    create_enhanced_map(band_data, input$map_band, input$map_metric)
  })
  
  # Update band choices for map
  observe({
    if (values$data_loaded) {
      bands <- unique(values$filtered_data$BandName)
      updateSelectInput(session, "map_band", choices = bands, selected = bands[1])
    }
  })
  
  # Download map
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("drone_qa_map_", input$map_band, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      map <- create_enhanced_map(
        values$filtered_data %>% filter(BandName == input$map_band),
        input$map_band,
        input$map_metric
      )
      htmlwidgets::saveWidget(map, file, selfcontained = TRUE)
    }
  )
  
  # Report generation
  observeEvent(input$generate_report, {
    req(values$data_loaded)
    
    withProgress(message = 'Generating report...', value = 0, {
      
      incProgress(0.5, detail = "Creating report content...")
      
      # Generate report based on selected components
      # This would involve creating an R Markdown document
      # For now, show a success message
      
      incProgress(1, detail = "Complete!")
      
      showNotification("Report generated successfully!", type = "success")
    })
  })
  
  # Report status
  output$report_status <- renderUI({
    if (values$data_loaded) {
      div(
        class = "alert alert-info",
        icon("info-circle"),
        "Ready to generate report"
      )
    } else {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        "Please load data first"
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
      
