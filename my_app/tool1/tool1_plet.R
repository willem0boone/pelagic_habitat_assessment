# ==========================================================
# tool1/tool1_plet.R — PLET workflow module
# ==========================================================

library(shiny)
library(leaflet)
library(sf)
library(arrow)
library(dplyr)
library(shinycssloaders)
library(magrittr)
library(fs)

# --- Logging ---
tool1_log_dir <- "tool1/logs"
if (!dir.exists(tool1_log_dir)) dir.create(tool1_log_dir, recursive = TRUE)
log_file <- file.path(tool1_log_dir, "log.txt")
log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0(timestamp, " - ", msg, "\n"), file = log_file, append = TRUE)
}

# --- UI ---
tool1PletUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      # Left: map + feature info
      column(
        width = 6,
        tags$h5(
          tags$a(
            href = "https://odims.ospar.org/en/submissions/ospar_comp_au_2023_01/",
            target = "_blank",
            "OSPAR Common Procedure Assessment Unit"
          ),
          " — select a unit on the map below"
        ),
        leafletOutput(ns("map"), height = "520px"),
        br(),
        h6("Selected feature info:"),
        verbatimTextOutput(ns("feature_info")),
        br(),
        uiOutput(ns("dataset_ui")),
        br(),
        sliderInput(ns("ref_years"), "Reference period:",
                    min = 1990, max = 2030, value = c(2010, 2017), sep = ""),
        sliderInput(ns("comp_years"), "Comparison period:",
                    min = 1990, max = 2030, value = c(2017, 2025), sep = "")
      ),
      # Right: first 10 rows + period info
      column(
        width = 6,
        h5("First 10 rows of selected dataset:"),
        withSpinner(verbatimTextOutput(ns("dataset_preview"))),
        br(),
        h5("Selected periods:"),
        verbatimTextOutput(ns("period_info"))
      )
    ),
    br(),
    div(
      class = "text-center",
      actionButton(ns("return_main"), "Return to main menu", class = "btn btn-warning")
    )
  )
}

# --- SERVER ---
tool1PletServer <- function(id, global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      features = NULL,
      selected_ID = NULL,
      datasets = NULL
    )
    
    ref_start <- reactiveVal(2010)
    ref_end <- reactiveVal(2017)
    comp_start <- reactiveVal(2017)
    comp_end <- reactiveVal(2025)
    
    # Load WFS
    tool1_wfs_url <- "https://odims.ospar.org/geoserver/odims/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=ospar_comp_au_2023_01_001&outputFormat=application/json"
    observe({
      rv$features <- tryCatch(
        sf::st_read(tool1_wfs_url, quiet = TRUE),
        error = function(e) { warning("Failed to read WFS: ", conditionMessage(e)); NULL }
      )
    })
    
    # Connect Parquet
    arrow_ds_ok <- reactiveVal(FALSE)
    meta_ds <- NULL
    full_ds <- NULL
    observe({
      tryCatch({
        s3_fs <- arrow::s3_bucket(bucket = "oidc-willemboone",
                                  endpoint_override = "https://minio.dive.edito.eu")
        meta_ds <<- arrow::open_dataset("data/merged-data3.parquet", filesystem = s3_fs)
        full_ds <<- arrow::open_dataset("data/merged-data3.parquet", filesystem = s3_fs)
        arrow_ds_ok(TRUE)
      }, error = function(e) { warning("Failed to open parquet: ", conditionMessage(e)); arrow_ds_ok(FALSE) })
    })
    
    # Render map
    output$map <- renderLeaflet({
      req(rv$features)
      validate(need("ID" %in% names(rv$features), "WFS features do not include 'ID'."))
      leaflet(rv$features) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(layerId = ~ID, color = "blue", weight = 2, fillOpacity = 0.25,
                    highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
                    label = ~ID)
    })
    
    # --- Polygon click ---
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id)
      rv$selected_ID <- click$id
      log_message(paste("Region selected:", rv$selected_ID))
      
      # Highlight polygon
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data = rv$features, layerId = ~ID,
                    color = ~ifelse(ID == rv$selected_ID, "red", "blue"),
                    weight = 2, fillOpacity = ~ifelse(ID == rv$selected_ID, 0.5, 0.25),
                    highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
                    label = ~ID)
      
      # Display feature info
      output$feature_info <- renderPrint({
        feature <- rv$features[rv$features$ID == rv$selected_ID, , drop = FALSE]
        if (nrow(feature) == 0) return(cat("No feature properties found."))
        props <- feature
        st_geometry(props) <- NULL
        props
      })
      
      # Load datasets
      if (arrow_ds_ok()) {
        ds_names <- tryCatch({
          meta_ds %>%
            filter(region_id == rv$selected_ID) %>%
            select(dataset_name) %>% distinct() %>% collect() %>% pull(dataset_name)
        }, error = function(e) { warning(e); character(0) })
        rv$datasets <- ds_names
      } else rv$datasets <- character(0)
    })
    
    # Dataset select UI
    output$dataset_ui <- renderUI({
      req(rv$datasets)
      if (length(rv$datasets) == 0) p("No datasets available for this region.") else
        selectInput(ns("dataset_select"), "Select dataset:",
                    choices = c(" " = "", rv$datasets), selected = "",
                    width = "150%")
    })
    
    # Dataset selection
    observeEvent(input$dataset_select, {
      ds <- input$dataset_select
      if (is.null(ds) || ds == "") return()
      log_message(paste("Dataset selected:", ds))
      
      output$dataset_preview <- renderPrint({
        req(rv$selected_ID)
        withProgress(message = "Loading dataset (first 10 rows)...", value = 0, {
          incProgress(0.2)
          tbl <- tryCatch({
            full_ds %>%
              filter(region_id == rv$selected_ID, dataset_name == ds) %>%
              head(10) %>% collect()
          }, error = function(e) { warning(e); tibble::tibble() })
          incProgress(0.8)
          tbl
        })
      })
    })
    
    # Time sliders
    observeEvent(input$ref_years, {
      ref_start(input$ref_years[1])
      ref_end(input$ref_years[2])
      log_message(paste("ref_start set to", ref_start(), "ref_end set to", ref_end()))
    })library(shiny)
    library(leaflet)
    library(sf)
    library(dplyr)
    library(shinycssloaders)
    library(magrittr)
    library(arrow)
    library(fs)
    
    tool1PletUI <- function(id) {
      ns <- NS(id)
      fluidPage(
        h4("Tool1 – PLET Workflow"),
        leafletOutput(ns("map"), height = "400px"),
        uiOutput(ns("dataset_ui")),
        verbatimTextOutput(ns("dataset_preview")),
        actionButton(ns("reset_workflow"), "Reset PLET Workflow", class = "btn btn-warning mt-3")
      )
    }
    
    tool1PletServer <- function(id, global) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        rv <- reactiveValues(selected_ID = NULL, dataset = NULL)
        
        # ---------------- Map and dataset logic ----------------
        output$map <- renderLeaflet({
          leaflet() %>% addProviderTiles("CartoDB.Positron")
        })
        
        output$dataset_ui <- renderUI({
          selectInput(ns("dataset_select"), "Select dataset:", choices = c("", "A", "B"), selected = "")
        })
        
        observeEvent(input$dataset_select, {
          rv$dataset <- input$dataset_select
          output$dataset_preview <- renderPrint({ paste("Preview for dataset:", rv$dataset) })
        })
        
        # ---------------- Reset workflow ----------------
        reset <- function() {
          rv$selected_ID <- NULL
          rv$dataset <- NULL
          output$dataset_preview <- renderPrint({ "" })
          updateSelectInput(session, "dataset_select", selected = "")
        }
        
        observeEvent(input$reset_workflow, {
          showModal(modalDialog(
            title = "Reset PLET Workflow",
            "This will reset all PLET selections.",
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_reset"), "OK", class = "btn btn-danger")
            )
          ))
        })
        
        observeEvent(input$confirm_reset, {
          removeModal()
          reset()
        })
        
        invisible(list(reset = reset))
      })
    }
    
    observeEvent(input$comp_years, {
      comp_start(input$comp_years[1])
      comp_end(input$comp_years[2])
      log_message(paste("comp_start set to", comp_start(), "comp_end set to", comp_end()))
    })
    
    output$period_info <- renderPrint({
      cat("Reference period:", ref_start(), "-", ref_end(), "\n")
      cat("Comparison period:", comp_start(), "-", comp_end(), "\n")
    })
    
    # Return to main menu
    observeEvent(input$return_main, {
      rv$selected_ID <- NULL
      rv$datasets <- NULL
      log_message("Tool1 reset via Return to main menu")
      global$return_to_tool1_menu <- isolate(global$return_to_tool1_menu) + 1
    })
    
  })
}
