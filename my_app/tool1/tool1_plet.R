# ==========================================================
# tool1_plet.R — cleaned and resilient for dynamic recreation
# ==========================================================

library(shiny)
library(leaflet)
library(sf)
library(arrow)
library(dplyr)
library(shinycssloaders)

tool1PletUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      # --- Map + title
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
        leafletOutput(ns("map"), height = "600px")
      ),
      # --- Right panel: dynamic controls
      column(width = 6, uiOutput(ns("controls_ui")))
    ),
    br(),
    div(class = "text-center",
        actionButton(ns("return_main"), "Return to main menu", class = "btn btn-warning")
    )
  )
}

tool1PletServer <- function(id, global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------- Reactive setup --------------------
    rv <- reactiveValues(
      features = NULL,
      selected_ID = NULL,
      datasets = NULL
    )
    
    # -------------------- Load spatial data --------------------
    comp4_wfs_url <- "https://odims.ospar.org/geoserver/odims/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=ospar_comp_au_2023_01_001&outputFormat=application/json"
    observe({
      rv$features <- tryCatch(
        sf::st_read(comp4_wfs_url, quiet = TRUE),
        error = function(e) NULL
      )
    })
    
    # -------------------- Arrow datasets --------------------
    s3_fs <- arrow::s3_bucket(
      bucket = "oidc-willemboone",
      endpoint_override = "https://minio.dive.edito.eu"
    )
    
    meta_ds <- arrow::open_dataset("data/merged-data3.parquet", filesystem = s3_fs)
    full_ds <- arrow::open_dataset("data/merged-data3.parquet", filesystem = s3_fs)
    
    dataset_cache <- reactiveVal(list())
    
    # -------------------- Map rendering --------------------
    output$map <- renderLeaflet({
      req(rv$features)
      leaflet(rv$features) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          layerId = ~ID,
          color = "blue",
          weight = 2,
          fillOpacity = 0.3,
          label = ~ID
        )
    })
    
    # -------------------- Polygon click logic --------------------
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id)
      rv$selected_ID <- click$id
      rv$datasets <- NULL
      output$controls_ui <- renderUI({})
      output$dataset_rows <- renderPrint({ NULL })
      
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(
          data = rv$features,
          layerId = ~ID,
          color = ~ifelse(ID == rv$selected_ID, "red", "blue"),
          weight = 2,
          fillOpacity = ~ifelse(ID == rv$selected_ID, 0.5, 0.3),
          label = ~ID
        )
      
      # --- Load dataset list (with caching)
      cache <- dataset_cache()
      if (!is.null(cache[[rv$selected_ID]])) {
        ds_names <- cache[[rv$selected_ID]]
      } else {
        ds_names <- meta_ds %>%
          filter(region_id == rv$selected_ID) %>%
          distinct(dataset_name) %>%
          collect() %>%
          pull(dataset_name)
        cache[[rv$selected_ID]] <- ds_names
        dataset_cache(cache)
      }
      rv$datasets <- ds_names
      
      # --- Render dataset selector
      output$controls_ui <- renderUI({
        req(rv$datasets)
        tagList(
          h5("Select dataset:"),
          selectInput(
            ns("dataset_select"), 
            label = NULL,
            choices = c(" " = "", rv$datasets), 
            selected = ""
          ),
          br(),
          uiOutput(ns("data_panel"))
        )
      })
    })
    
    # -------------------- Dataset selection --------------------
    observeEvent(input$dataset_select, {
      req(input$dataset_select)
      ds <- input$dataset_select
      if (ds == "") {
        output$data_panel <- renderUI({ NULL })
        return()
      }
      output$data_panel <- renderUI({
        tagList(
          h5("First 10 rows of selected dataset:"),
          withSpinner(verbatimTextOutput(ns("dataset_rows")))
        )
      })
      output$dataset_rows <- renderPrint({
        full_ds %>%
          filter(region_id == rv$selected_ID, dataset_name == ds) %>%
          head(10) %>%
          collect()
      })
    })
    
    # -------------------- Reset helper --------------------
    reset_tool1_ui <- function() {
      rv$selected_ID <- NULL
      rv$datasets <- NULL
      output$controls_ui <- renderUI({})
      output$dataset_rows <- renderPrint({ NULL })
    }
    
    # -------------------- Return to Main Menu --------------------
    observeEvent(input$return_main, {
      if (!is.null(session) && !session$isClosed()) {
        try(removeModal(), silent = TRUE)  # defensive modal cleanup
        reset_tool1_ui()
        global$return_to_tool1_menu <- isolate(global$return_to_tool1_menu) + 1
      }
    })
    
    # -------------------- Global Reset --------------------
    observeEvent(global$reset_all, {
      try(removeModal(), silent = TRUE)
      reset_tool1_ui()
    })
    
  })
}
