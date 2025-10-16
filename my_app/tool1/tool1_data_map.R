library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tibble)
library(arrow)

# ---- Map UI ----
tool1_data_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("region_map"), height = "400px"),
    verbatimTextOutput(ns("selected_region_info")),
    tableOutput(ns("region_data_summary")),
    uiOutput(ns("proceed_btn_ui"))
  )
}

# ---- Map Server ----
tool1_data_map_server <- function(id, analysis_results, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load WFS (regions)
    wfs_url <- "https://odims.ospar.org/geoserver/odims/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=ospar_comp_au_2023_01_001&outputFormat=application/json"
    wfs_sf <- sf::st_read(wfs_url, quiet = TRUE)
    
    # Load Parquet data
    parquet_url <- "https://minio.dive.edito.eu/oidc-willemboone/data/merged-data3.parquet"
    parquet_data <- arrow::read_parquet(parquet_url)
    
    pal <- colorFactor(rainbow(nrow(wfs_sf)), domain = wfs_sf$ID)
    
    # Render map
    output$region_map <- renderLeaflet({
      leaflet(wfs_sf) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(ID),
          color = "black",
          weight = 2,
          fillOpacity = 0.9,
          layerId = ~ID,
          label = ~paste0(LongName, " (", ID, ")")
        )
    })
    
    # When a region is clicked
    observeEvent(input$region_map_shape_click, {
      click <- input$region_map_shape_click
      req(click$id)
      
      selected_region <- wfs_sf[wfs_sf$ID == click$id, ]
      analysis_results$region <- selected_region
      
      output$selected_region_info <- renderPrint({
        sf::st_drop_geometry(selected_region)
      })
      
      region_data <- parquet_data %>% filter(region_id == click$id)
      if (nrow(region_data) > 0) {
        summary_df <- region_data %>%
          summarise(
            n_records = n(),
            across(where(is.numeric), ~mean(.x, na.rm = TRUE), .names = "mean_{col}")
          ) %>%
          t() %>%
          as.data.frame() %>%
          rownames_to_column("metric")
      } else {
        summary_df <- data.frame(metric = "No data available for this region.", value = NA)
      }
      
      output$region_data_summary <- renderTable(summary_df)
      
      output$proceed_btn_ui <- renderUI({
        actionButton(ns("proceed_btn"), "Proceed to Analysis", class = "btn-success")
      })
    })
    
    # Proceed button
    observeEvent(input$proceed_btn, {
      updateTabsetPanel(parent_session, "mainTabs", selected = "analysisTab")
    })
    
    # Reset map selection
    observe({
      if (is.null(analysis_results$region)) {
        output$selected_region_info <- renderPrint({ NULL })
        output$region_data_summary <- renderTable({ NULL })
        output$proceed_btn_ui <- renderUI({ NULL })
      }
    })
  })
}
