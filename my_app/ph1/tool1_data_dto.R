# DTO / WFS Module

library(leaflet)
library(sf)
library(jsonlite)

tool1_data_dto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("region_map"), height = "400px"),
    verbatimTextOutput(ns("selected_region_info")),
    uiOutput(ns("proceed_btn_ui"))
  )
}

tool1_data_dto_server <- function(input, output, session, analysis_results, parent_session) {
  ns <- session$ns
  
  # Load WFS once
  wfs_url <- "https://odims.ospar.org/geoserver/odims/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=ospar_comp_au_2023_01_001&outputFormat=application/json"
  wfs_sf <- st_read(wfs_url, quiet = TRUE)
  
  pal <- colorFactor(rainbow(nrow(wfs_sf)), domain = wfs_sf$ID)
  
  output$region_map <- renderLeaflet({
    leaflet(wfs_sf) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(ID),
        color = "black",
        weight = 2,
        fillOpacity = 1,
        layerId = ~ID,
        label = ~paste0(LongName, " (", ID, ")")
      )
  })
  
  observeEvent(input$region_map_shape_click, {
    click <- input$region_map_shape_click
    analysis_results$region <- wfs_sf[wfs_sf$ID == click$id, ]
    
    output$selected_region_info <- renderPrint({
      analysis_results$region %>% st_drop_geometry()
    })
    
    output$proceed_btn_ui <- renderUI({
      actionButton(ns("proceed_btn"), "Proceed to Analysis", class = "btn-success")
    })
  })
  
  observeEvent(input$proceed_btn, {
    updateTabsetPanel(parent_session, "mainTabs", selected = "analysisTab")
  })
}
