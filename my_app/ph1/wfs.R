library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(RColorBrewer)

wfs_url <- "https://odims.ospar.org/geoserver/odims/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=ospar_comp_au_2023_01_001&outputFormat=application/json"

geo_data <- function(url) {
  res <- httr::GET(url)
  stop_for_status(res)
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

geo <- geo_data(wfs_url)
features <- geo$features

flatten_coords <- function(coords) {
  to_matrix <- function(poly) {
    mat <- do.call(rbind, lapply(poly[[1]], function(pt) c(as.numeric(pt[[1]]), as.numeric(pt[[2]]))))
    colnames(mat) <- c("lng", "lat")
    mat
  }
  
  if (is.numeric(coords[[1]][[1]])) {
    list(to_matrix(list(coords)))
  } else if (is.list(coords[[1]][[1]])) {
    lapply(coords, to_matrix)
  } else {
    stop("Unknown geometry structure")
  }
}

# Assign distinct colors
num_features <- length(features)
colors <- colorFactor(brewer.pal(min(12, num_features), "Set3"), domain = 1:num_features)

ui <- fluidPage(
  titlePanel("OSPAR WFS Map Viewer"),
  leafletOutput("map", height = 600),
  hr(),
  h4("Clicked Feature Info:"),
  verbatimTextOutput("feature_info")
)

server <- function(input, output, session) {
  
  # Prepare leaflet polygons
  poly_list <- lapply(seq_along(features), function(i) {
    f <- features[[i]]
    geom <- f$geometry
    coords <- flatten_coords(geom$coordinates)
    list(
      coords = coords,
      props = f$properties,
      color = colors(i),
      id = i  # store feature index for layerId
    )
  })
  
  output$map <- renderLeaflet({
    m <- leaflet() %>% addTiles()
    
    for (p in poly_list) {
      for (mat in p$coords) {
        m <- m %>% addPolygons(
          lng = mat[, "lng"],
          lat = mat[, "lat"],
          color = p$color,
          weight = 2,
          fillOpacity = 1,   # fully opaque
          layerId = as.character(p$id),
          label = p$props$LongName
        )
      }
    }
    
    m
  })
  
  output$feature_info <- renderPrint({
    click <- input$map_shape_click
    if (is.null(click)) return("Click on a polygon")
    
    # retrieve properties using feature index
    idx <- as.numeric(click$id)
    if (is.na(idx) || idx < 1 || idx > length(features)) return("Invalid feature")
    f <- features[[idx]]$properties
    
    paste0(
      paste0(names(f), ": ", f, collapse = "\n")
    )
  })
}

shinyApp(ui, server)
