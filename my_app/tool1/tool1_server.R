library(shiny)

source("tool1/tool1_data_map.R")
source("tool1/tool1_data_selection.R")
source("tool1/tool1_analysis.R")      # CSV analysis
source("tool1/tool1_analysis_PLET.R") # PLET/map analysis

# ---- UI ----
tool1_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main_ui"))
}

# ---- Server ----
tool1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    current_step <- reactiveVal("data_selection")
    selected_data_page <- reactiveVal(NULL)
    analysis_results <- reactiveValues(df = NULL, region = NULL)
    
    # Render main UI
    output$main_ui <- renderUI({
      if (current_step() == "data_selection") {
        tool1_data_selection_ui(ns("data_selection"))
      } else if (current_step() == "analysis") {
        tool1_analysis_ui(ns("analysis"))
      }
    })
    
    # Call selection module
    tool1_data_selection_server(
      id = "data_selection",
      analysis_results = analysis_results,
      selected_data_page = selected_data_page,
      parent_session = session
    )
    
    # Call analysis module (CSV)
    tool1_analysis_server(
      id = "analysis",
      analysis_results = analysis_results,
      on_previous = function() current_step("data_selection")
    )
  })
}
