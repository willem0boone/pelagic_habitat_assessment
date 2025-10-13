source("ph1/tool1_data_csv.R")
source("ph1/tool1_data_dto.R")


tool1_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioGroupButtons(
      inputId = ns("input_mode"),
      label = "Select Input Method",
      choices = c("Upload CSV", "Select Region"),
      justified = TRUE,
      status = "primary"
    ),
    uiOutput(ns("data_input_ui"))
  )
}

tool1_server <- function(input, output, session, analysis_results) {
  ns <- session$ns
  
  output$data_input_ui <- renderUI({
    req(input$input_mode)
    if (input$input_mode == "Upload CSV") {
      tool1_data_csv_ui(ns("csv"))
    } else {
      tool1_data_dto_ui(ns("dto"))
    }
  })
  
  
  callModule(tool1_data_csv_server, "csv", analysis_results = analysis_results, parent_session = session)
  callModule(tool1_data_dto_server, "dto", analysis_results = analysis_results, parent_session = session)
  
}
