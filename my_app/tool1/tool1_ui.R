output$main_ui <- renderUI({
  page <- current_step()
  
  if (page == "data_selection") {
    tool1_data_selection_ui(session$ns("data_selection"))
  } else if (page == "analysis") {
    tool1_analysis_PLET_ui(session$ns("analysis_PLET"))
  }
})
