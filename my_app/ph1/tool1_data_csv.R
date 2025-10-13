# CSV Upload Module

tool1_data_csv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("csvfile"), "Upload Lifeform CSV", accept = ".csv"),
    hr(),
    textOutput(ns("qc_status")),
    uiOutput(ns("proceed_btn_ui")),
    hr(),
    h5("Data Preview"),
    tableOutput(ns("data_head")),
    hr(),
    h5("Summary by Lifeform"),
    tableOutput(ns("data_summary"))
  )
}

tool1_data_csv_server <- function(input, output, session, analysis_results, parent_session) {
  ns <- session$ns
  
  observeEvent(input$csvfile, {
    req(input$csvfile)
    df <- fread(input$csvfile$datapath) %>% clean_names() %>% as.data.frame()
    
    qc_msg <- c()
    if (!"period" %in% names(df)) qc_msg <- c(qc_msg, "❌ Missing 'period' column.")
    if (!"lifeform" %in% names(df)) qc_msg <- c(qc_msg, "❌ Missing 'lifeform' column.")
    if ("period" %in% names(df) && "lifeform" %in% names(df)) {
      dup_rows <- df %>% group_by(period, lifeform) %>% filter(n() > 1)
      if (nrow(dup_rows) > 0) qc_msg <- c(qc_msg, "❌ Duplicate period + lifeform combinations detected.")
    }
    
    if (length(qc_msg) == 0) {
      qc_msg <- c("✅ File successfully loaded and passed QC checks.")
      analysis_results$df <- df
      analysis_results$ready <- TRUE
    } else {
      analysis_results$ready <- FALSE
    }
    
    output$qc_status <- renderText({ paste(qc_msg, collapse = "\n") })
    output$data_head <- renderTable({ head(df, 10) })
    output$data_summary <- renderTable({
      if ("lifeform" %in% names(df)) {
        df %>%
          group_by(lifeform) %>%
          summarise(
            n_records = n(),
            first_period = min(period, na.rm = TRUE),
            last_period = max(period, na.rm = TRUE)
          )
      } else NULL
    })
    
    output$proceed_btn_ui <- renderUI({
      if (analysis_results$ready) actionButton(ns("proceed_btn"), "Proceed to Analysis", class = "btn-success")
    })
  })
  
  observeEvent(input$proceed_btn, {
    updateTabsetPanel(parent_session, "mainTabs", selected = "analysisTab")
  })
}
