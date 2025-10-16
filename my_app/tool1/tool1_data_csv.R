# tool1_data_csv.R
library(shiny)
library(data.table)
library(dplyr)
library(janitor)
library(shinyjs)

# ---- UI ----
tool1_data_csv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload CSV Page"),
    fileInput(ns("file"), "Choose CSV File", accept = ".csv"),
    textOutput(ns("qc_status")),
    tableOutput(ns("preview")),
    div(style = "margin-top:20px;",
        actionButton(ns("next_btn"), "Next", class = "btn-success"),
        actionButton(ns("restart_btn"), "Restart", class = "btn-warning"))
  )
}

# ---- Server ----
tool1_data_csv_server <- function(input, output, session, analysis_results, on_next) {
  ns <- session$ns
  
  valid_csv <- reactiveVal(FALSE)  # track if CSV passed QC
  
  # Observe file upload
  observeEvent(input$file, {
    req(input$file)
    
    # Read CSV with data.table + clean names
    df <- fread(input$file$datapath) %>% clean_names() %>% as.data.frame()
    
    qc_msg <- c()
    
    # QC checks
    if (!"period" %in% names(df)) qc_msg <- c(qc_msg, "❌ Missing 'period' column.")
    if (!"lifeform" %in% names(df)) qc_msg <- c(qc_msg, "❌ Missing 'lifeform' column.")
    if ("period" %in% names(df) && "lifeform" %in% names(df)) {
      dup_rows <- df %>% group_by(period, lifeform) %>% filter(n() > 1)
      if (nrow(dup_rows) > 0) qc_msg <- c(qc_msg, "❌ Duplicate period + lifeform combinations detected.")
    }
    
    if (length(qc_msg) == 0) {
      qc_msg <- "✅ File successfully loaded and passed QC checks."
      analysis_results$df <- df
      valid_csv(TRUE)
    } else {
      analysis_results$df <- NULL
      valid_csv(FALSE)
    }
    
    # Show QC messages
    output$qc_status <- renderText({ paste(qc_msg, collapse = "\n") })
    
    # Show preview
    output$preview <- renderTable({
      if (valid_csv()) head(df, 6) else NULL
    })
  })
  
  # Reset preview and QC when CSV is cleared
  observe({
    if (is.null(analysis_results$df)) {
      output$preview <- renderTable(NULL)
      output$qc_status <- renderText("")
      valid_csv(FALSE)
    }
  })
  
  # Next button only works if CSV is valid
  observeEvent(input$next_btn, {
    req(valid_csv())
    if (!is.null(on_next)) on_next()
  })
  
  # Restart button clears stored data
  observeEvent(input$restart_btn, {
    analysis_results$df <- NULL
    valid_csv(FALSE)
  })
}
