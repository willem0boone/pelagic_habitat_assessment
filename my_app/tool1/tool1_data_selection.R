library(shiny)
library(data.table)
library(janitor)
library(dplyr)

# ---- UI ----
tool1_data_selection_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("selection_ui"))
}

# ---- Server ----
tool1_data_selection_server <- function(id, analysis_results, selected_data_page, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reset function ---
    reset_all <- function() {
      # Clear memory
      analysis_results$df <- NULL
      analysis_results$region <- NULL
      
      # Safely clear map outputs if they exist
      if ("selected_region_info" %in% names(output)) output$selected_region_info <- renderPrint(NULL)
      if ("region_data_summary" %in% names(output)) output$region_data_summary <- renderTable(NULL)
      if ("proceed_btn_ui" %in% names(output)) output$proceed_btn_ui <- renderUI(NULL)
      
      # Clear CSV preview
      if ("csv_preview" %in% names(output)) output$csv_preview <- renderTable(NULL)
    }
    
    # --- Render selection UI ---
    output$selection_ui <- renderUI({
      page <- selected_data_page()
      
      if (is.null(page)) {
        tagList(
          h3("Select Data Source"),
          actionButton(ns("btn_csv"), "Upload CSV", class = "btn-primary"),
          actionButton(ns("btn_map"), "Select on Map", class = "btn-info")
        )
      } else if (page == "csv") {
        tagList(
          fileInput(ns("csvfile"), "Upload CSV File", accept = ".csv"),
          tableOutput(ns("csv_preview")),
          br(),
          actionButton(ns("next_btn"), "Next", class = "btn-success"),
          actionButton(ns("restart_btn"), "Restart", class = "btn-warning")
        )
      } else if (page == "map") {
        tagList(
          div(id = ns("map1_container"),
              tool1_data_map_ui(ns("map1"))
          ),
          actionButton(ns("restart_btn"), "Restart", class = "btn-warning")
        )
      }
    })
    
    # --- Page switching buttons ---
    observeEvent(input$btn_csv, { 
      reset_all()
      selected_data_page("csv") 
    })
    
    observeEvent(input$btn_map, { 
      reset_all()
      selected_data_page("map") 
    })
    
    # --- Initialize map module when "map" page selected ---
    observeEvent(selected_data_page(), {
      if (selected_data_page() == "map") {
        analysis_results$region <- NULL  # ensure region cleared
        tool1_data_map_server(
          id = "map1",
          analysis_results = analysis_results,
          parent_session = parent_session
        )
      }
    })
    
    # --- Restart button ---
    observeEvent(input$restart_btn, {
      reset_all()
      selected_data_page(NULL)
    })
    
    # --- CSV upload QC ---
    observeEvent(input$csvfile, {
      req(input$csvfile)
      df <- data.table::fread(input$csvfile$datapath) %>%
        janitor::clean_names() %>% as.data.frame()
      
      qc_msg <- c()
      if (!"period" %in% names(df)) qc_msg <- c(qc_msg, "❌ Missing 'period'")
      if (!"lifeform" %in% names(df)) qc_msg <- c(qc_msg, "❌ Missing 'lifeform'")
      if (all(c("period","lifeform") %in% names(df))) {
        dup_rows <- df %>% dplyr::group_by(period, lifeform) %>% dplyr::filter(dplyr::n() > 1)
        if (nrow(dup_rows) > 0) qc_msg <- c(qc_msg, "❌ Duplicate period+lifeform")
      }
      
      if (length(qc_msg) == 0) {
        qc_msg <- "✅ File OK"
        analysis_results$df <- df
      } else {
        analysis_results$df <- NULL
      }
      
      output$csv_preview <- renderTable(head(df, 6))
      showNotification(paste(qc_msg, collapse = "\n"), type = "message")
    })
    
    # --- Next button ---
    observeEvent(input$next_btn, {
      req(analysis_results$df)
      updateTabsetPanel(parent_session, "mainTabs", selected = "analysisTab")
    })
    
  })
}
