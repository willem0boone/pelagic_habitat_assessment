library(shiny)
library(dplyr)
library(tidyr)
library(data.table)
library(openxlsx)

tool1_analysis_PLET_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("PLET Analysis Results"),
    verbatimTextOutput(ns("summary_text")),
    tableOutput(ns("pi_table")),
    downloadButton(ns("download_results"), "Download Excel Results")
  )
}

tool1_analysis_PLET_server <- function(id, parquet_filtered_data, output_dir = tempdir()) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ---- Reactive to hold processed results ----
    results <- reactive({
      df <- parquet_filtered_data()  # df already filtered by region
      
      req(nrow(df) > 0)
      
      # ----- Mimic steps from your PLET script -----
      # 1. Clean & reshape
      id_vars <- c("polygon_wkt", "period", "num_samples")
      id_vars <- id_vars[id_vars %in% colnames(df)]
      lf_list <- colnames(df)[!(colnames(df) %in% id_vars)]
      
      # Convert to numeric & handle NAs
      df[lf_list] <- suppressWarnings(sapply(df[lf_list], as.numeric))
      df <- df[, colSums(is.na(df)) < nrow(df)]
      
      df[rowSums(df[, lf_list], na.rm = TRUE) > 0, lf_list][
        is.na(df[rowSums(df[, lf_list], na.rm = TRUE) > 0, lf_list])
      ] <- 0
      
      # Long format
      df_long <- df %>% pivot_longer(-all_of(id_vars), names_to = "lifeform", values_to = "abundance")
      
      # Split period into year/month
      dates <- tstrsplit(df_long$period, "-", fixed = TRUE)
      df_long <- cbind(data.frame(year = as.numeric(dates[[1]]),
                                  month = as.numeric(dates[[2]])), df_long) %>%
        dplyr::select(-period)
      
      # Placeholder: lifeform pairs indicator calculation
      pi_results <- df_long %>%
        group_by(lifeform, year) %>%
        summarise(mean_abundance = mean(abundance, na.rm = TRUE), .groups = "drop")
      
      list(df_long = df_long, pi_results = pi_results)
    })
    
    # ---- Render summary ----
    output$summary_text <- renderPrint({
      df_long <- results()$df_long
      cat("Rows in filtered dataset:", nrow(df_long), "\n")
      cat("Lifeforms detected:", paste(unique(df_long$lifeform), collapse = ", "))
    })
    
    output$pi_table <- renderTable({
      results()$pi_results
    })
    
    # ---- Download results ----
    output$download_results <- downloadHandler(
      filename = function() { paste0("PLET_results.xlsx") },
      content = function(file) {
        list_of_datasets <- list(
          "PI_results" = results()$pi_results,
          "Data_long" = results()$df_long
        )
        openxlsx::write.xlsx(list_of_datasets, file = file, overwrite = TRUE)
      }
    )
    
  })
}
