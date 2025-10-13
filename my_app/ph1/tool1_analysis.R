# ph1/tool1_analysis.R

tool1_analysis_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("lf_select_ui")),
        shinyWidgets::sliderTextInput(
          inputId = ns("ref_slider"),
          label = HTML("<span style='color:red;'>Reference Period</span>"),
          choices = as.character(2010:2025),
          selected = c("2015", "2018"),
          grid = TRUE
        ),
        shinyWidgets::sliderTextInput(
          inputId = ns("comp_slider"),
          label = HTML("<span style='color:blue;'>Comparison Period</span>"),
          choices = as.character(2010:2025),
          selected = c("2019", "2024"),
          grid = TRUE
        ),
        hr(),
        actionButton(ns("run_analysis"), "Run Analysis", class = "btn-primary"),
        hr(),
        downloadButton(ns("download_results"), "Download Results")
      ),
      mainPanel(
        textOutput(ns("status")),
        plotOutput(ns("envPlot"), height = "500px"),
        plotOutput(ns("tsPlot1"), height = "400px"),
        plotOutput(ns("tsPlot2"), height = "400px")
      )
    )
  )
}

tool1_analysis_server <- function(input, output, session, analysis_results) {
  ns <- session$ns
  
  # --- lifeform selectors ---
  output$lf_select_ui <- renderUI({
    req(analysis_results$df)
    lf_unique <- unique(analysis_results$df$lifeform)
    tagList(
      selectInput(ns("lf1"), "Lifeform 1", choices = lf_unique, selected = lf_unique[1]),
      selectInput(ns("lf2"), "Lifeform 2", choices = lf_unique[lf_unique != lf_unique[1]], selected = lf_unique[2])
    )
  })
  
  # --- storage for results ---
  local_results <- reactiveValues(
    env_plot = NULL,
    ts_plot1 = NULL,
    ts_plot2 = NULL,
    list_of_datasets = NULL
  )
  
  # --- Run full analysis when button pressed ---
  observeEvent(input$run_analysis, {
    req(analysis_results$df)
    df <- analysis_results$df
    output$status <- renderText("Processing analysis...")
    
    # --- prepare date fields ---
    dates <- read.table(text = as.character(df$period), sep = "-", stringsAsFactors = FALSE)
    colnames(dates) <- c("year", "month")
    df <- cbind(dates, df) %>% dplyr::select(-period)
    
    # --- filter by years ---
    ref_years <- as.numeric(input$ref_slider)
    comp_years <- as.numeric(input$comp_slider)
    
    df <- df %>%
      dplyr::filter(year >= min(c(ref_years, comp_years)),
                    year <= max(c(ref_years, comp_years)))
    
    # --- create assessment ids ---
    assess_list <- create_assess_id(x = df)
    df <- assess_list[[1]]
    df_assess_id <- assess_list[[2]]
    
    # --- polygon maps, transforms, gaps ---
    polygon_maps <- plot_polys(x = df_assess_id, buff = 2)
    df <- log_transform(x = df, method = 1)
    df <- clean_years(x = df, thr = 8)
    df <- fill_gaps(x = df, max_gap = 3)
    
    # --- subset lifeforms ---
    selected_lfs <- c(input$lf1, input$lf2)
    df_ref <- dataSelect(x = df, lf = selected_lfs, lims = ref_years)
    df_comp <- dataSelect(x = df, lf = selected_lfs, lims = comp_years)
    
    # --- lifeform pair table ---
    lf_pairs <- data.frame(V1 = input$lf1, V2 = input$lf2)
    
    # --- PI envelope and calculations ---
    envAll <- find_envAll(x = df_ref, lf = lf_pairs)
    piResults <- PIcalcAll(x = envAll, y = df_comp, z = df_ref, lf = lf_pairs)
    piResultsAnnual <- suppressWarnings(
      PIcalcAnnual(x = envAll, y = df_comp, z = df_ref, lf = lf_pairs)
    )
    
    # --- plots ---
    env_plots <- plot_env(x = envAll, y = df_ref, z = df_comp, lf = lf_pairs, pi = piResults)
    df_fits_tot <- kendallAll(x = df, seasonal = FALSE)
    df_plot <- create_ts(x = df, y = df_fits_tot)
    
    df_plot1 <- df_plot %>% filter(lifeform == input$lf1)
    df_plot2 <- df_plot %>% filter(lifeform == input$lf2)
    ts_plot1 <- plot_ts(df_plot1)
    ts_plot2 <- plot_ts(df_plot2)
    
    # --- save locally for rendering/download ---
    local_results$env_plot <- env_plots
    local_results$ts_plot1 <- ts_plot1
    local_results$ts_plot2 <- ts_plot2
    
    if (nrow(df_assess_id) > 1) {
      local_results$list_of_datasets <- list(
        "Kendall_results" = df_fits_tot,
        "PI_results" = piResults,
        "PI_annual_results" = piResultsAnnual,
        "Assessment_ids" = df_assess_id
      )
    } else {
      local_results$list_of_datasets <- list(
        "Kendall_results" = df_fits_tot,
        "PI_results" = piResults,
        "PI_annual_results" = piResultsAnnual
      )
    }
    
    output$status <- renderText("✅ Analysis completed successfully!")
  })
  
  # --- Render plots ---
  output$envPlot <- renderPlot({ req(local_results$env_plot); local_results$env_plot })
  output$tsPlot1 <- renderPlot({ req(local_results$ts_plot1); local_results$ts_plot1 })
  output$tsPlot2 <- renderPlot({ req(local_results$ts_plot2); local_results$ts_plot2 })
  
  # --- Download handler ---
  output$download_results <- downloadHandler(
    filename = function() paste0("PI_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(local_results$list_of_datasets)
      openxlsx::write.xlsx(local_results$list_of_datasets, file = file, overwrite = TRUE)
    }
  )
}
