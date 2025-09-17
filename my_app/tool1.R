library(shiny)
library(shinyjs)
library(tidyverse)
library(data.table)
library(janitor)
library(pracma)
library(broom)
library(EnvStats)
library(patchwork)
library(rnaturalearth)
library(zoo)
library(openxlsx)

# Load supporting functions
source("Supporting_scripts/PI_functions_v1.R")
source("Supporting_scripts/Supporting_functions_v2.R")

# ----------------------------------
# UI for tool1
# ----------------------------------
tool1_ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      fileInput("csvfile", "Upload Lifeform CSV", accept = ".csv"),
      selectInput("lf1", "Lifeform 1", choices = NULL),
      selectInput("lf2", "Lifeform 2", choices = NULL),
      numericInput("ref_start", "Reference start year", value = 2015),
      numericInput("ref_end", "Reference end year", value = 2018),
      numericInput("comp_start", "Comparison start year", value = 2018),
      numericInput("comp_end", "Comparison end year", value = 2025),
      actionButton("run_analysis", "Run Analysis"),
      hr(),
      textOutput("warning"),
      downloadButton("download_results", "Download Excel Results")
    ),
    mainPanel(
      textOutput("status"),
      plotOutput("envPlot", height = "500px"),
      plotOutput("tsPlot1", height = "400px"),
      plotOutput("tsPlot2", height = "400px")
    )
  )
)

# ----------------------------------
# Server logic for tool1
# ----------------------------------
tool1_server <- function(input, output, session) {
  
  analysis_results <- reactiveValues(
    env_plot = NULL,
    ts_plot1 = NULL,
    ts_plot2 = NULL,
    list_of_datasets = NULL
  )
  
  # Reactive dataset
  df_data <- reactive({
    req(input$csvfile)
    df <- fread(input$csvfile$datapath) %>% clean_names() %>% as.data.frame()
    
    # Check duplicate period + lifeform
    dup_rows <- df %>% group_by(period, lifeform) %>% filter(n() > 1)
    if(nrow(dup_rows) > 0){
      output$warning <- renderText("Error: Duplicate period + lifeform combinations detected!")
      shinyjs::disable("run_analysis")
      return(NULL)
    }
    
    # Must have at least 2 lifeforms
    lf_unique <- unique(df$lifeform)
    if(length(lf_unique) < 2){
      output$warning <- renderText("Error: Need at least two lifeforms for analysis.")
      shinyjs::disable("run_analysis")
      return(NULL)
    }
    
    output$warning <- renderText("")
    shinyjs::enable("run_analysis")
    df
  })
  
  # Populate lifeform dropdowns when CSV is loaded
  observeEvent(input$csvfile, {
    df <- df_data()
    req(df)
    lf_unique <- unique(df$lifeform)
    updateSelectInput(session, "lf1", choices = lf_unique, selected = lf_unique[1])
    updateSelectInput(session, "lf2", choices = lf_unique[lf_unique != lf_unique[1]], selected = lf_unique[2])
  })
  
  # Update Lifeform 2 choices if Lifeform 1 changes
  observeEvent(input$lf1, {
    req(input$lf1)
    df <- df_data()
    req(df)
    lf_unique <- unique(df$lifeform)
    updateSelectInput(session, "lf2", choices = lf_unique[lf_unique != input$lf1])
  })
  
  # Run analysis
  observeEvent(input$run_analysis, {
    req(df_data())
    df <- df_data()
    output$status <- renderText({"Processing analysis..."})
    
    # Extract year/month
    dates <- read.table(text = as.character(df$period), sep="-", stringsAsFactors = FALSE)
    colnames(dates) <- c("year", "month")
    df <- cbind(dates, df) %>% select(-period)
    
    # Filter by input years
    ref_years <- c(input$ref_start, input$ref_end)
    comp_years <- c(input$comp_start, input$comp_end)
    df <- df %>% filter(year >= min(c(ref_years, comp_years)),
                        year <= max(c(ref_years, comp_years)))
    
    # Assessment IDs
    assess_list <- create_assess_id(x=df)
    df <- assess_list[[1]]
    df_assess_id <- assess_list[[2]]
    
    # Polygon maps
    polygon_maps <- plot_polys(x=df_assess_id, buff=2)
    
    # Log transform
    df <- log_transform(x=df, method=1)
    
    # Clean and fill gaps
    df <- clean_years(x=df, thr=8)
    df <- fill_gaps(x=df, max_gap=3)
    
    # Subset for selected lifeforms
    selected_lfs <- c(input$lf1, input$lf2)
    df_ref <- dataSelect(x=df, lf=selected_lfs, lims=ref_years)
    df_comp <- dataSelect(x=df, lf=selected_lfs, lims=comp_years)
    
    # Lifeform pair table
    lf_pairs <- data.frame(V1=input$lf1, V2=input$lf2)
    
    # Reference envelopes
    envAll <- find_envAll(x=df_ref, lf=lf_pairs)
    
    # PI calculations
    piResults <- PIcalcAll(x=envAll, y=df_comp, z=df_ref, lf=lf_pairs)
    piResultsAnnual <- suppressWarnings(
      PIcalcAnnual(x=envAll, y=df_comp, z=df_ref, lf=lf_pairs)
    )
    
    # Plots
    env_plots <- plot_env(x=envAll, y=df_ref, z=df_comp, lf=lf_pairs, pi=piResults)
    df_fits_tot <- kendallAll(x=df, seasonal=FALSE)
    df_plot <- create_ts(x=df, y=df_fits_tot)
    
    # Temporal plots for each lifeform
    df_plot1 <- df_plot %>% filter(lifeform == input$lf1)
    df_plot2 <- df_plot %>% filter(lifeform == input$lf2)
    ts_plot1 <- plot_ts(df_plot1)
    ts_plot2 <- plot_ts(df_plot2)
    
    # Save to reactiveValues
    analysis_results$env_plot <- env_plots
    analysis_results$ts_plot1 <- ts_plot1
    analysis_results$ts_plot2 <- ts_plot2
    
    # Save datasets for download
    if(nrow(df_assess_id) > 1){
      analysis_results$list_of_datasets <- list(
        "Kendall_results" = df_fits_tot,
        "PI_results" = piResults,
        "PI_annual_results" = piResultsAnnual,
        "Assessment_ids" = df_assess_id
      )
    } else {
      analysis_results$list_of_datasets <- list(
        "Kendall_results" = df_fits_tot,
        "PI_results" = piResults,
        "PI_annual_results" = piResultsAnnual
      )
    }
    
    output$status <- renderText({"Analysis completed successfully!"})
  })
  
  # Render plots
  output$envPlot <- renderPlot({
    req(analysis_results$env_plot)
    analysis_results$env_plot
  })
  
  output$tsPlot1 <- renderPlot({
    req(analysis_results$ts_plot1)
    analysis_results$ts_plot1
  })
  
  output$tsPlot2 <- renderPlot({
    req(analysis_results$ts_plot2)
    analysis_results$ts_plot2
  })
  
  # Download Excel
  output$download_results <- downloadHandler(
    filename = function(){ paste0("PI_results.xlsx") },
    content = function(file){
      req(analysis_results$list_of_datasets)
      openxlsx::write.xlsx(analysis_results$list_of_datasets, file = file, overwrite = TRUE)
    }
  )
}
