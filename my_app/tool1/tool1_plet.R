# ==========================================================
# tool1/tool1_plet.R — PLET workflow module (integrated)
# ==========================================================

library(shiny)
library(leaflet)
library(sf)
library(arrow)
library(dplyr)
library(shinycssloaders)
library(magrittr)
library(fs)
library(openxlsx)
library(data.table)

# ==========================================================
# Logging setup (all logs go to tool1_log.txt)
# ==========================================================
tool1_log_dir <- "tool1/logs"
if (!dir.exists(tool1_log_dir)) dir.create(tool1_log_dir, recursive = TRUE)
log_file <- file.path(tool1_log_dir, "tool1_log.txt")

log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] [Tool1] ", msg, "\n"), file = log_file, append = TRUE)
}

# ==========================================================
# Lifeform choices
# ==========================================================
lifeform_choices <- c(
  "auto_and_mix_dinos","carniv_zoo","ciliates","copepods","crustacean","diatom","dinoflagellate",
  "fish_larvae","gelatinous","holoplankton","lg_copepods","lg_phyto","meroplankton",
  "non_carniv_zoo","pelagic_diatoms","phytoplankton","potentially_toxic/nuisance_diatoms",
  "potentially_toxic/nuisance_dinos","sm_copepods","sm_phyto","tychopelagic_diatoms"
)

# ==========================================================
# UI
# ==========================================================
tool1PletUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 6,
        h4("OSPAR Common Procedure Assessment Units"),
        tags$p(
          "Select a region on the map to load available datasets ",
          tags$a(
            href = "https://odims.ospar.org/en/submissions/ospar_comp_au_2023_01/",
            target = "_blank",
            "from ODIMS."
          )
        ),
        leafletOutput(ns("map"), height = "520px") %>% withSpinner(),
        br(),
        # Dataset & lifeform UI
        uiOutput(ns("dataset_ui_container")),
        uiOutput(ns("lifeform_ui")),
        br()
      ),
      column(
        width = 6,
        h4("Analysis & Preview"),
        uiOutput(ns("dataset_preview_container")),
        br(),
        actionButton(ns("reset_workflow"), "Reset PLET Workflow", class = "btn btn-warning")
      )
    ),
    br(),
    div(
      class = "text-center",
      actionButton(ns("return_main"), "Return to main menu", class = "btn btn-primary")
    )
  )
}

# ==========================================================
# SERVER
# ==========================================================
tool1PletServer <- function(id, global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      features = NULL,
      selected_ID = NULL,
      datasets = character(0)
    )
    
    selected_dataset <- reactiveVal(NULL)
    
    # ==========================================================
    # Load OSPAR WFS features
    # ==========================================================
    observe({
      wfs_url <- "https://odims.ospar.org/geoserver/odims/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=ospar_comp_au_2023_01_001&outputFormat=application/json"
      log_message("Loading WFS features...")
      rv$features <- tryCatch(
        sf::st_read(wfs_url, quiet = TRUE),
        error = function(e) { log_message(paste("Failed to load WFS:", conditionMessage(e))); NULL }
      )
      if (!is.null(rv$features) && !"ID" %in% names(rv$features)) {
        rv$features$ID <- seq_len(nrow(rv$features))
        log_message("Added fallback numeric ID column (WFS lacked 'ID').")
      }
      if (!is.null(rv$features)) log_message("Loaded OSPAR features successfully.")
    })
    
    # ==========================================================
    # Connect to Parquet dataset
    # ==========================================================
    arrow_ds_ok <- reactiveVal(FALSE)
    ds_data <- reactiveVal(NULL)
    
    observe({
      tryCatch({
        s3_fs <- arrow::s3_bucket(
          bucket = "oidc-willemboone",
          endpoint_override = "https://minio.dive.edito.eu"
        )
        ds <- open_dataset("data/merged-data3.parquet", filesystem = s3_fs)
        ds_data(ds)
        arrow_ds_ok(TRUE)
        log_message("Connected to Parquet dataset.")
      }, error = function(e) {
        log_message(paste("Failed to connect to parquet:", conditionMessage(e)))
        arrow_ds_ok(FALSE)
      })
    })
    
    # ==========================================================
    # Render map
    # ==========================================================
    output$map <- renderLeaflet({
      req(rv$features)
      leaflet(rv$features) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          layerId = ~as.character(ID),
          color = "blue", weight = 2, fillOpacity = 0.25,
          highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
          label = ~as.character(ID)
        )
    })
    
    # ==========================================================
    # Polygon click
    # ==========================================================
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id)
      
      # Reset previous selection
      rv$selected_ID <- click$id
      rv$datasets <- character(0)
      selected_dataset(NULL)
      output$dataset_ui_container <- renderUI({ NULL })
      output$lifeform_ui <- renderUI({ NULL })
      output$dataset_preview_container <- renderUI({ NULL })
      
      log_message(paste("Region selected:", rv$selected_ID))
      
      # Highlight selected polygon
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(
          data = rv$features,
          layerId = ~as.character(ID),
          color = ~ifelse(ID == rv$selected_ID, "red", "blue"),
          weight = 2,
          fillOpacity = ~ifelse(ID == rv$selected_ID, 0.5, 0.25),
          highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
          label = ~as.character(ID)
        )
      
      # Load datasets for selected region
      if (isTRUE(arrow_ds_ok())) {
        ds_names <- tryCatch({
          ds_data() %>%
            filter(region_id == rv$selected_ID) %>%
            select(dataset_name) %>%
            distinct() %>%
            collect() %>%
            pull(dataset_name)
        }, error = function(e) { log_message(paste("Error loading dataset names:", conditionMessage(e))); character(0) })
        rv$datasets <- ds_names
        
        log_message(paste("Available datasets for region", rv$selected_ID, ":", paste(rv$datasets, collapse = ", ")))
      }
      
      # Show dataset dropdown only if datasets exist
      if (length(rv$datasets) > 0) {
        output$dataset_ui_container <- renderUI({
          tagList(
            h5("Select dataset for this region:"),
            selectInput(ns("dataset_select"),
                        "Dataset:",
                        choices = c(" " = "", rv$datasets),
                        selected = "")
          )
        })
      }
    })
    
    # ==========================================================
    # Dataset & lifeform selection
    # ==========================================================
    observeEvent(input$dataset_select, {
      ds <- input$dataset_select
      selected_dataset(ds)
      if (is.null(ds) || ds == "") {
        output$lifeform_ui <- renderUI({ NULL })
        return()
      }
      
      log_message(paste("Dataset selected:", ds))
      
      output$lifeform_ui <- renderUI({
        tagList(
          selectInput(ns("lifeform1"), "Lifeform 1:", choices = lifeform_choices, selected = lifeform_choices[1]),
          selectInput(ns("lifeform2"), "Lifeform 2:", choices = lifeform_choices[-1], selected = lifeform_choices[2]),
          sliderInput(ns("ref_years"), "Reference period:", min = 1960, max = 2030, value = c(2015, 2019)),
          sliderInput(ns("comp_years"), "Comparison period:", min = 1960, max = 2030, value = c(1960, 2014)),
          actionButton(ns("run_analysis"), "Run Analysis", class = "btn btn-primary mt-2")
        )
      })
    })
    
    # Prevent selecting same lifeform twice
    observeEvent(input$lifeform1, {
      lf1 <- input$lifeform1
      updateSelectInput(session, "lifeform2",
                        choices = setdiff(lifeform_choices, lf1),
                        selected = ifelse(input$lifeform2 == lf1, setdiff(lifeform_choices, lf1)[1], input$lifeform2))
    })
    
    # ==========================================================
    # Run Analysis
    # ==========================================================
    observeEvent(input$run_analysis, {
      req(selected_dataset(), input$lifeform1, input$lifeform2, input$ref_years, input$comp_years)
      log_message(paste("Running analysis for dataset", selected_dataset(),
                        "Lifeform1:", input$lifeform1, "Lifeform2:", input$lifeform2,
                        "Reference period:", paste(input$ref_years, collapse="-"),
                        "Comparison period:", paste(input$comp_years, collapse="-")))
      
      # Pull dataset locally
      df <- ds_data() %>%
        collect() %>%
        filter(region_id == rv$selected_ID, dataset_name == selected_dataset())
      
      # Data cleaning / long format
      id_vars <- c("polygon_wkt", "period", "num_samples")
      id_vars <- id_vars[id_vars %in% colnames(df)]
      lf_list <- colnames(df)[!(colnames(df) %in% id_vars)]
      df[lf_list] <- suppressWarnings(sapply(df[lf_list], as.numeric))
      df <- df[, colSums(is.na(df)) < nrow(df)]
      df[rowSums(df[, lf_list], na.rm=TRUE) > 0, lf_list][
        is.na(df[rowSums(df[, lf_list], na.rm=TRUE) > 0, lf_list])
      ] <- 0
      df <- df %>%
        pivot_longer(-all_of(id_vars), names_to="lifeform", values_to="abundance") %>%
        mutate(abundance = as.numeric(abundance))
      dates <- tstrsplit(df$period, "-", fixed=TRUE)
      df <- cbind(data.frame(year = as.numeric(dates[[1]]), month = as.numeric(dates[[2]])), df) %>%
        select(-period)
      
      # Subset by reference + comparison period
      ref_years <- input$ref_years
      comp_years <- input$comp_years
      df <- df %>% filter(year >= min(c(ref_years, comp_years)), year <= max(c(ref_years, comp_years)))
      
      # Lifeform pair setup
      df_lf_user <- rbind(data.frame(V1 = input$lifeform1, V2 = input$lifeform2))
      df_lf <- rbind(df_lf, df_lf_user)  # combine with default pairs
      
      # Analysis functions
      assess_list <- create_assess_id(df)
      df <- assess_list[[1]]
      df_assess_id <- assess_list[[2]]
      
      polygon_maps <- plot_polys(df_assess_id, buff=2)
      df <- log_transform(df, method=1)
      df <- clean_years(df, thr=8)
      df <- fill_gaps(df, max_gap=3)
      
      df_ref <- dataSelect(df, lf=df_lf, lims=ref_years)
      df_comp <- dataSelect(df, lf=df_lf, lims=comp_years)
      df_ref <- qc_ref(df_ref, ind_years=3, ind_months=30, rep_months=2)
      
      envAll <- find_envAll(df_ref, lf=df_lf)
      piResults <- PIcalcAll(envAll, df_comp, df_ref, lf=df_lf)
      piResultsAnnual <- PIcalcAnnual(envAll, df_comp, df_ref, lf=df_lf)
      df_fits_tot <- kendallAll(df, seasonal=FALSE)
      df_plot <- create_ts(df, df_fits_tot)
      ts_plots <- plot_ts(df_plot)
      env_plots <- plot_env(envAll, df_ref, df_comp, df_lf, piResults)
      
      # Render outputs
      output$dataset_preview_container <- renderUI({
        tagList(
          h5("Dataset preview (first 10 rows)"), verbatimTextOutput(ns("dataset_preview")),
          h5("Lifeform Pairs Indicator Summary"), verbatimTextOutput(ns("summary_text")),
          h5("PI Table"), tableOutput(ns("pi_table")),
          downloadButton(ns("download_results"), "Download Results")
        )
      })
      output$dataset_preview <- renderPrint({ head(df, 10) })
      output$summary_text <- renderPrint({
        cat("Rows in filtered dataset:", nrow(df), "\n")
        cat("Lifeforms detected:", paste(unique(df$lifeform), collapse=", "))
      })
      output$pi_table <- renderTable({ piResults })
      
      output$download_results <- downloadHandler(
        filename = function() { paste0("PH1_results.xlsx") },
        content = function(file) {
          list_of_datasets <- if(nrow(df_assess_id) > 1){
            list("Kendall_results" = df_fits_tot, "PI_results" = piResults, "PI_annual_results" = piResultsAnnual, "Assessment_ids" = df_assess_id)
          } else {
            list("Kendall_results" = df_fits_tot, "PI_results" = piResults, "PI_annual_results" = piResultsAnnual)
          }
          openxlsx::write.xlsx(list_of_datasets, file, overwrite=TRUE)
        }
      )
    })
    
    # ==========================================================
    # Reset workflow
    # ==========================================================
    reset <- function() {
      rv$selected_ID <- NULL
      rv$datasets <- character(0)
      selected_dataset(NULL)
      output$dataset_ui_container <- renderUI({ NULL })
      output$lifeform_ui <- renderUI({ NULL })
      output$dataset_preview_container <- renderUI({ NULL })
      log_message("PLET workflow reset.")
      if (!is.null(rv$features)) {
        leafletProxy("map", session) %>%
          clearShapes() %>%
          addPolygons(
            data = rv$features,
            layerId = ~as.character(ID),
            color = "blue", weight = 2, fillOpacity = 0.25,
            highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
            label = ~as.character(ID)
          )
      }
    }
    
    observeEvent(input$reset_workflow, {
      showModal(modalDialog(
        title = "Reset PLET Workflow",
        "This will reset all selections and loaded data.",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "OK", class = "btn btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_reset, { removeModal(); reset() })
    
    # ==========================================================
    # Return to main menu
    # ==========================================================
    observeEvent(input$return_main, { reset(); log_message("Return to main menu triggered."); global$return_to_tool1_menu <- isolate(global$return_to_tool1_menu) + 1 })
    
    invisible(list(reset = reset))
  })
}
