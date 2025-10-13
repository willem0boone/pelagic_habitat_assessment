library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("D1 MSFD Indicators"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Select CSV file", accept = ".csv"),
      hr(),
      h4("PH2 Settings"),
      numericInput("ph2_nbMonths", "Minimum Months", value = 6, min = 1),
      numericInput("ph2_nbYears", "Minimum Years", value = 5, min = 1),
      checkboxInput("ph2_anomalies", "Compute Anomalies", TRUE),
      checkboxInput("ph2_cumsum", "Cumulative Sum", TRUE),
      checkboxInput("ph2_decomposition", "Decomposition", TRUE),
      hr(),
      h4("PH3 Settings"),
      numericInput("ph3_nbMonths", "Minimum Months", value = 6, min = 1),
      numericInput("ph3_nbYears", "Minimum Years", value = 5, min = 1),
      checkboxInput("ph3_boxplot", "Boxplots", TRUE),
      checkboxInput("ph3_contour", "Contours", TRUE),
      checkboxInput("ph3_indices", "Indices Graphs", TRUE),
      hr(),
      actionButton("runPH2", "Run PH2"),
      actionButton("runPH3", "Run PH3"),
      actionButton("runPC", "Run Physico-Chemical")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("PH2 Output", DTOutput("ph2_table")),
        tabPanel("PH3 Output", DTOutput("ph3_table")),
        tabPanel("Physico-Chemical", DTOutput("pc_table")),
        tabPanel("Messages", verbatimTextOutput("messages"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store data and messages
  rv <- reactiveValues(
    datafile = NULL,
    ph2_results = NULL,
    ph3_results = NULL,
    pc_results = NULL,
    messages = ""
  )
  
  observeEvent(input$file, {
    rv$datafile <- input$file$datapath
    rv$messages <- paste(rv$messages, "CSV file loaded:", input$file$name, "\n")
  })
  
  observeEvent(input$runPH2, {
    req(rv$datafile)
    rv$messages <- paste(rv$messages, "Running PH2 computation...\n")
    
    # Placeholder: replace with actual PH2 computation functions
    # Example: dfr <- dataImport(rv$datafile)
    rv$ph2_results <- data.frame(
      station = c("Station1", "Station2"),
      t_test = c(2.1, -1.3),
      p_value = c(0.04, 0.2)
    )
    
    rv$messages <- paste(rv$messages, "PH2 computation done.\n")
  })
  
  observeEvent(input$runPH3, {
    req(rv$datafile)
    rv$messages <- paste(rv$messages, "Running PH3 computation...\n")
    
    # Placeholder: replace with actual PH3 computation functions
    rv$ph3_results <- data.frame(
      station = c("Station1", "Station2"),
      richness = c(10, 8),
      gini = c(0.3, 0.25)
    )
    
    rv$messages <- paste(rv$messages, "PH3 computation done.\n")
  })
  
  observeEvent(input$runPC, {
    req(rv$datafile)
    rv$messages <- paste(rv$messages, "Running Physico-Chemical computation...\n")
    
    # Placeholder: replace with actual PC computation
    rv$pc_results <- data.frame(
      station = c("Station1", "Station2"),
      parameter = c("Temperature", "Salinity"),
      mean_value = c(15.2, 35.1)
    )
    
    rv$messages <- paste(rv$messages, "Physico-Chemical computation done.\n")
  })
  
  # Render outputs
  output$ph2_table <- renderDT({
    req(rv$ph2_results)
    datatable(rv$ph2_results)
  })
  
  output$ph3_table <- renderDT({
    req(rv$ph3_results)
    datatable(rv$ph3_results)
  })
  
  output$pc_table <- renderDT({
    req(rv$pc_results)
    datatable(rv$pc_results)
  })
  
  output$messages <- renderText({
    rv$messages
  })
}

# Run the app
shinyApp(ui, server)
