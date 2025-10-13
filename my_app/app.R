library(shiny)
library(shinyjs)

# Load the tool1 module
source("ph1/tool1.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("My Shiny App"),
  tabsetPanel(
    id = "mainTabs",  # give tabsetPanel an id for navigation
    tabPanel("Welcome", includeHTML("welcome.html")),
    tabPanel("Tool 1", tool1_ui("tool1")),  # pass an id
    tabPanel("Tool 2", h3("Tool 2 Page"), p("This page will be filled later.")),
    tabPanel("Tool 3", h3("Tool 3 Page"), p("This page will be filled later."))
  )
)

server <- function(input, output, session) {
  # reactiveValues object to share data between modules
  analysis_results <- reactiveValues()
  
  # Call the tool1 module with the same id as in UI
  callModule(tool1_server, "tool1", analysis_results = analysis_results)
  
}

shinyApp(ui, server)
