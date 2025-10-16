library(shiny)
library(shinyjs)
library(shinyWidgets)

source("tool1/tool1_server.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Samson en Gert: Plankton Platform"),
  tabsetPanel(
    id = "mainTabs",
    tabPanel("Welcome", includeHTML("welcome.html")),
    tabPanel("Tool 1", tool1_ui("tool1")),  # module UI
    tabPanel("Tool 2", h3("Tool 2 Page")),
    tabPanel("Tool 3", h3("Tool 3 Page"))
  )
)

server <- function(input, output, session) {
  # Directly call the module as a function
  tool1_server("tool1")  
}

shinyApp(ui, server)
