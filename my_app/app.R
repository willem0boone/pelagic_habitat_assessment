library(shiny)
library(shinyjs)

# Load the tool1 script
source("tool1.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("My Shiny App"),
  tabsetPanel(
    tabPanel("Welcome", includeHTML("welcome.html")),
    tabPanel("Tool 1", tool1_ui),  # <- comma missing previously
    tabPanel("Tool 2", h3("Tool 2 Page"), p("This page will be filled later.")),
    tabPanel("Tool 3", h3("Tool 3 Page"), p("This page will be filled later."))
  )
)

server <- function(input, output, session) {
  tool1_server(input, output, session)  # <- call the server function
}

shinyApp(ui, server)
