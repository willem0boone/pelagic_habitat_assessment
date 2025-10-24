library(shiny)
library(bslib)

ui <- page_fluid(
  selectInput( 
    "select", 
    "Select options below:", 
    list("Choice 1A" = "1A", "Choice 1B" = "1B", "Choice 1C" = "1C"), 
    multiple = FALSE 
  ), 
  textOutput("value")
)

server <- function(input, output) {
  output$value <- renderText({input$select})
}

shinyApp(ui = ui, server = server)