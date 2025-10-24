
# run_tool1.R
library(shiny)
source("tool1/mod_tool1.R")  # ensure submodules are sourced too

# Minimal "global" reactiveValues
global <- reactiveValues(return_to_tool1_menu = 0, reset_all = 0)

shinyApp(
  ui = fluidPage(
    tool1UI("tool1")
  ),
  server = function(input, output, session) {
    tool1Server("tool1", global)
  }
)
