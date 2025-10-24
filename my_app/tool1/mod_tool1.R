# ==========================================================
# mod_tool1.R — Tool1 module (Color selection + workflows)
# ==========================================================

library(shiny)

# --- Source color modules ---
source("tool1/tool1_red.R")
source("tool1/tool1_green.R")
source("tool1/tool1_blue.R")
source("tool1/tool1_yellow.R")

# ---------------------- Main Tool1 Module ----------------------
tool1UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("main_ui"))
  )
}

tool1Server <- function(id, global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(selected = NULL)
    
    # Safe initialization
    isolate({
      if (is.null(global$return_to_tool1_menu)) global$return_to_tool1_menu <- 0
    })
    
    # Main UI
    output$main_ui <- renderUI({
      if (is.null(local_rv$selected)) {
        fluidPage(
          div(class = "card p-4 shadow-sm rounded-3 mt-4 text-center",
              h3("Welcome to Tool 1"),
              p("Select a color to continue:"),
              br(),
              actionButton(ns("btn_red"), "Red", class = "btn btn-danger btn-lg mx-2"),
              actionButton(ns("btn_green"), "Green", class = "btn btn-success btn-lg mx-2"),
              actionButton(ns("btn_blue"), "Blue", class = "btn btn-primary btn-lg mx-2"),
              actionButton(ns("btn_yellow"), "Yellow", class = "btn btn-warning btn-lg mx-2")
          )
        )
      } else if (local_rv$selected == "red") {
        tool1RedUI(ns("red"))
      } else if (local_rv$selected == "green") {
        tool1GreenUI(ns("green"))
      } else if (local_rv$selected == "blue") {
        tool1BlueUI(ns("blue"))
      } else if (local_rv$selected == "yellow") {
        tool1YellowUI(ns("yellow"))
      }
    })
    
    # Color selection
    observeEvent(input$btn_red,   { local_rv$selected <- "red" })
    observeEvent(input$btn_green, { local_rv$selected <- "green" })
    observeEvent(input$btn_blue,  { local_rv$selected <- "blue" })
    observeEvent(input$btn_yellow,{ local_rv$selected <- "yellow" })
    
    # Launch color submodules
    tool1RedServer("red", global)
    tool1GreenServer("green", global)
    tool1BlueServer("blue", global)
    tool1YellowServer("yellow", global)
    
    # Return to main menu
    observeEvent(global$return_to_tool1_menu, {
      local_rv$selected <- NULL
    })
    
    # Reset Tool1 externally
    reset_tool1 <- function() {
      local_rv$selected <- NULL
    }
    
    return(list(reset = reset_tool1))
  })
}
