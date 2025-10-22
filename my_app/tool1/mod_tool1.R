# ===============================
# TOOL 1 MODULE
# ===============================

source("tool1/tool1_red.R")
source("tool1/tool1_green.R")
source("tool1/tool1_blue.R")

tool1UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("main_ui")),
    br(),
    div(class = "text-center",
        actionButton(ns("reset_tool"), "Reset Tool 1", class = "btn btn-warning mt-3")
    )
  )
}

tool1Server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # local reactive state
    local_rv <- reactiveValues(selected = NULL)
    
    # --- Render UI dynamically ---
    output$main_ui <- renderUI({
      if (is.null(local_rv$selected)) {
        fluidPage(
          div(class = "card p-4 shadow-sm rounded-3 mt-4 text-center",
              h3("Welcome to Tool 1"),
              p("Select a color to continue:"),
              br(),
              actionButton(ns("btn_red"), "Red", class = "btn btn-danger btn-lg mx-2"),
              actionButton(ns("btn_green"), "Green", class = "btn btn-success btn-lg mx-2"),
              actionButton(ns("btn_blue"), "Blue", class = "btn btn-primary btn-lg mx-2")
          )
        )
      } else if (local_rv$selected == "red") {
        tool1RedUI(ns("red"))
      } else if (local_rv$selected == "green") {
        tool1GreenUI(ns("green"))
      } else if (local_rv$selected == "blue") {
        tool1BlueUI(ns("blue"))
      }
    })
    
    # --- Color selection ---
    observeEvent(input$btn_red,   { local_rv$selected <- "red" })
    observeEvent(input$btn_green, { local_rv$selected <- "green" })
    observeEvent(input$btn_blue,  { local_rv$selected <- "blue" })
    
    # --- Reset Tool button ---
    observeEvent(input$reset_tool, {
      rv$reset_tool1 <- rv$reset_tool1 + 1  # increment reactive trigger
    })
    
    # --- Connect color submodules ---
    tool1RedServer("red", rv)
    tool1GreenServer("green", rv)
    tool1BlueServer("blue", rv)
    
    # --- Watch reset trigger ---
    observeEvent(rv$reset_tool1, {
      local_rv$selected <- NULL
      output$text <- renderText({ "" })
    })
  })
}
