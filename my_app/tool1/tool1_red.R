library(shiny)

tool1RedUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Tool1 – Red Workflow"),
    actionButton(ns("btn"), "Press me", class = "btn btn-primary"),
    br(), br(),
    textOutput(ns("text")),
    br(),
    actionButton(ns("reset_workflow"), "Reset Red Workflow", class = "btn btn-warning")
  )
}

tool1RedServer <- function(id, global, log_fun = function(msg){}) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(counter = 0)
    
    # --- Button press ---
    observeEvent(input$btn, {
      rv$counter <- rv$counter + 1
      output$text <- renderText({ sprintf("Button pressed %d times", rv$counter) })
      log_fun(paste0("Red button pressed ", rv$counter, " times"))
    })
    
    # --- Reset function ---
    reset <- function() {
      rv$counter <- 0
      output$text <- renderText({ "" })
      log_fun("Red workflow reset")
    }
    
    # --- Reset button ---
    observeEvent(input$reset_workflow, {
      showModal(modalDialog(
        title = "Reset Red Workflow",
        "This will reset all Red workflow selections.",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "OK", class = "btn btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_reset, {
      removeModal()
      reset()
    })
    
    invisible(list(reset = reset))
  })
}
