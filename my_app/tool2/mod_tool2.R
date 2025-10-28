tool2UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card p-3 shadow-sm rounded-3 mt-3",
        h4("Tool 2 Mockup"),
        actionButton(ns("btn"), "Press me", class = "btn btn-primary"),
        br(), br(),
        div(class = "fw-semibold text-success", textOutput(ns("text"), container = span)),
        br(),
        actionButton(ns("reset_tool"), "Reset Tool 2", class = "btn btn-warning mt-3")
    )
  )
}

tool2Server <- function(id, global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(counter = 0)
    
    observeEvent(input$btn, {
      rv$counter <- rv$counter + 1
      output$text <- renderText({
        sprintf("You pressed Tool 2 button %d times", rv$counter)
      })
    })
    
    reset_tool2_ui <- function() {
      rv$counter <- 0
      output$text <- renderText({ "" })
    }
    
    observeEvent(input$reset_tool, {
      showModal(modalDialog(
        title = "Reset Tool 2",
        "You are resetting Tool 2. All data will be cleared.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "OK", class = "btn btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_reset, {
      try(removeModal(), silent = TRUE)
      reset_tool2_ui()
    })
    
    observeEvent(global$reset_all, {
      reset_tool2_ui()
    })
    
    list(reset = reset_tool2_ui)
  })
}