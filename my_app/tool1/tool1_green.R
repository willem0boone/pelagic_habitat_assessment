tool1GreenUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card p-3 shadow-sm rounded-3 mt-3",
        h4("Tool 1 Green Page"),
        # green button
        actionButton(ns("btn"), "Press me", class = "btn btn-success"),
        br(), br(),
        div(class = "fw-semibold text-success", textOutput(ns("text"), container = span))
    )
  )
}

tool1GreenServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$btn, {
      rv$counters$tool1$green <- rv$counters$tool1$green + 1
      output$text <- renderText({
        sprintf("You pressed Green button %d times", rv$counters$tool1$green)
      })
    })
    # --- Watch for Tool 1 reset ---
    observeEvent(rv$reset_tool1, {
      output$text <- renderText({ "" })
      rv$counters$tool1$green <- 0
    })
  })
}