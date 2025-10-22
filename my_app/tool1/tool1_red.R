tool1RedUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card p-3 shadow-sm rounded-3 mt-3",
        h4("Tool 1 Red Page"),
        actionButton(ns("btn"), "Press me", class = "btn btn-danger"),
        br(), br(),
        div(class = "fw-semibold text-success", textOutput(ns("text"), container = span))
    )
  )
}

tool1RedServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Button logic: increment only the red counter
    observeEvent(input$btn, {
      rv$counters$tool1$red <- rv$counters$tool1$red + 1
      output$text <- renderText({
        sprintf("You pressed Red button %d times", rv$counters$tool1$red)
      })
    })
    
    # Watch for Tool 1 reset
    observeEvent(rv$reset_tool1, {
      output$text <- renderText({ "" })
      rv$counters$tool1$red <- 0
    })
  })
}
