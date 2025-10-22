tool1BlueUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card p-3 shadow-sm rounded-3 mt-3",
        h4("Tool 1 Blue Page"),
        # blue button
        actionButton(ns("btn"), "Press me", class = "btn btn-primary"),
        br(), br(),
        div(class = "fw-semibold text-success", textOutput(ns("text"), container = span))
    )
  )
}

tool1BlueServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Button logic
    observeEvent(input$btn, {
      rv$counters$tool1$blue <- rv$counters$tool1$blue + 1
      output$text <- renderText({
        sprintf("You pressed Blue button %d times", rv$counters$tool1$blue)
      })
    })
    
    # Watch for Tool 1 reset
    observeEvent(rv$reset_tool1, {
      output$text <- renderText({ "" })
      rv$counters$tool1$blue <- 0
    })
  })
}

