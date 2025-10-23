tool1YellowUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card p-3 shadow-sm rounded-3 mt-3",
        h4("Tool 1 – Yellow Workflow"),
        actionButton(ns("btn"), "Press me", class = "btn btn-warning"),
        br(), br(),
        div(class = "fw-semibold text-success", textOutput(ns("text"), container = span)),
        br(),
        actionButton(ns("reset_tool1"), "Return to Main Menu", class = "btn btn-warning mt-3")
    )
  )
}

tool1YellowServer <- function(id, global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(counter = 0)
    
    observeEvent(input$btn, {
      local_rv$counter <- local_rv$counter + 1
      output$text <- renderText({
        sprintf("You pressed the Yellow button %d times", local_rv$counter)
      })
    })
    
    observeEvent(input$reset_tool1, {
      showModal(modalDialog(
        title = "Reset Tool 1",
        "You are leaving the Yellow workflow. All data will be cleared.",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset_tool1"), "OK", class = "btn btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_reset_tool1, {
      removeModal()
      local_rv$counter <- 0
      output$text <- renderText({ "" })
      global$return_to_tool1_menu <- isolate(global$return_to_tool1_menu) + 1
    })
    
    observeEvent(global$reset_all, {
      local_rv$counter <- 0
      output$text <- renderText({ "" })
    })
  })
}
