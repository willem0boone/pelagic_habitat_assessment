# ===============================
# TOOL 1 MODULE
# ===============================

tool2UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card p-3 shadow-sm rounded-3 mt-3",
        h4("Tool 2 Mockup"),
        # main action button
        actionButton(ns("btn"), "Press me", class = "btn btn-primary"),
        br(), br(),
        # output text
        div(class = "fw-semibold text-success", textOutput(ns("text"), container = span)),
        br(),
        # --- Reset Tool button ---
        actionButton(ns("reset_tool"), "Reset Tool 2", class = "btn btn-warning mt-3")
    )
  )
}

tool2Server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Button counter ---
    observeEvent(input$btn, {
      rv$counters$tool2 <- rv$counters$tool2 + 1
      output$text <- renderText({
        sprintf("You pressed Tool 2 button %d times", rv$counters$tool2)
      })
    })
    
    # --- Reset button logic ---
    observeEvent(input$reset_tool, {
      # Signal app.R to trigger modal + reset
      rv$pending_new <- "tool2"
      showModal(modalDialog(
        title = "Reset Tool 2",
        "You are resetting Tool 2. All data will be cleared.",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("leave_ok", "OK", class = "btn btn-danger")
        )
      ))
    })
    
    # --- Clear UI text if counters reset ---
    observeEvent(rv$counters$tool2, {
      if (rv$counters$tool2 == 0) {
        output$text <- renderText({ "" })
      }
    })
  })
}
