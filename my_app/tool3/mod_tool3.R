# ===============================
# TOOL 3 MODULE (self-contained)
# ===============================

tool3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card p-3 shadow-sm rounded-3 mt-3",
        h4("Tool 3 Mockup"),
        # main action button
        actionButton(ns("btn"), "Press me", class = "btn btn-primary"),
        br(), br(),
        # output text
        div(class = "fw-semibold text-success", textOutput(ns("text"), container = span)),
        br(),
        # --- Reset Tool button ---
        actionButton(ns("reset_tool"), "Reset Tool 3", class = "btn btn-warning mt-3")
    )
  )
}

tool3Server <- function(id, global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Local reactive state ---
    local_rv <- reactiveValues(counter = 0)
    
    # --- Main button logic ---
    observeEvent(input$btn, {
      local_rv$counter <- local_rv$counter + 1
      output$text <- renderText({
        sprintf("You pressed Tool 3 button %d times", local_rv$counter)
      })
    })
    
    # --- Local reset modal ---
    observeEvent(input$reset_tool, {
      showModal(modalDialog(
        title = "Reset Tool 3",
        "You are resetting Tool 3. All data will be cleared.",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "OK", class = "btn btn-danger")
        )
      ))
    })
    
    # --- Confirm reset ---
    observeEvent(input$confirm_reset, {
      removeModal()
      local_rv$counter <- 0
      output$text <- renderText({ "" })
    })
    
    # --- Respond to global reset (from app.R) ---
    observeEvent(global$reset_all, {
      local_rv$counter <- 0
      output$text <- renderText({ "" })
    })
  })
}
