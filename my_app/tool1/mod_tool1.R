# ==========================================================
# mod_tool1.R — Tool1 module with static workflows and sidebar
# ==========================================================

library(shiny)
library(shinyjs)

# --- Source workflow modules locally ---
source("tool1/tool1_red.R", local = TRUE)
source("tool1/tool1_green.R", local = TRUE)
source("tool1/tool1_blue.R", local = TRUE)
source("tool1/tool1_yellow.R", local = TRUE)

# ---------------- Logging setup ----------------
tool1_default_log_fun <- local({
  tool1_log_dir <- "tool1/logs"
  if (!dir.exists(tool1_log_dir)) dir.create(tool1_log_dir, recursive = TRUE)
  log_file <- file.path(tool1_log_dir, "tool1_log.txt")
  function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(sprintf("[%s] %s\n", timestamp, msg), file = log_file, append = TRUE)
  }
})

# ---------------- UI ----------------
tool1UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    tags$style(HTML("
      .tool1-btn {
        width: 100%;
        text-align: center;
        font-weight: 500;
        padding: 10px 0;
      }
    ")),
    fluidRow(
      column(
        width = 2,
        wellPanel(
          actionButton(ns("menu_welcome"), "Welcome", class = "btn btn-outline-primary tool1-btn mb-2"),
          actionButton(ns("menu_red"), "Red", class = "btn btn-outline-danger tool1-btn mb-2"),
          actionButton(ns("menu_green"), "Green", class = "btn btn-outline-success tool1-btn mb-2"),
          actionButton(ns("menu_blue"), "Blue", class = "btn btn-outline-primary tool1-btn mb-2"),
          actionButton(ns("menu_yellow"), "Yellow", class = "btn btn-outline-warning tool1-btn mb-2")
        )
      ),
      column(
        width = 10,
        div(id = ns("welcome_div"), includeHTML("tool1/tool1_welcome.html")),
        div(id = ns("red_div"), tool1RedUI(ns("red"))),
        div(id = ns("green_div"), tool1GreenUI(ns("green"))),
        div(id = ns("blue_div"), tool1BlueUI(ns("blue"))),
        div(id = ns("yellow_div"), tool1YellowUI(ns("yellow")))
      )
    )
  )
}

# ---------------- Server ----------------
tool1Server <- function(id, global, log_fun = NULL, tab_active = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(selected = "welcome") # default
    
    # --- Choose logging function ---
    if (is.null(log_fun)) log_fun <- tool1_default_log_fun
    log_tool1 <- function(msg) log_fun(paste0("[Tool1] ", msg))
    
    # --- Launch all workflows once ---
    red_srv    <- tool1RedServer("red", global, log_fun = log_tool1)
    green_srv  <- tool1GreenServer("green", global, log_fun = log_tool1)
    blue_srv   <- tool1BlueServer("blue", global, log_fun = log_tool1)
    yellow_srv <- tool1YellowServer("yellow", global, log_fun = log_tool1)
    
    # --- Helper: hide all divs ---
    hide_all <- function() {
      hide("welcome_div"); hide("red_div"); hide("green_div"); hide("blue_div"); hide("yellow_div")
    }
    
    # --- Initial visibility ---
    hide_all()
    show("welcome_div")
    log_tool1("Tool1 main page initialized")
    
    # --- Sidebar menu clicks ---
    observeEvent(input$menu_welcome, { perform_switch("welcome") })
    observeEvent(input$menu_red,    { switch_workflow("red") })
    observeEvent(input$menu_green,  { switch_workflow("green") })
    observeEvent(input$menu_blue,   { switch_workflow("blue") })
    observeEvent(input$menu_yellow, { switch_workflow("yellow") })
    
    # --- Workflow switch with confirmation ---
    switch_workflow <- function(wf) {
      old <- local_rv$selected
      if (!is.null(old) && old != "welcome" && old != wf) {
        showModal(modalDialog(
          title = "Leaving Workflow",
          paste0("You are leaving ", old, ". All data will be reset."),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_switch"), "OK", class = "btn btn-danger")
          )
        ))
        local_rv$pending <- wf
      } else {
        perform_switch(wf)
      }
    }
    
    # --- Confirm workflow switch ---
    observeEvent(input$confirm_switch, {
      removeModal()
      old <- local_rv$selected
      new <- isolate(local_rv$pending)
      
      if (old %in% c("red","green","blue","yellow")) {
        switch(old,
               red    = red_srv$reset(),
               green  = green_srv$reset(),
               blue   = blue_srv$reset(),
               yellow = yellow_srv$reset())
        log_tool1(paste0("Reset workflow ", old))
      }
      
      perform_switch(new)
      local_rv$pending <- NULL
    })
    
    # --- Perform switch ---
    perform_switch <- function(wf) {
      hide_all()
      switch(wf,
             "welcome" = show("welcome_div"),
             "red"     = show("red_div"),
             "green"   = show("green_div"),
             "blue"    = show("blue_div"),
             "yellow"  = show("yellow_div"))
      local_rv$selected <- wf
      log_tool1(paste0("Switched workflow to ", wf))
    }
    
    # --- Expose reset function ---
    reset_all <- function() {
      red_srv$reset(); green_srv$reset(); blue_srv$reset(); yellow_srv$reset()
      hide_all(); show("welcome_div")
      local_rv$selected <- "welcome"
      log_tool1("All workflows reset via global$reset_all")
    }
    
    # --- Reset Tool1 when tab becomes active ---
    if (!is.null(tab_active)) {
      observeEvent(tab_active(), {
        if (tab_active() == "tool1") {
          shinyjs::delay(50, reset_all)  # force Welcome page after modules settle
        }
      })
    }
    
    invisible(list(reset = reset_all))
  })
}
