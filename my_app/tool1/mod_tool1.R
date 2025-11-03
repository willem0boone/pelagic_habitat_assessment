# ==========================================================
# mod_tool1.R — Tool1 module with neutral subtabs and active highlight
# ==========================================================

library(shiny)
library(shinyjs)

# --- Source workflow modules ---
source("tool1/tool1_plet.R", local = TRUE)
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
    
    # --- Neutral sidebar menu ---
    fluidRow(
      column(
        width = 2,
        wellPanel(
          # Add CSS for active highlight
          tags$head(tags$style(HTML("
            .menu-btn {
              width: 100%;
              text-align: center;
              font-weight: 500;
              padding: 10px 0;
              margin-bottom: 5px;
              background-color: #f5f5f5;
              border: 1px solid #ccc;
              border-radius: 4px;
              color: #333;
            }
            .menu-btn.active {
              background-color: #d9edf7;
              font-weight: 600;
              color: #31708f;
            }
          "))),
          
          actionButton(ns("menu_welcome"), "Welcome", class = "menu-btn"),
          actionButton(ns("menu_plet"), "PLET", class = "menu-btn"),
          actionButton(ns("menu_green"), "Green", class = "menu-btn"),
          actionButton(ns("menu_blue"), "Blue", class = "menu-btn"),
          actionButton(ns("menu_yellow"), "Yellow", class = "menu-btn")
        )
      ),
      
      column(
        width = 10,
        div(id = ns("welcome_div"), includeHTML("tool1/tool1_welcome.html")),
        div(id = ns("plet_div"), tool1PletUI(ns("plet"))),
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
    
    # --- Logging ---
    if (is.null(log_fun)) log_fun <- tool1_default_log_fun
    log_tool1 <- function(msg) log_fun(paste0("[Tool1] ", msg))
    
    # --- Launch workflows ---
    plet_srv   <- tool1PletServer("plet", global)
    green_srv  <- tool1GreenServer("green", global, log_fun = log_tool1)
    blue_srv   <- tool1BlueServer("blue", global, log_fun = log_tool1)
    yellow_srv <- tool1YellowServer("yellow", global, log_fun = log_tool1)
    
    # --- Helper: hide all divs ---
    hide_all <- function() {
      hide("welcome_div"); hide("plet_div"); hide("green_div"); hide("blue_div"); hide("yellow_div")
    }
    
    # --- Highlight active menu button ---
    highlight_menu <- function(active) {
      btns <- c("menu_welcome","menu_plet","menu_green","menu_blue","menu_yellow")
      for (b in btns) {
        if (b == paste0("menu_", active)) {
          shinyjs::addClass(selector = paste0("#", ns(b)), class = "active")
        } else {
          shinyjs::removeClass(selector = paste0("#", ns(b)), class = "active")
        }
      }
    }
    
    # --- Perform switch ---
    perform_switch <- function(wf) {
      hide_all()
      switch(wf,
             "welcome" = show("welcome_div"),
             "plet"    = show("plet_div"),
             "green"   = show("green_div"),
             "blue"    = show("blue_div"),
             "yellow"  = show("yellow_div"))
      local_rv$selected <- wf
      highlight_menu(wf)
      log_tool1(paste0("Switched workflow to ", wf))
    }
    
    # --- Sidebar menu clicks ---
    observeEvent(input$menu_welcome, { perform_switch("welcome") })
    observeEvent(input$menu_plet,    { perform_switch("plet") })
    observeEvent(input$menu_green,   { perform_switch("green") })
    observeEvent(input$menu_blue,    { perform_switch("blue") })
    observeEvent(input$menu_yellow,  { perform_switch("yellow") })
    
    # --- Expose reset function ---
    reset_all <- function() {
      plet_srv$reset(); green_srv$reset(); blue_srv$reset(); yellow_srv$reset()
      hide_all(); perform_switch("welcome")
      log_tool1("All workflows reset via global$reset_all")
    }
    
    # --- Reset on leaving Tool1 tab ---
    if (!is.null(tab_active)) {
      observeEvent(tab_active(), {
        if (tab_active() != "tool1") reset_all()
      }, ignoreInit = TRUE)
    }
    
    # --- Initialize ---
    perform_switch("welcome")
    
    invisible(list(reset = reset_all))
  })
}
