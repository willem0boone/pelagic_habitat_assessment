library(shiny)
library(bslib)
library(shinyjs)

# --- Source modules ---
source("tool1/mod_tool1.R")
source("tool2/mod_tool2.R")
source("tool3/mod_tool3.R")

# --- Theme ---
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Poppins")
)

# --- Logging ---
log_event <- function(msg) {
  dir.create("log", showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s\n", timestamp, msg), file = "log/log.txt", append = TRUE)
}

# --- UI ---
ui <- navbarPage(
  title = "Plankton DTO",
  id = "tabs",
  theme = my_theme,
  header = tagList(
    useShinyjs(),
    # CSS for uniform buttons and color overrides
    tags$head(
      tags$style(HTML("
        .tool1-btn {
          width: 100%;
          text-align: center;
          font-weight: 500;
          padding: 10px 0;
        }

        /* Restore Bootstrap default colors */
        .btn-outline-primary { color: #0d6efd !important; border-color: #0d6efd !important; }
        .btn-outline-success { color: #198754 !important; border-color: #198754 !important; }
        .btn-outline-danger  { color: #dc3545 !important; border-color: #dc3545 !important; }
        .btn-outline-warning { color: #ffc107 !important; border-color: #ffc107 !important; }
      "))
    ),
    tags$script(HTML("
      $(document).on('click', '#tabs li a', function(e) {
        var new_tab = $(this).attr('data-value');
        Shiny.setInputValue('tab_click', new_tab, {priority: 'event'});
        e.preventDefault(); // stop default tab switch
      });
    "))
  ),
  
  tabPanel("Welcome", value = "welcome", fluidPage(includeHTML("welcome.html"))),
  tabPanel("Tool 1", value = "tool1", div(id = "tool1_div", tool1UI("tool1"))),
  tabPanel("Tool 2", value = "tool2", div(id = "tool2_div", tool2UI("tool2"))),
  tabPanel("Tool 3", value = "tool3", div(id = "tool3_div", tool3UI("tool3")))
)

# --- SERVER ---
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    current_tab = "welcome",
    pending_tab = NULL
  )
  
  # --- Initialize modules ---
  tool1_reset <- tool1Server("tool1", rv, tab_active = reactive(input$tabs))$reset
  tool2_reset <- tool2Server("tool2", rv)$reset
  tool3_reset <- tool3Server("tool3", rv)$reset
  
  # --- Hide modules initially ---
  hide("tool1_div")
  hide("tool2_div")
  hide("tool3_div")
  
  toggleTabVisibility <- function(tab) {
    hide("tool1_div")
    hide("tool2_div")
    hide("tool3_div")
    if (tab == "tool1") show("tool1_div")
    if (tab == "tool2") show("tool2_div")
    if (tab == "tool3") show("tool3_div")
  }
  
  # --- Intercept tab clicks ---
  observeEvent(input$tab_click, {
    new_tab <- input$tab_click
    old_tab <- rv$current_tab
    if (is.null(new_tab) || new_tab == old_tab) return()
    
    log_event(paste0("Tab switch requested from '", old_tab, "' to '", new_tab, "'"))
    
    if (grepl("^tool", old_tab)) {
      rv$pending_tab <- new_tab
      showModal(modalDialog(
        title = "Leaving Tool",
        paste0("You are leaving ", old_tab, ". All data will be reset."),
        easyClose = FALSE,
        footer = tagList(
          actionButton("cancel_leave", "Cancel"),
          actionButton("confirm_leave", "OK", class = "btn btn-danger")
        )
      ))
    } else {
      rv$current_tab <- new_tab
      updateTabsetPanel(session, "tabs", selected = new_tab)
      toggleTabVisibility(new_tab)
      log_event(paste0("Switched to tab '", new_tab, "' without confirmation"))
      
      # Reset Tool1 after tab is fully shown
      if (new_tab == "tool1") {
        invalidateLater(10, session)
        tool1_reset()
      }
    }
  })
  
  observeEvent(input$confirm_leave, {
    removeModal()
    old <- rv$current_tab
    new <- rv$pending_tab
    rv$pending_tab <- NULL
    
    # Reset old module
    if (old == "tool1") tool1_reset()
    if (old == "tool2") tool2_reset()
    if (old == "tool3") tool3_reset()
    
    # Switch tab
    rv$current_tab <- new
    updateTabsetPanel(session, "tabs", selected = new)
    toggleTabVisibility(new)
    log_event(paste0("Switched from '", old, "' to '", new, "' after OK"))
    
    # Explicitly reset Tool1 if switching TO it
    if (new == "tool1") {
      # short delay ensures divs are shown before reset
      invalidateLater(10, session)
      tool1_reset()
    }
  })
  
  # --- Cancel leaving tool ---
  observeEvent(input$cancel_leave, {
    removeModal()
    updateTabsetPanel(session, "tabs", selected = rv$current_tab) # restore previous tab
    rv$pending_tab <- NULL
    log_event(paste0("User cancelled tab switch from '", rv$current_tab, "'"))
  })
}

# --- Run app ---
shinyApp(ui, server)
