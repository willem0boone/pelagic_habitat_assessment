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

# --- UI ---
ui <- navbarPage(
  title = "Plankton DTO",
  id = "tabs",
  theme = my_theme,
  header = useShinyjs(),
  
  tabPanel("Welcome", value = "welcome",
           fluidPage(includeHTML("welcome.html"))
  ),
  tabPanel("Tool 1", value = "tool1",
           fluidPage(tool1UI("tool1"))
  ),
  tabPanel("Tool 2", value = "tool2",
           fluidPage(tool2UI("tool2"))
  ),
  tabPanel("Tool 3", value = "tool3",
           fluidPage(tool3UI("tool3"))
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  rv <- reactiveValues(
    counters = list(
      tool1 = list(red = 0, green = 0, blue = 0), # <- must be a list
      tool2 = 0,
      tool3 = 0
    ),
    current_tab = "welcome",
    pending_new = NULL,
    reset_tool1 = 0
  )
  
  # Call module servers
  tool1Server("tool1", rv)
  tool2Server("tool2", rv)
  tool3Server("tool3", rv)
  
  # Reset helper for counters & JS outputs
  reset_tool <- function(tool_name) {
    rv$counters[[tool_name]] <- 0
    session$sendCustomMessage("resetTool", tool_name)
    
    # Trigger Tool 1 reset if applicable
    if (tool_name == "tool1") {
      rv$reset_tool1 <- rv$reset_tool1 + 1
    }
  }
  
  # Tab switching with confirmation
  observeEvent(input$tabs, {
    new <- input$tabs
    old <- rv$current_tab
    if (is.null(new) || new == old) return()
    
    if (grepl("^tool", old) && new != old) {
      # revert tab selection until user confirms reset
      updateTabsetPanel(session, "tabs", selected = old)
      rv$pending_new <- new
      showModal(modalDialog(
        title = "Leaving Tool",
        paste0("You are leaving ", old, ". All data will be reset."),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("leave_ok", "OK", class = "btn btn-danger")
        )
      ))
    } else {
      rv$current_tab <- new
    }
  })
  
  # Modal OK event
  observeEvent(input$leave_ok, {
    removeModal()
    old <- rv$current_tab
    if (grepl("^tool", old)) reset_tool(old)
    new <- rv$pending_new
    rv$pending_new <- NULL
    if (!is.null(new)) {
      rv$current_tab <- new
      updateTabsetPanel(session, "tabs", selected = new)
    }
  })
  
  # JS reset handler for textOutputs in modules
  js_reset <- "
    Shiny.addCustomMessageHandler('resetTool', function(tool) {
      const txt = document.querySelector(`#${tool}-text`);
      if (txt) txt.innerHTML = '';
    });
  "
  extendShinyjs(text = js_reset, functions = NULL)
}

# --- Run app ---
shinyApp(ui, server)
