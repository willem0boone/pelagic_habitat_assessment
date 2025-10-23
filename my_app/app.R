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
    reset_all = 0,
    current_tab = "welcome",
    pending_new = NULL
  )
  
  # Call modular servers (each module keeps its own counters)
  tool1Server("tool1", rv)
  tool2Server("tool2", rv)
  tool3Server("tool3", rv)
  
  # Tab-switch confirmation
  observeEvent(input$tabs, {
    new <- input$tabs
    old <- rv$current_tab
    if (is.null(new) || new == old) return()
    
    if (grepl("^tool", old) && new != old) {
      updateTabsetPanel(session, "tabs", selected = old)
      rv$pending_new <- new
      showModal(modalDialog(
        title = "Leaving Tool",
        paste0("You are leaving ", old, ". All data will be reset."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("leave_ok", "OK", class = "btn btn-danger")
        )
      ))
    } else {
      rv$current_tab <- new
    }
  })
  
  observeEvent(input$leave_ok, {
    removeModal()
    old <- rv$current_tab
    new <- rv$pending_new
    rv$pending_new <- NULL
    
    if (grepl("^tool", old)) {
      rv$reset_all <- rv$reset_all + 1  # <-- single global reset trigger
    }
    
    if (!is.null(new)) {
      rv$current_tab <- new
      updateTabsetPanel(session, "tabs", selected = new)
    }
  })
}

# --- Run app ---
shinyApp(ui, server)
