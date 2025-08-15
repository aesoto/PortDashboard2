# Complete Shiny App for Portfolio Analytics
library(shiny)
library(DT)
library(dplyr)
library(data.table)
library(openxlsx)
library(stringr)
library(httr)
library(jsonlite)

# Source the functions file
source("activeShareFunctions.R")

# UI
ui <- fluidPage(
  # Add custom CSS for typography and styling
  tags$head(
    tags$style(HTML("
      /* Import Google Fonts */
      @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@300;400;500;600;700&display=swap');
      
      /* Custom color variables */
      :root {
        --header-blue: #6e8db0;
        --text-black: rgba(0, 0, 0, 0.85);
      }
      
      /* Header text styling - Adobe Garamond Pro with fallbacks */
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Adobe Garamond Pro', 'EB Garamond', 'Times New Roman', serif !important;
        color: var(--header-blue) !important;
        font-weight: 600 !important;
      }
      
      /* Body text styling - Montserrat */
      body, p, div, span, label, input, select, button, .form-control, .btn {
        font-family: 'Montserrat', 'Helvetica Neue', Arial, sans-serif !important;
        color: var(--text-black) !important;
      }
      
      /* Button styling */
      .btn-primary {
        background-color: var(--header-blue) !important;
        border-color: var(--header-blue) !important;
        font-weight: 500 !important;
      }
      
      .btn-primary:hover {
        background-color: #5a7a9e !important;
        border-color: #5a7a9e !important;
      }
      
      /* Content sections */
      .content-header {
        background-color: #f8f9fa;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
        border-left: 4px solid var(--header-blue);
      }
      
      .parameter-panel {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  # Header
  fluidRow(
    column(12,
           div(class = "content-header",
               h1("Portfolio Analytics", style = "margin: 0;")
           )
    )
  ),
  
  # Main content
  tabsetPanel(
    tabPanel("Active Share",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 div(class = "parameter-panel",
                     h4("Parameters"),
                     
                     selectInput("account",
                                 label = "Account:",
                                 choices = list("Loading..." = ""),
                                 selected = NULL),
                     
                     selectInput("model",
                                 label = "Model:",
                                 choices = list("Loading..." = ""),
                                 selected = NULL),
                     
                     dateInput("date",
                               label = "Date:",
                               value = Sys.Date(),
                               format = "yyyy-mm-dd"),
                     
                     br(),
                     actionButton("calculate_active_share",
                                  "Calculate Active Share",
                                  class = "btn-primary",
                                  style = "width: 100%;"),
                     
                     br(), br(),
                     uiOutput("calculation_status")
                 )
               ),
               
               mainPanel(
                 width = 9,
                 h3("Active Share Results"),
                 
                 # Test output
                 div(
                   style = "margin: 20px 0; padding: 10px; background-color: #f8f9fa;",
                   h5("Debug Info:"),
                   verbatimTextOutput("test_output")
                 ),
                 
                 # Results
                 uiOutput("active_share_summary"),
                 br(),
                 DT::dataTableOutput("active_share_table")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  cat("=== SERVER STARTED ===\n")
  
  values <- reactiveValues(
    portfolio_data = NULL,
    model_data = NULL,
    active_share_results = NULL
  )
  
  # Test output - this should ALWAYS work
  output$test_output <- renderText({
    cat("=== TEST OUTPUT RENDERING ===\n")
    return(paste("Button clicks:", input$calculate_active_share, "| Time:", Sys.time()))
  })
  
  # Simple status
  output$calculation_status <- renderUI({
    div("Status: Ready")
  })
  
  # Simple summary
  output$active_share_summary <- renderUI({
    cat("=== SUMMARY RENDERING ===\n")
    if (is.null(values$active_share_results)) {
      return(div("No results yet"))
    } else {
      return(div("Results available!"))
    }
  })
  
  # Simple table
  output$active_share_table <- renderDT({
    cat("=== TABLE RENDERING ===\n")
    return(datatable(data.frame(Test = "Table works"), options = list(dom = 't')))
  })
  
  cat("=== SERVER SETUP COMPLETE ===\n")
}

# Run the app
shinyApp(ui = ui, server = server)