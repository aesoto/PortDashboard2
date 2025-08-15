# Portfolio Dashboard 2
# Summer 2025 version
# Enhanced UI for Portfolio Dashboard Shiny App

library(shiny)
library(shinycssloaders)  # For loading animations (optional)

fluidPage(
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
      
      /* Specific adjustments for form elements */
      .control-label {
        font-weight: 500 !important;
        margin-bottom: 8px !important;
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
      
      .parameter-panel h4 {
        margin-top: 0 !important;
      }
      
      .results-placeholder {
        text-align: center;
        color: #6c757d;
        padding: 40px;
        border: 2px dashed #dee2e6;
        border-radius: 5px;
        margin-top: 20px;
      }
      
      .results-placeholder h4 {
        color: #6c757d !important;
      }
      
      /* Tab styling */
      .nav-tabs > li.active > a {
        color: var(--header-blue) !important;
        border-bottom-color: var(--header-blue) !important;
      }
      
      .nav-tabs > li > a:hover {
        color: var(--header-blue) !important;
      }
      
      /* Status indicators */
      .status-success {
        color: #28a745 !important;
      }
      
      .status-error {
        color: #dc3545 !important;
      }
      
      .status-info {
        color: var(--header-blue) !important;
      }
    "))
  ),
  
  # Custom header with title and logo
  fluidRow(
    column(8,
           div(class = "content-header",
               h1("Portfolio Analytics", 
                  style = "margin: 0;")
           )
    ),
    column(4,
           div(style = "text-align: right; margin-top: 10px;",
               img(src = "logo.jpg", 
                   height = "60px",
                   style = "max-width: 100%; border-radius: 3px;",
                   onerror = "this.style.display='none'")  # Hide if logo fails to load
           )
    )
  ),
  
  # Create tabset panel for different sections
  tabsetPanel(
    id = "main_tabs",
    
    # First tab: Active Share
    tabPanel("Active Share",
             value = "active_share",
             sidebarLayout(
               # Sidebar panel for inputs
               sidebarPanel(
                 width = 3,
                 div(class = "parameter-panel",
                     h4("Parameters"),
                     
                     # Account dropdown with loading state
                     selectInput("account",
                                 label = "Account:",
                                 choices = list("Loading..." = ""),
                                 selected = NULL),
                     
                     # Model dropdown with loading state
                     selectInput("model",
                                 label = "Model:",
                                 choices = list("Loading..." = ""),
                                 selected = NULL),
                     
                     # Date input with calendar widget
                     dateInput("date",
                               label = "Date:",
                               value = Sys.Date(),
                               format = "yyyy-mm-dd",
                               max = Sys.Date()),  # Prevent future dates
                     
                     # Action button to trigger calculations
                     br(),
                     actionButton("calculate_active_share",
                                  "Calculate Active Share",
                                  class = "btn-primary",
                                  style = "width: 100%;"),
                     
                     # Status indicator
                     br(), br(),
                     uiOutput("calculation_status")
                 )
               ),
               
               # Main panel for results
               mainPanel(
                 width = 9,
                 h3("Active Share Results"),
                 
                 # Results section
                 conditionalPanel(
                   condition = "input.calculate_active_share == 0",
                   div(class = "results-placeholder",
                       icon("chart-line", style = "font-size: 48px; color: #dee2e6; margin-bottom: 15px;"),
                       br(),
                       h4("Ready to Calculate", style = "margin-bottom: 10px;"),
                       p("Select your parameters and click 'Calculate Active Share' to view results.")
                   )
                 ),
                 
                 # Placeholder for actual results (uncomment when server logic is ready)
                 # conditionalPanel(
                 #   condition = "input.calculate_active_share > 0",
                 #   div(
                 #     style = "margin-top: 20px;",
                 #     
                 #     # Summary statistics
                 #     fluidRow(
                 #       column(4, 
                 #              wellPanel(
                 #                h4("Overall Active Share", style = "margin-top: 0;"),
                 #                h2(textOutput("overall_active_share"), style = "color: #007bff;")
                 #              )
                 #       ),
                 #       column(4,
                 #              wellPanel(
                 #                h4("Number of Holdings", style = "margin-top: 0;"),
                 #                h2(textOutput("num_holdings"), style = "color: #28a745;")
                 #              )
                 #       ),
                 #       column(4,
                 #              wellPanel(
                 #                h4("Tracking Error", style = "margin-top: 0;"),
                 #                h2(textOutput("tracking_error"), style = "color: #dc3545;")
                 #              )
                 #       )
                 #     ),
                 #     
                 #     # Data table with loading spinner
                 #     br(),
                 #     h4("Detailed Holdings Analysis"),
                 #     withSpinner(dataTableOutput("active_share_table"))
                 #   )
                 # )
               )
             )
    )
  )
)