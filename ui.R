# Portfolio Dashboard 2
# Summer 20025 version
# UI for Portfolio Dashboard Shiny App

# UI for Portfolio Dashboard Shiny App

library(shiny)

fluidPage(
  # Custom header with title and logo
  fluidRow(
    column(8,
           h1("Portfolio Analytics", style = "margin-top: 15px; margin-bottom: 20px;")
    ),
    column(4,
           div(style = "text-align: right; margin-top: 10px;",
               img(src = "logo.jpg", 
                   height = "60px",
                   style = "max-width: 100%;")
           )
    )
  ),
  
  # Create tabset panel for different sections
  tabsetPanel(
    # First tab: Active Share
    tabPanel("Active Share",
             sidebarLayout(
               # Sidebar panel for inputs
               sidebarPanel(
                 width = 3,
                 h4("Parameters"),
                 
                 # Account dropdown
                 selectInput("account",
                             label = "Account:",
                             choices = NULL,  # Will be populated from server
                             selected = NULL),
                 
                 # Model dropdown  
                 selectInput("model",
                             label = "Model:",
                             choices = NULL,  # Will be populated from server
                             selected = NULL),
                 
                 # Date input with calendar widget
                 dateInput("date",
                           label = "Date:",
                           value = Sys.Date(),  # Default to today's date
                           format = "yyyy-mm-dd")
               ),
               
               # Main panel for results
               mainPanel(
                 width = 9,
                 h3("Active Share Results"),
                 
                 # Placeholder for data table output
                 div(
                   style = "margin-top: 20px;",
                   p("Results will be displayed here once parameters are selected and calculations are run."),
                   
                   # This is where your data table will go
                   # dataTableOutput("active_share_table")
                 )
               )
             )
    )
    
    # Additional tabs can be added here later, for example:
    # tabPanel("Performance Attribution", ...),
    # tabPanel("Risk Analysis", ...)
  )
)