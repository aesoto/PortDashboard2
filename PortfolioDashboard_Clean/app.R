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
  # Header - Replace your existing header fluidRow with this updated version
  fluidRow(
    column(12,
           div(class = "content-header",
               style = "display: flex; justify-content: space-between; align-items: center;",
               h1("Portfolio Analytics", style = "margin: 0; flex-grow: 1;"),
               img(src = "logo.jpg", 
                   style = "height: 60px; max-width: 200px; object-fit: contain;",
                   alt = "Company Logo")
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
                                  "Calculate",
                                  class = "btn-primary",
                                  style = "width: 100%;padding: 8px 12px; white-space: nowrap;"),
                     
                     br(), br(),
                     uiOutput("calculation_status")
                 )
               ),
               
               mainPanel(
                 width = 9,
                 h3("Active Share Results"),
                 
                 # Test output
                 # div(
                 #  style = "margin: 20px 0; padding: 10px; background-color: #f8f9fa;",
                 # h5("Debug Info:"),
                 # verbatimTextOutput("test_output")
                 # ),
                 
                 # Results
                 uiOutput("active_share_summary"),
                 br(),
                 DT::dataTableOutput("active_share_table")
               )
             )
    )
  )
)

# Line 141
# Server
server <- function(input, output, session) {
  
  cat("=== SERVER STARTED ===\n")
  
  values <- reactiveValues(
    portfolio_data = NULL,
    model_data = NULL,
    available_accounts = NULL,
    available_models = NULL,
    available_dates = NULL,
    active_share_results = NULL,
    calculation_in_progress = FALSE,
    error_message = NULL
  )
  
  # Enhanced function to get available portfolio dates
  getPortfolioDates <- function() {
    tryCatch({
      original_dir <- getwd()
      setwd('C:/Users/asoto/mairsandpower.com/Quant - Data/PortfolioData')
      data.files <- list.files(pattern = 'Account_Holdings')
      setwd(original_dir)
      
      if (length(data.files) == 0) {
        return(NULL)
      }
      
      # Extract dates from filenames
      dates <- sapply(data.files, function(filename) {
        date_match <- regmatches(filename, regexpr("\\d{8}", filename))
        if (length(date_match) > 0) {
          date_str <- paste0(substr(date_match, 1, 4), "-", 
                             substr(date_match, 5, 6), "-", 
                             substr(date_match, 7, 8))
          return(as.Date(date_str))
        }
        return(NA)
      }, USE.NAMES = FALSE)
      
      dates <- as.Date(dates[!is.na(dates)], origin = "1970-01-01")
      return(sort(dates, decreasing = TRUE))
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Enhanced getPortfolioData function with date parameter
  getPortfolioDataByDate <- function(selected_date) {
    tryCatch({
      original_dir <- getwd()
      setwd('C:/Users/asoto/mairsandpower.com/Quant - Data/PortfolioData')
      data.files <- list.files(pattern = 'Account_Holdings')
      
      target_file <- NULL
      for (file in data.files) {
        date_match <- regmatches(file, regexpr("\\d{8}", file))
        if (length(date_match) > 0) {
          date_str <- paste0(substr(date_match, 1, 4), "-", 
                             substr(date_match, 5, 6), "-", 
                             substr(date_match, 7, 8))
          file_date <- as.Date(date_str)
          if (file_date == selected_date) {
            target_file <- file
            break
          }
        }
      }
      
      setwd(original_dir)
      
      if (is.null(target_file)) {
        return(NULL)
      }
      
      setwd('C:/Users/asoto/mairsandpower.com/Quant - Data/PortfolioData')
      portfolio_data <- readRDS(target_file)
      setwd(original_dir)
      
      return(portfolio_data)
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Function to get available model dates
  getModelDates <- function() {
    tryCatch({
      original_dir <- getwd()
      setwd('C:/Users/asoto/mairsandpower.com/Quant - Models')
      data.files <- list.files(pattern = 'Advisory Model Comparison')
      setwd(original_dir)
      
      if (length(data.files) == 0) {
        return(NULL)
      }
      
      dates <- sapply(data.files, function(filename) {
        date_match <- regmatches(filename, regexpr("\\d{4}\\.\\d{2}\\.\\d{2}", filename))
        if (length(date_match) > 0) {
          return(as.Date(date_match, format = "%Y.%m.%d"))
        }
        return(NA)
      }, USE.NAMES = FALSE)
      
      dates <- as.Date(dates[!is.na(dates)], origin = "1970-01-01")
      return(sort(dates, decreasing = TRUE))
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Line 254
  # Enhanced getModelData function with date parameter
  getModelDataByDate <- function(selected_date) {
    tryCatch({
      original_dir <- getwd()
      setwd('C:/Users/asoto/mairsandpower.com/Quant - Models')
      data.files <- list.files(pattern = 'Advisory Model Comparison')
      
      target_file <- NULL
      best_date <- NULL
      
      for (file in data.files) {
        date_match <- regmatches(file, regexpr("\\d{4}\\.\\d{2}\\.\\d{2}", file))
        if (length(date_match) > 0) {
          file_date <- as.Date(date_match, format = "%Y.%m.%d")
          
          if (file_date <= selected_date) {
            if (is.null(best_date) || file_date > best_date) {
              best_date <- file_date
              target_file <- file
            }
          }
        }
      }
      
      setwd(original_dir)
      
      if (is.null(target_file)) {
        return(NULL)
      }
      
      setwd('C:/Users/asoto/mairsandpower.com/Quant - Models')
      
      date_match <- regmatches(target_file, regexpr("\\d{4}\\.\\d{2}\\.\\d{2}", target_file))
      date <- format(as.Date(date_match, format = "%Y.%m.%d"), "%Y-%m-%d")
      
      model.data <- read.xlsx(target_file, sheet=1, startRow = 3, colNames = TRUE)
      model.data <- model.data[ , -3]
      model.data <- model.data[!is.na(model.data[[2]]), ]
      names(model.data)[1:2] <- c("Description", "Ticker")
      model.data$Description[model.data$Description == "All Cash"] <- "CASH_EQUIV"
      model.data$Ticker[model.data$Description == "CASH_EQUIV"] <- "CASH"
      model.data$Description <- trimws(model.data$Description)
      model.data <- model.data[ , !grepl("\\.1$", names(model.data))]
      model.data[ , 3:ncol(model.data)] <- lapply(model.data[ , 3:ncol(model.data)], as.numeric)
      model.data[ , 3:ncol(model.data)] <- model.data[ , 3:ncol(model.data)] / 100
      model.data <- model.data[ , c("Ticker", setdiff(names(model.data), "Ticker"))]
      model.data <- cbind(Date = date, model.data)
      model.data$Ticker <- toupper(model.data$Ticker)
      model.data <- getModelSectors(model.data)
      
      setwd(original_dir)
      return(model.data)
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Initialize data when app starts
  observe({
    cat("=== INITIALIZING DATA ===\n")
    values$available_dates <- getPortfolioDates()
    
    if (!is.null(values$available_dates)) {
      updateDateInput(session, "date",
                      value = max(values$available_dates),
                      min = min(values$available_dates),
                      max = max(values$available_dates))
      cat("Portfolio dates loaded:", length(values$available_dates), "dates\n")
    } else {
      cat("No portfolio dates found\n")
    }
  })
  
  # Line 328
  # Load portfolio and model data when date changes
  observeEvent(input$date, {
    cat("=== DATE CHANGED ===", as.character(input$date), "\n")
    
    if (!is.null(input$date)) {
      # Load portfolio data
      tryCatch({
        values$portfolio_data <- getPortfolioDataByDate(input$date)
        
        if (!is.null(values$portfolio_data)) {
          account_codes <- unique(values$portfolio_data$PortCode)
          values$available_accounts <- setNames(account_codes, account_codes)
          
          updateSelectInput(session, "account",
                            choices = values$available_accounts,
                            selected = if(length(account_codes) > 0) account_codes[1] else NULL)
          
          cat("Portfolio data loaded for", length(account_codes), "accounts\n")
        } else {
          cat("No portfolio data found for date\n")
          updateSelectInput(session, "account", choices = list(), selected = NULL)
        }
      }, error = function(e) {
        cat("Error loading portfolio data:", e$message, "\n")
        updateSelectInput(session, "account", choices = list(), selected = NULL)
      })
      
      # Load model data
      tryCatch({
        model_dates <- getModelDates()
        if (!is.null(model_dates)) {
          closest_date <- max(model_dates[model_dates <= input$date])
          values$model_data <- getModelDataByDate(closest_date)
          
          if (!is.null(values$model_data)) {
            model_cols <- names(values$model_data)
            excluded_cols <- c("Date", "Ticker", "Description", "Sector")
            available_models <- setdiff(model_cols, excluded_cols)
            
            values$available_models <- setNames(available_models, available_models)
            
            updateSelectInput(session, "model",
                              choices = values$available_models,
                              selected = if(length(available_models) > 0) available_models[1] else NULL)
            
            cat("Model data loaded with", length(available_models), "models\n")
          }
        }
      }, error = function(e) {
        cat("Error loading model data:", e$message, "\n")
        updateSelectInput(session, "model", choices = list(), selected = NULL)
      })
    }
  })
  
  # Active Share calculation (triggered by action button)
  observeEvent(input$calculate_active_share, {
    
    cat("=== CALCULATION STARTED ===\n")
    cat("Account:", input$account, "\n")
    cat("Model:", input$model, "\n")
    cat("Date:", input$date, "\n")
    
    # Validate inputs
    if (is.null(input$account) || input$account == "" ||
        is.null(input$model) || input$model == "" ||
        is.null(values$portfolio_data) || is.null(values$model_data)) {
      cat("Validation failed\n")
      values$error_message <- "Please select all parameters"
      return()
    }
    
    # Set calculation in progress
    values$calculation_in_progress <- TRUE
    values$error_message <- NULL
    
    tryCatch({
      # Get portfolio data for selected account
      cat("Step 1: Getting portfolio data...\n")
      account_data <- getPortfolio(input$account, values$portfolio_data)
      cat("Portfolio data retrieved, rows:", nrow(account_data), "\n")
      
      cat("Step 2: Cleaning account data...\n")
      account_data <- cleanAccount(account_data)
      cat("Account data cleaned, rows:", nrow(account_data), "\n")
      
      cat("Step 3: Preparing model data...\n")
      # Filter model data for selected model (inline getModel logic)
      if (!input$model %in% names(values$model_data)) {
        available_models <- setdiff(names(values$model_data), c("Date", "Ticker", "Description", "Sector"))
        stop(paste("Model not found. Available:", paste(available_models, collapse = ", ")))
      }
      
      model_data <- values$model_data[, c(1:4, which(names(values$model_data) == input$model))]
      model_data <- model_data[model_data[[ncol(model_data)]] != 0, ]
      cat("Model data prepared, rows:", nrow(model_data), "\n")
      
      cat("Step 4: Calculating Active Share...\n")
      # Calculate Active Share
      active_share_result <- getActiveShare(account_data, model_data)
      values$active_share_results <- active_share_result
      
      # Calculation complete
      values$calculation_in_progress <- FALSE
      cat("=== CALCULATION COMPLETED SUCCESSFULLY ===\n")
      
    }, error = function(e) {
      cat("=== ERROR OCCURRED ===\n")
      cat("Error message:", e$message, "\n")
      values$calculation_in_progress <- FALSE
      values$error_message <- paste("Calculation error:", e$message)
    })
  })
  
  # Test output - this should ALWAYS work
  output$test_output <- renderText({
    paste("Button clicks:", input$calculate_active_share, 
          "| Portfolio data:", !is.null(values$portfolio_data),
          "| Model data:", !is.null(values$model_data),
          "| Results:", !is.null(values$active_share_results),
          "| Time:", Sys.time())
  })
  
  # Line 452
  # Status indicator
  output$calculation_status <- renderUI({
    if (!is.null(values$error_message)) {
      div(style = "color: red;", "Error: ", values$error_message)
    } else if (values$calculation_in_progress) {
      div(style = "color: blue;", "Calculating...")
    } else {
      div(style = "color: green;", "Ready")
    }
  })
  
  # Simple summary
  output$active_share_summary <- renderUI({
    if (is.null(values$active_share_results)) {
      if (input$calculate_active_share == 0) {
        return(div("Ready to calculate Active Share"))
      } else {
        return(div("Calculation completed but no results available"))
      }
    } else {
      result <- values$active_share_results
      
      # Format dates as dd-month-YYYY
      account_date_formatted <- format(as.Date(result$accountDate), "%d-%B-%Y")
      model_date_formatted <- format(as.Date(result$modelDate), "%d-%B-%Y")
      
      return(div(
        h4("Active Share: ", paste0(round(result$activeShareValue * 100, 2), "%")),
        p("Account Date: ", account_date_formatted),
        p("Model Date: ", model_date_formatted),
        p("Holdings: ", nrow(result$result))
      ))
    }
  })
  
  # Line 488
  # Data table with professional formatting
  output$active_share_table <- renderDT({
    if (is.null(values$active_share_results)) {
      return(datatable(data.frame(Message = "No results available"), 
                       options = list(dom = 't'), rownames = FALSE))
    } else {
      result_table <- values$active_share_results$result
      
      # Clean up data - only remove rows where Ticker is NA
      valid_rows <- !is.na(result_table$Ticker) & result_table$Ticker != ""
      result_table <- result_table[valid_rows, ]
      
      # Replace NAs with 0 for calculations
      numeric_cols <- sapply(result_table, is.numeric)
      result_table[numeric_cols][is.na(result_table[numeric_cols])] <- 0
      
      # Create sector-grouped table
      display_rows <- list()  # Use list instead of rbind for better performance
      row_counter <- 1
      
      # Get unique sectors, with CASH first
      sectors <- unique(result_table$Sector)
      sectors <- c("CASH", sectors[sectors != "CASH"])
      
      for (sector in sectors) {
        sector_data <- result_table[result_table$Sector == sector, ]
        
        if (nrow(sector_data) > 0) {
          # Add sector header (except for CASH)
          if (sector != "CASH") {
            header_row <- sector_data[1, !names(sector_data) %in% "Sector"]  # Use existing structure
            header_row[1, ] <- NA  # Clear all values
            header_row$Ticker <- paste0("<b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", toupper(sector), "</b>")
            header_row$Description <- ""
            for (col in names(header_row)[3:ncol(header_row)]) {
              header_row[[col]] <- ""
            }
            display_rows[[row_counter]] <- header_row
            row_counter <- row_counter + 1
          }
          
          # Add sector data (without Sector column)
          sector_display <- sector_data[, !names(sector_data) %in% "Sector"]
          
          # Format numeric columns as percentages
          for (col in names(sector_display)) {
            if (is.numeric(sector_display[[col]])) {
              sector_display[[col]] <- sprintf("%.2f%%", sector_display[[col]] * 100)
            }
          }
          
          # Add all sector rows
          for (i in 1:nrow(sector_display)) {
            display_rows[[row_counter]] <- sector_display[i, ]
            row_counter <- row_counter + 1
          }
          
          # Add subtotal row (except for CASH)
          if (sector != "CASH" && nrow(sector_data) > 1) {
            subtotal_row <- sector_data[1, !names(sector_data) %in% "Sector"]  # Use existing structure
            subtotal_row[1, ] <- NA  # Clear all values
            subtotal_row$Ticker <- ""
            subtotal_row$Description <- "<div style='text-align: right;'><b>Subtotal:</b></div>"
            
            # Calculate subtotals for ALL numeric columns (including Active Share)
            original_numeric_cols <- sapply(sector_data, is.numeric)
            for (col in names(sector_data)[original_numeric_cols]) {
              subtotal_value <- sum(sector_data[[col]], na.rm = TRUE)
              subtotal_row[[col]] <- paste0("<b>", sprintf("%.2f%%", subtotal_value * 100), "</b>")
            }
            
            display_rows[[row_counter]] <- subtotal_row
            row_counter <- row_counter + 1
          }
          
          # Add blank row for spacing (except after CASH)
          if (sector != "CASH") {
            blank_row <- sector_data[1, !names(sector_data) %in% "Sector"]  # Use existing structure
            blank_row[1, ] <- NA  # Clear all values
            blank_row$Ticker <- ""
            blank_row$Description <- ""
            for (col in names(blank_row)[3:ncol(blank_row)]) {
              blank_row[[col]] <- ""
            }
            display_rows[[row_counter]] <- blank_row
            row_counter <- row_counter + 1
          }
        }
      }
      
      # Combine all rows into final table
      display_table <- do.call(rbind, display_rows)
      
      cat("Final display table created with", nrow(display_table), "rows\n")
      
      # Return the table with minimal options (same as working version)
      return(datatable(display_table, 
                       options = list(
                         scrollY = "600px",
                         scrollX = TRUE,
                         paging = FALSE,
                         searching = TRUE,
                         info = FALSE,
                         dom = 'ft'
                       ), 
                       rownames = FALSE,
                       escape = FALSE))
    }
  })
  
  cat("=== SERVER SETUP COMPLETE ===\n")
}

# Run the app
shinyApp(ui = ui, server = server, 
         options = list(launch.browser = function(url) {
           utils::browseURL(url)
         }, port = getOption("shiny.port"), 
         width = 1400, height = 800))