# Enhanced Server logic for Portfolio Dashboard Shiny App
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

# Define server function properly
server <- function(input, output, session) {
  
  cat("=== SERVER FUNCTION STARTED ===\n")
  
  # Reactive values to store data
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
  
  cat("=== REACTIVE VALUES INITIALIZED ===\n")
  
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
        # First try: YYYYMMDD format (no separators)
        date_match <- regmatches(filename, regexpr("\\d{8}", filename))
        if (length(date_match) > 0) {
          # Convert YYYYMMDD to YYYY-MM-DD format
          date_str <- paste0(substr(date_match, 1, 4), "-", 
                             substr(date_match, 5, 6), "-", 
                             substr(date_match, 7, 8))
          return(as.Date(date_str))
        }
        
        # Second try: YYYY-MM-DD, YYYY.MM.DD, or YYYY_MM_DD format
        date_match <- regmatches(filename, regexpr("\\d{4}[._-]\\d{2}[._-]\\d{2}", filename))
        if (length(date_match) > 0) {
          # Convert to standard date format
          date_str <- gsub("[._-]", "-", date_match)
          return(as.Date(date_str))
        }
        
        return(NA)
      }, USE.NAMES = FALSE)  # Remove names from sapply result
      
      # Convert numeric dates back to Date objects and remove NAs
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
      
      # Find file matching the selected date
      target_file <- NULL
      for (file in data.files) {
        # First try: YYYYMMDD format (no separators)
        date_match <- regmatches(file, regexpr("\\d{8}", file))
        if (length(date_match) > 0) {
          # Convert YYYYMMDD to YYYY-MM-DD format
          date_str <- paste0(substr(date_match, 1, 4), "-", 
                             substr(date_match, 5, 6), "-", 
                             substr(date_match, 7, 8))
          file_date <- as.Date(date_str)
          if (file_date == selected_date) {
            target_file <- file
            break
          }
        }
        
        # Second try: YYYY-MM-DD, YYYY.MM.DD, or YYYY_MM_DD format
        date_match <- regmatches(file, regexpr("\\d{4}[._-]\\d{2}[._-]\\d{2}", file))
        if (length(date_match) > 0) {
          file_date <- as.Date(gsub("[._-]", "-", date_match))
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
      
      # Extract dates from filenames
      dates <- sapply(data.files, function(filename) {
        # Extract date from filename - assuming format like "YYYY.MM.DD_Advisory Model Comparison.xlsx"
        date_match <- regmatches(filename, regexpr("\\d{4}\\.\\d{2}\\.\\d{2}", filename))
        if (length(date_match) > 0) {
          # Convert YYYY.MM.DD to Date
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
  
  # Enhanced getModelData function with date parameter
  getModelDataByDate <- function(selected_date) {
    tryCatch({
      original_dir <- getwd()
      setwd('C:/Users/asoto/mairsandpower.com/Quant - Models')
      data.files <- list.files(pattern = 'Advisory Model Comparison')
      
      # Find file matching the selected date or closest prior date
      target_file <- NULL
      best_date <- NULL
      
      for (file in data.files) {
        date_match <- regmatches(file, regexpr("\\d{4}\\.\\d{2}\\.\\d{2}", file))
        if (length(date_match) > 0) {
          file_date <- as.Date(date_match, format = "%Y.%m.%d")
          
          # Check if this file date is on or before the selected date
          if (file_date <= selected_date) {
            # Check if this is the best (most recent) date so far
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
      
      # Load and process the model data
      setwd('C:/Users/asoto/mairsandpower.com/Quant - Models')
      
      # Extract date from filename for the data
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
  
  # Function to find closest model date (enhanced)
  findClosestModelDate <- function(portfolio_date) {
    model_dates <- getModelDates()
    if (is.null(model_dates) || length(model_dates) == 0) {
      return(NULL)
    }
    
    portfolio_date <- as.Date(portfolio_date)
    
    # Find dates that are on or before the portfolio date
    valid_dates <- model_dates[model_dates <= portfolio_date]
    
    if (length(valid_dates) == 0) {
      return(NULL)
    }
    
    # Return the closest (most recent) date
    return(max(valid_dates))
  }
  
  # Initialize data when app starts
  observe({
    # Get available dates
    values$available_dates <- getPortfolioDates()
    
    if (!is.null(values$available_dates)) {
      # Update date choices in UI
      updateDateInput(session, "date",
                      value = max(values$available_dates),
                      min = min(values$available_dates),
                      max = max(values$available_dates))
    }
  })
  
  # Load portfolio and model data when date changes
  observeEvent(input$date, {
    if (!is.null(input$date)) {
      # Load portfolio data
      tryCatch({
        values$portfolio_data <- getPortfolioDataByDate(input$date)
        
        if (!is.null(values$portfolio_data)) {
          # Get unique account codes
          account_codes <- unique(values$portfolio_data$PortCode)
          values$available_accounts <- setNames(account_codes, account_codes)
          
          # Update account dropdown
          updateSelectInput(session, "account",
                            choices = values$available_accounts,
                            selected = if(length(account_codes) > 0) account_codes[1] else NULL)
          
          values$error_message <- NULL
        } else {
          values$error_message <- "No portfolio data available for selected date"
          updateSelectInput(session, "account", choices = list(), selected = NULL)
        }
      }, error = function(e) {
        values$error_message <- paste("Error loading portfolio data:", e$message)
        updateSelectInput(session, "account", choices = list(), selected = NULL)
      })
      
      # Load model data for the selected date
      tryCatch({
        # Find the closest model date
        closest_model_date <- findClosestModelDate(input$date)
        
        if (!is.null(closest_model_date)) {
          values$model_data <- getModelDataByDate(closest_model_date)
          
          if (!is.null(values$model_data)) {
            # Extract model names (column names excluding Date, Ticker, Description, Sector)
            model_cols <- names(values$model_data)
            excluded_cols <- c("Date", "Ticker", "Description", "Sector")
            available_models <- setdiff(model_cols, excluded_cols)
            
            values$available_models <- setNames(available_models, available_models)
            
            # Update model dropdown
            updateSelectInput(session, "model",
                              choices = values$available_models,
                              selected = if(length(available_models) > 0) available_models[1] else NULL)
            
            # Show notification if model date is different from portfolio date
            if (as.Date(closest_model_date) != as.Date(input$date)) {
              showNotification(
                paste("Using model data from", closest_model_date, "- closest available date"),
                type = "message", duration = 5
              )
            }
          } else {
            values$error_message <- "Error loading model data"
            updateSelectInput(session, "model", choices = list(), selected = NULL)
          }
        } else {
          values$error_message <- "No model data available for selected date"
          updateSelectInput(session, "model", choices = list(), selected = NULL)
        }
      }, error = function(e) {
        values$error_message <- paste("Error loading model data:", e$message)
        updateSelectInput(session, "model", choices = list(), selected = NULL)
      })
    }
  })
  
  # Validation reactive
  input_valid <- reactive({
    cat("=== VALIDATION CHECK ===\n")
    cat("Account:", input$account, "(not null:", !is.null(input$account), ", not empty:", input$account != "", ")\n")
    cat("Model:", input$model, "(not null:", !is.null(input$model), ", not empty:", input$model != "", ")\n")
    cat("Date:", input$date, "(not null:", !is.null(input$date), ")\n")
    cat("Portfolio data available:", !is.null(values$portfolio_data), "\n")
    cat("Model data available:", !is.null(values$model_data), "\n")
    
    result <- !is.null(input$account) && 
      !is.null(input$model) && 
      !is.null(input$date) &&
      input$account != "" && 
      input$model != "" &&
      !is.null(values$portfolio_data) &&
      !is.null(values$model_data)
    
    cat("Validation result:", result, "\n")
    cat("=== END VALIDATION ===\n")
    
    return(result)
  })
  
  # Status indicator for the Active Share tab
  output$calculation_status <- renderUI({
    if (!is.null(values$error_message)) {
      div(
        style = "text-align: center;",
        class = "status-error",
        icon("exclamation-triangle"),
        br(),
        values$error_message
      )
    } else if (values$calculation_in_progress) {
      div(
        style = "text-align: center;",
        class = "status-info",
        icon("spinner", class = "fa-spin"),
        br(),
        "Calculating..."
      )
    } else if (input_valid() && input$calculate_active_share > 0) {
      div(
        style = "text-align: center;",
        class = "status-success",
        icon("check-circle"),
        br(),
        "Calculation Complete"
      )
    } else if (!input_valid()) {
      div(
        style = "text-align: center;",
        class = "status-error",
        icon("exclamation-triangle"),
        br(),
        "Please select all parameters"
      )
    }
  })
  
  # Active Share calculation (triggered by action button)
  observeEvent(input$calculate_active_share, {
    
    cat("=== CALCULATION STARTED ===\n")
    cat("Button clicked count:", input$calculate_active_share, "\n")
    
    # Validate inputs first
    if (!input_valid()) {
      cat("INPUT VALIDATION FAILED\n")
      if (is.null(values$portfolio_data)) {
        cat("Portfolio data is NULL\n")
        showNotification("No portfolio data available for selected date.", type = "error")
      } else {
        cat("Other validation failed\n")
        showNotification("Please select all required parameters.", type = "error")
      }
      return()
    }
    
    cat("INPUT VALIDATION PASSED\n")
    cat("Account:", input$account, "\n")
    cat("Model:", input$model, "\n")
    cat("Date:", input$date, "\n")
    
    # Set calculation in progress
    values$calculation_in_progress <- TRUE
    values$error_message <- NULL
    
    tryCatch({
      cat("Step 1: Getting portfolio data...\n")
      # Get portfolio data for selected account
      account_data <- getPortfolio(input$account, values$portfolio_data)
      cat("Portfolio data retrieved, rows:", nrow(account_data), "\n")
      
      cat("Step 2: Cleaning account data...\n")
      account_data <- cleanAccount(account_data)
      cat("Account data cleaned, rows:", nrow(account_data), "\n")
      
      cat("Step 3: Preparing model data...\n")
      # Get model data for selected advisor - using inline code instead of getModel function
      # Verify the model column exists in model data
      if (!input$model %in% names(values$model_data)) {
        available_models <- setdiff(names(values$model_data), c("Date", "Ticker", "Description", "Sector"))
        error_msg <- paste("Model '", input$model, "' not found. Available models: ", 
                           paste(available_models, collapse = ", "))
        cat("ERROR:", error_msg, "\n")
        stop(error_msg)
      }
      
      cat("Model column found in data\n")
      cat("Model data columns:", names(values$model_data), "\n")
      cat("Model data rows:", nrow(values$model_data), "\n")
      
      # Filter model data for selected advisor (inline getModel logic)
      model_data <- values$model_data[, c(1:4, which(names(values$model_data) == input$model))]
      cat("Model data after column selection, rows:", nrow(model_data), "\n")
      
      model_data <- model_data[model_data[[ncol(model_data)]] != 0, ]
      cat("Model data after filtering zeros, rows:", nrow(model_data), "\n")
      
      cat("Step 4: Calculating Active Share...\n")
      # Calculate Active Share
      active_share_result <- getActiveShare(account_data, model_data)
      cat("Active Share calculation completed\n")
      
      # Store results
      values$active_share_results <- active_share_result
      cat("Results stored in values\n")
      
      # Calculation complete
      values$calculation_in_progress <- FALSE
      cat("=== CALCULATION COMPLETED SUCCESSFULLY ===\n")
      
      showNotification("Active Share calculation completed successfully!", 
                       type = "message", duration = 3)
      
    }, error = function(e) {
      cat("=== ERROR OCCURRED ===\n")
      cat("Error message:", e$message, "\n")
      cat("Error call:", deparse(e$call), "\n")
      
      values$calculation_in_progress <- FALSE
      values$error_message <- paste("Calculation error:", e$message)
      showNotification(paste("Error during calculation:", e$message), type = "error")
    })
  })
  
  # Display Active Share results - SIMPLIFIED VERSION
  output$active_share_summary <- renderUI({
    cat("=== RENDERING SUMMARY (SIMPLIFIED) ===\n")
    
    # Just return something simple to test
    if (is.null(values$active_share_results)) {
      return(div("No results yet - waiting for calculation"))
    }
    
    return(div("Results are available! Active Share calculation completed."))
  })
  
  # Display Active Share table
  output$active_share_table <- renderDT({
    cat("=== RENDERING TABLE ===\n")
    cat("Button clicks:", input$calculate_active_share, "\n")
    cat("Results available:", !is.null(values$active_share_results), "\n")
    
    # Don't show table if no calculation done
    if (input$calculate_active_share == 0) {
      cat("No calculation done yet\n")
      return(NULL)
    }
    
    if (is.null(values$active_share_results)) {
      cat("No results available for table\n")
      return(datatable(data.frame(Message = "No results available"), 
                       options = list(dom = 't'), rownames = FALSE))
    }
    
    tryCatch({
      result_table <- values$active_share_results$result
      
      # Debug: Print table info
      cat("Table columns:", names(result_table), "\n")
      cat("Table dimensions:", dim(result_table), "\n")
      cat("First few rows:\n")
      print(head(result_table, 3))
      
      # Create a simple table first - no formatting
      simple_table <- datatable(
        result_table,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip'  # Add filtering and info
        ),
        rownames = FALSE
      )
      
      cat("Table created successfully\n")
      return(simple_table)
      
    }, error = function(e) {
      cat("Error in active_share_table:", e$message, "\n")
      # Return a simple error table
      return(datatable(data.frame(Error = paste("Error displaying table:", e$message)), 
                       options = list(dom = 't'), rownames = FALSE))
    })
  }, server = FALSE)
  
  # Simple test output
  output$test_output <- renderText({
    cat("=== TEST OUTPUT CALLED ===\n")
    paste("Button clicks:", input$calculate_active_share, 
          "| Results available:", !is.null(values$active_share_results),
          "| Time:", Sys.time())
  })
  
  cat("=== SERVER FUNCTION COMPLETED ===\n")
}

# Return the server function
server

# Debug outputs (temporary - remove after troubleshooting)
output$debug_button_count <- renderText({
  input$calculate_active_share
})

output$debug_results_available <- renderText({
  !is.null(values$active_share_results)
})

output$debug_calculation_progress <- renderText({
  values$calculation_in_progress
})
}