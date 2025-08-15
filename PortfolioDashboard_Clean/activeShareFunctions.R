# This username and password is for bench testing only.  FactSet will lock out if on server....
# for local machine at M&P
username <- 'MAIRSPOWER-1380337'
password <- 'ZvIWYtZwxni7MPhr0Xcfsftvc5Z7XxvY8CQoeu64'

setCash <- function(portfolio) {
  cash_row <- portfolio %>%
    filter(SecType %in% c("MMFUND", "CASH")) %>%
    summarise(
      Date = first(Date),
      PortCode = first(PortCode),
      Account = first(Account),
      CUSIP = "CASH",
      Description = "CASH_EQUIV",
      SecType = "CASH",
      Units = sum(Units, na.rm = TRUE),
      MktVal = sum(MktVal, na.rm = TRUE),
      BookVal = sum(BookVal, na.rm = TRUE)
    )
  
  portfolio <- portfolio %>%
    filter(!SecType %in% c("MMFUND", "CASH")) %>%
    bind_rows(cash_row)
  
  return(portfolio)  
}

mergeLots <- function(portfolio) {
  portfolio_collapsed <- portfolio %>%
    group_by(Description) %>%
    summarise(
      Date = first(Date),
      PortCode = first(PortCode),
      Account = first(Account),
      CUSIP = first(CUSIP),
      SecType = first(SecType),
      Units = sum(Units, na.rm = TRUE),
      BookVal = sum(BookVal, na.rm = TRUE),
      MktVal = sum(MktVal, na.rm = TRUE),
      .groups = "drop"
    )
  
  cols <- names(portfolio_collapsed)
  new_order <- c(cols[2:4], cols[1], cols[5:9])
  setcolorder(portfolio_collapsed, new_order)
  
  return(portfolio_collapsed)  
}

getTickers <- function(portfolio) {
  time_series_endpoint  <- 'https://api.factset.com/formula-api/v1/time-series'
  
  CUSIP <- portfolio$CUSIP
  
  formula <- paste0('P_SYMBOL("LOCAL")')
  request = list(data = list(ids=CUSIP,
                             formulas=list(formula),
                             flatten="Y")) #Use the flatten parameter flatten to return a flat, table-like JSON response model instead of the standard nested JSON response model. 
  # The flattened data model returned with flatten=Y easily converts to a standard R DataFrame.
  
  #3.1b `/formula-api/v1/time-series` - Pull data and view results 
  
  response  <- httr::POST(time_series_endpoint ,authenticate(username,password,type="basic"),body=(request),
                          add_headers(Accept='application/json'),encode='json')
  
  output <- rawToChar(response$content)
  prettify(output, indent = 2)
  json <- fromJSON(output)[['data']]
  df <- data.frame(json) 
  df <- df[ , c(1, 2)]
  names(df)[1] <- 'CUSIP'
  names(df)[2] <- 'Ticker'
  #df <-  df %>%    probably not required, in tidyr library, I think
  #drop_na()
  
  portfolio <- portfolio %>%
    left_join(df, by = "CUSIP") %>%     
    select(Date, PortCode, Account, Ticker, 
           Description, CUSIP, SecType, Units, 
           BookVal, MktVal)             
  
  return(portfolio)
}

getPortfolioSectors <- function(portfolio) {
  time_series_endpoint  <- 'https://api.factset.com/formula-api/v1/time-series'
  
  tickers <- portfolio$Ticker
  
  formula <- paste0('FG_GICS_SECTOR')
  request = list(data = list(ids=tickers,
                             formulas=list(formula),
                             flatten="Y")) #Use the flatten parameter flatten to return a flat, table-like JSON response model instead of the standard nested JSON response model. 
  # The flattened data model returned with flatten=Y easily converts to a standard R DataFrame.
  
  #3.1b `/formula-api/v1/time-series` - Pull data and view results 
  
  response  <- httr::POST(time_series_endpoint ,authenticate(username,password,type="basic"),body=(request),
                          add_headers(Accept='application/json'),encode='json')
  
  output <- rawToChar(response$content)
  prettify(output, indent = 2)
  json <- fromJSON(output)[['data']]
  df <- data.frame(json) 
  names(df)[1] <- 'Ticker'
  names(df)[2] <- 'Sector'
  df <- df %>%
    mutate(Sector = if_else(Ticker == "CASH", "CASH", Sector))
  #df <-  df %>%    probably not required, in tidyr library, I think
  #drop_na()
  
  portfolio <- portfolio %>%
    left_join(df, by = "Ticker") %>%     
    select(Date, PortCode, Account, Ticker, 
           Description, Sector, CUSIP, SecType, Units, 
           BookVal, MktVal)             
  
  return(portfolio)
}

getWeights <- function(portfolio) {
  portfolio <- portfolio %>%
    mutate(Weight = MktVal / sum(MktVal, na.rm = TRUE))
  return(portfolio)
}

getPortfolioData <- function() {
  setwd('C:/Users/asoto/mairsandpower.com/Quant - Data/PortfolioData')
  data.files <- list.files(pattern = 'Account_Holdings')
  data.files <- sort(data.files)
  filename <- tail(data.files, 1)
  portfolio.data <- readRDS(filename)
  return(portfolio.data)
}

getPortfolio <- function(account, portfolio.data) {
  account.data <- portfolio.data[portfolio.data[["PortCode"]] == account, ]
  return(account.data)
}

cleanAccount <- function(account.data) {
  account.data <- setCash(account.data)
  account.data <- mergeLots(account.data)
  account.data <- getTickers(account.data)
  account.data <- getPortfolioSectors(account.data)
  account.data <- getWeights(account.data)
  #removing this out of doubt, don't think we need this
  account.data$CUSIP <- NULL
  account.data <- account.data[ , !(names(account.data) %in% c("SecType", "Units", "BookVal", "MktVal"))]
  return(account.data)
}



getModelSectors <- function(model.data) {
  time_series_endpoint  <- 'https://api.factset.com/formula-api/v1/time-series'
  
  tickers <- model.data$Ticker
  
  formula <- paste0('FG_GICS_SECTOR')
  request = list(data = list(ids=tickers,
                             formulas=list(formula),
                             flatten="Y")) #Use the flatten parameter flatten to return a flat, table-like JSON response model instead of the standard nested JSON response model. 
  # The flattened data model returned with flatten=Y easily converts to a standard R DataFrame.
  
  #3.1b `/formula-api/v1/time-series` - Pull data and view results 
  
  response  <- httr::POST(time_series_endpoint ,authenticate(username,password,type="basic"),body=(request),
                          add_headers(Accept='application/json'),encode='json')
  
  output <- rawToChar(response$content)
  prettify(output, indent = 2)
  json <- fromJSON(output)[['data']]
  df <- data.frame(json) 
  names(df)[1] <- 'Ticker'
  names(df)[2] <- 'Sector'
  df <- df %>%
    mutate(Sector = if_else(Ticker == "CASH", "CASH", Sector))
  #df <-  df %>%    probably not required, in tidyr library, I think
  #drop_na()
  
  model.data <- merge(model.data, df, by = "Ticker", all.x = TRUE)
  col_order <- names(model.data)
  new_order <- c("Date", "Ticker", "Description", "Sector", setdiff(col_order, c("Date", "Ticker", "Description", "Sector")))
  model.data <- model.data[ , new_order]
  return(model.data)
}


getModelData <- function() {
  setwd('C:/Users/asoto/mairsandpower.com/Quant - Models')
  data.files <- list.files(pattern = 'Advisory Model Comparison')
  data.files <- sort(data.files)
  filename <- tail(data.files, 1)
  date <- sub("_.*", "", filename) 
  date <- format(as.Date(date, format = "%Y.%m.%d"), "%Y-%m-%d")
  model.data <- read.xlsx(filename, sheet=1, startRow = 3, colNames = TRUE)
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
  return(model.data)
}

getModel <- function(advisor, model.data) {
  model.data <- model.data[ , c(1:4, which(names(model.data) == advisor))]
  model.data <- model.data[ model.data[[ncol(model.data)]] != 0, ]
  return(model.data)
}

getActiveShare <- function(account.data, model.data) {
  portCode <- unique(account.data$PortCode)
  accountName <- unique(account.data$Account)
  accountDate <- unique(account.data$Date)
  modelDate <- unique(model.data$Date)
  merged <- merge(account.data, model.data, by = "Ticker", all = TRUE)
  merged$Weight[is.na(merged$Weight)] <- 0
  modelColName <- names(model.data)[ncol(model.data)]
  merged[[modelColName]][is.na(merged[[modelColName]])] <- 0
  result <- merged[ , c("Ticker", "Description.x", "Sector.x", "Weight", modelColName)]
  
  names(result)[names(result) == "Description.x"] <- "Description"
  names(result)[names(result) == "Sector.x"] <- "Sector"
  names(result)[names(result) == "Weight"] <- portCode  # rename Weight to portCode value
  
  num_cols <- sapply(result, is.numeric)
  result[ , num_cols] <- lapply(result[ , num_cols], function(x) round(x, 4))
  
  num_col_names <- names(result)[num_cols]
  n <- length(num_col_names)
  col1 <- num_col_names[n - 1]
  col2 <- num_col_names[n]
  
  result$`Active Share` <- abs(result[[col1]] - result[[col2]]) / 2
  activeShareValue <- sum(result$`Active Share`, na.rm = TRUE)
  result <- result[order(result$Sector), ]
  cash_rows <- result$Sector == "CASH"
  result <- rbind(result[cash_rows, ], result[!cash_rows, ])
  
  return(list(
    accountDate = accountDate,
    accountName = accountName,
    modelDate = modelDate,
    activeShareValue = activeShareValue,
    result = result
  ))
}

