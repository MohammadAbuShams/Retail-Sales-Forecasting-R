# ============================================================
# PROJECT 1 - TIME SERIES FORECASTING
# ============================================================

# Install these packages ONLY the first time if needed:
# install.packages("readxl")
# install.packages("writexl")

library(readxl)
library(writexl)


# ============================================================
# 1. LOAD DATA FROM EXCEL
# ============================================================

sales_data <- read_excel(
  "project1.xlsx",
  sheet = "Data"
)

# Rename the first three columns
names(sales_data)[1:3] <- c("t", "Month", "Sales")

# Make sure Month and Sales have the correct data type
sales_data$Month <- as.Date(sales_data$Month)
sales_data$Sales <- as.numeric(sales_data$Sales)

# Sort data chronologically
sales_data <- sales_data[order(sales_data$Month), ]

# Re-create period number after sorting
sales_data$t <- 1:nrow(sales_data)

# View original data
View(sales_data)

# Check number of observations
print(nrow(sales_data))


# ============================================================
# 2. CREATE TIME SERIES AND PLOT
# ============================================================

# Detect starting year and month automatically
start_year <- as.numeric(
  format(min(sales_data$Month), "%Y")
)

start_month <- as.numeric(
  format(min(sales_data$Month), "%m")
)

# Create monthly time series
sales_ts <- ts(
  sales_data$Sales,
  start = c(start_year, start_month),
  frequency = 12
)

# Plot original monthly sales
plot(
  sales_data$Month,
  sales_data$Sales,
  type = "o",
  main = "Monthly Retail Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "blue",
  lwd = 2,
  pch = 16,
  xaxt = "n"
)

# Show years on X-axis
axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

grid(
  col = "lightgray",
  lty = "dotted"
)


# ============================================================
# 3. NAIVE METHOD
# ============================================================

# Naive:
# Forecast for current month = Actual sales of previous month
sales_data$Naive <- c(
  NA,
  sales_data$Sales[1:(nrow(sales_data) - 1)]
)

# Detailed table for checking calculations
# This does NOT need to be placed in the main report
View(
  sales_data[, c(
    "t",
    "Month",
    "Sales",
    "Naive"
  )]
)


# ------------------------------------------------------------
# COMMON EVALUATION PERIOD
# ------------------------------------------------------------

# Start from period 13 because later MA n=12
# cannot produce a forecast before period 13.
# This keeps comparison fair between methods.

evaluation_rows <- 13:nrow(sales_data)


# ------------------------------------------------------------
# COMMON ERROR FUNCTION
# Used later for all forecasting methods
# ------------------------------------------------------------

calculate_errors <- function(actual, forecast) {
  
  # Forecast Error
  error <- actual - forecast
  
  # Mean Absolute Deviation
  MAD <- mean(
    abs(error),
    na.rm = TRUE
  )
  
  # Mean Squared Error
  MSE <- mean(
    error^2,
    na.rm = TRUE
  )
  
  # Mean Absolute Percentage Error
  MAPE <- mean(
    abs(error / actual),
    na.rm = TRUE
  ) * 100
  
  return(
    c(
      MAD = MAD,
      MSE = MSE,
      MAPE = MAPE
    )
  )
}


# ------------------------------------------------------------
# NAIVE ERROR MEASURES
# ------------------------------------------------------------

naive_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$Naive[evaluation_rows]
)

# Show raw results in Console
print(naive_errors)


# ------------------------------------------------------------
# NAIVE NEXT PERIOD FORECAST
# ------------------------------------------------------------

# Naive next forecast = last actual observation
Naive_next <- tail(
  sales_data$Sales,
  1
)


# ------------------------------------------------------------
# NAIVE RESULTS TABLE
# ------------------------------------------------------------

Naive_results <- data.frame(
  
  Method = "Naive",
  
  MAD = round(
    unname(naive_errors["MAD"]),
    2
  ),
  
  MSE = round(
    unname(naive_errors["MSE"]),
    2
  ),
  
  MAPE = paste0(
    round(
      unname(naive_errors["MAPE"]),
      3
    ),
    "%"
  ),
  
  Next_Forecast = round(
    Naive_next,
    2
  )
)

print(Naive_results)

View(Naive_results)


# ------------------------------------------------------------
# EXPORT NAIVE RESULTS TO EXCEL
# ------------------------------------------------------------

write_xlsx(
  Naive_results,
  "Naive_results.xlsx"
)


# ------------------------------------------------------------
# NAIVE METHOD PLOT - REPORT STYLE
# ------------------------------------------------------------

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Naive Forecast vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$Naive,
  col = "blue",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "Naive Forecast"
  ),
  col = c(
    "black",
    "blue"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# 4. MOVING AVERAGE
# ============================================================

# Function to calculate Moving Average
moving_average <- function(x, n) {
  
  forecast <- rep(
    NA,
    length(x)
  )
  
  for (i in (n + 1):length(x)) {
    
    forecast[i] <- mean(
      x[(i - n):(i - 1)]
    )
  }
  
  return(forecast)
}


# ------------------------------------------------------------
# CALCULATE 3 MOVING AVERAGES
# n = 3, 6, 12
# ------------------------------------------------------------

sales_data$MA_3 <- moving_average(
  sales_data$Sales,
  3
)

sales_data$MA_6 <- moving_average(
  sales_data$Sales,
  6
)

sales_data$MA_12 <- moving_average(
  sales_data$Sales,
  12
)


# Detailed MA table for checking calculations
View(
  sales_data[, c(
    "t",
    "Month",
    "Sales",
    "MA_3",
    "MA_6",
    "MA_12"
  )]
)


# ------------------------------------------------------------
# MOVING AVERAGE ERROR MEASURES
# ------------------------------------------------------------

MA3_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$MA_3[evaluation_rows]
)

MA6_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$MA_6[evaluation_rows]
)

MA12_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$MA_12[evaluation_rows]
)


# Show raw results in Console
print(MA3_errors)
print(MA6_errors)
print(MA12_errors)


# ------------------------------------------------------------
# NEXT PERIOD MOVING AVERAGE FORECASTS
# ------------------------------------------------------------

# n = 3
MA3_next <- mean(
  tail(
    sales_data$Sales,
    3
  )
)

# n = 6
MA6_next <- mean(
  tail(
    sales_data$Sales,
    6
  )
)

# n = 12
MA12_next <- mean(
  tail(
    sales_data$Sales,
    12
  )
)


# ------------------------------------------------------------
# MOVING AVERAGE RESULTS TABLE
# ------------------------------------------------------------

MA_results <- data.frame(
  
  Method = c(
    "Moving Average",
    "Moving Average",
    "Moving Average"
  ),
  
  n = c(
    3,
    6,
    12
  ),
  
  MAD = c(
    round(
      unname(MA3_errors["MAD"]),
      2
    ),
    round(
      unname(MA6_errors["MAD"]),
      2
    ),
    round(
      unname(MA12_errors["MAD"]),
      2
    )
  ),
  
  MSE = c(
    round(
      unname(MA3_errors["MSE"]),
      2
    ),
    round(
      unname(MA6_errors["MSE"]),
      2
    ),
    round(
      unname(MA12_errors["MSE"]),
      2
    )
  ),
  
  MAPE = c(
    paste0(
      round(
        unname(MA3_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(MA6_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(MA12_errors["MAPE"]),
        3
      ),
      "%"
    )
  ),
  
  Next_Forecast = c(
    round(MA3_next, 2),
    round(MA6_next, 2),
    round(MA12_next, 2)
  )
)

print(MA_results)

View(MA_results)


# ------------------------------------------------------------
# EXPORT MOVING AVERAGE RESULTS TO EXCEL
# ------------------------------------------------------------

write_xlsx(
  MA_results,
  "MA_results.xlsx"
)


# ============================================================
# MOVING AVERAGE PLOT 1 - n = 3
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Moving Average (n = 3) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$MA_3,
  col = "blue",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "MA n = 3"
  ),
  col = c(
    "black",
    "blue"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# MOVING AVERAGE PLOT 2 - n = 6
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Moving Average (n = 6) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$MA_6,
  col = "red",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "MA n = 6"
  ),
  col = c(
    "black",
    "red"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# MOVING AVERAGE PLOT 3 - n = 12
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Moving Average (n = 12) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$MA_12,
  col = "darkgreen",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "MA n = 12"
  ),
  col = c(
    "black",
    "darkgreen"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# 5. WEIGHTED MOVING AVERAGE
# ============================================================

# Define the three different weight combinations
# First weight = most recent month
# Second weight = two months ago
# Third weight = three months ago

W1 <- c(0.50, 0.30, 0.20)
W2 <- c(0.60, 0.30, 0.10)
W3 <- c(0.70, 0.20, 0.10)


# ------------------------------------------------------------
# WEIGHTED MOVING AVERAGE FUNCTION
# ------------------------------------------------------------

weighted_moving_average <- function(x, weights) {
  
  n <- length(weights)
  
  forecast <- rep(
    NA,
    length(x)
  )
  
  for (i in (n + 1):length(x)) {
    
    # Previous observations, starting with the most recent
    previous_values <- rev(
      x[(i - n):(i - 1)]
    )
    
    forecast[i] <- sum(
      previous_values * weights
    ) / sum(weights)
  }
  
  return(forecast)
}


# ------------------------------------------------------------
# CALCULATE THE 3 WMA FORECASTS
# ------------------------------------------------------------

sales_data$WMA_1 <- weighted_moving_average(
  sales_data$Sales,
  W1
)

sales_data$WMA_2 <- weighted_moving_average(
  sales_data$Sales,
  W2
)

sales_data$WMA_3 <- weighted_moving_average(
  sales_data$Sales,
  W3
)


# Detailed table for checking calculations
# No need to put this full table in the main report

View(
  sales_data[, c(
    "t",
    "Month",
    "Sales",
    "WMA_1",
    "WMA_2",
    "WMA_3"
  )]
)


# ============================================================
# WMA ERROR MEASURES
# ============================================================

# Use the same evaluation period used before:
# Period 13 until the final period

WMA1_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$WMA_1[evaluation_rows]
)

WMA2_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$WMA_2[evaluation_rows]
)

WMA3_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$WMA_3[evaluation_rows]
)


# Show raw results in Console

print(WMA1_errors)
print(WMA2_errors)
print(WMA3_errors)


# ============================================================
# NEXT PERIOD WMA FORECASTS
# ============================================================

# Get the latest 3 actual sales values
# Reverse them so the newest observation is first

last_3_sales <- rev(
  tail(
    sales_data$Sales,
    3
  )
)


# Forecast using weights 0.50, 0.30, 0.20

WMA1_next <- sum(
  last_3_sales * W1
) / sum(W1)


# Forecast using weights 0.60, 0.30, 0.10

WMA2_next <- sum(
  last_3_sales * W2
) / sum(W2)


# Forecast using weights 0.70, 0.20, 0.10

WMA3_next <- sum(
  last_3_sales * W3
) / sum(W3)


# ============================================================
# WMA RESULTS TABLE
# ============================================================

WMA_results <- data.frame(
  
  Method = c(
    "Weighted Moving Average",
    "Weighted Moving Average",
    "Weighted Moving Average"
  ),
  
  Weights = c(
    "0.50, 0.30, 0.20",
    "0.60, 0.30, 0.10",
    "0.70, 0.20, 0.10"
  ),
  
  MAD = c(
    round(
      unname(WMA1_errors["MAD"]),
      2
    ),
    
    round(
      unname(WMA2_errors["MAD"]),
      2
    ),
    
    round(
      unname(WMA3_errors["MAD"]),
      2
    )
  ),
  
  MSE = c(
    round(
      unname(WMA1_errors["MSE"]),
      2
    ),
    
    round(
      unname(WMA2_errors["MSE"]),
      2
    ),
    
    round(
      unname(WMA3_errors["MSE"]),
      2
    )
  ),
  
  MAPE = c(
    paste0(
      round(
        unname(WMA1_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(WMA2_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(WMA3_errors["MAPE"]),
        3
      ),
      "%"
    )
  ),
  
  Next_Forecast = c(
    round(WMA1_next, 2),
    round(WMA2_next, 2),
    round(WMA3_next, 2)
  )
)


print(WMA_results)

View(WMA_results)


# ============================================================
# EXPORT WMA RESULTS TO EXCEL
# ============================================================

write_xlsx(
  WMA_results,
  "WMA_results.xlsx"
)


# ============================================================
# WMA PLOT 1
# Weights = 0.50, 0.30, 0.20
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "WMA (0.50, 0.30, 0.20) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$WMA_1,
  col = "purple",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "WMA 0.50, 0.30, 0.20"
  ),
  col = c(
    "black",
    "purple"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# WMA PLOT 2
# Weights = 0.60, 0.30, 0.10
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "WMA (0.60, 0.30, 0.10) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$WMA_2,
  col = "orange",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "WMA 0.60, 0.30, 0.10"
  ),
  col = c(
    "black",
    "orange"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# WMA PLOT 3
# Weights = 0.70, 0.20, 0.10
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "WMA (0.70, 0.20, 0.10) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$WMA_3,
  col = "darkcyan",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "WMA 0.70, 0.20, 0.10"
  ),
  col = c(
    "black",
    "darkcyan"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)

# ============================================================
# 6. SIMPLE EXPONENTIAL SMOOTHING
# ============================================================

# Three different alpha values
alpha_1 <- 0.10
alpha_2 <- 0.30
alpha_3 <- 0.50


# ------------------------------------------------------------
# EXPONENTIAL SMOOTHING FUNCTION
# ------------------------------------------------------------

exponential_smoothing <- function(x, alpha) {
  
  forecast <- rep(
    NA,
    length(x)
  )
  
  # Initial forecast
  forecast[1] <- x[1]
  
  # Calculate forecasts
  for (i in 2:length(x)) {
    
    forecast[i] <-
      forecast[i - 1] +
      alpha * (
        x[i - 1] - forecast[i - 1]
      )
  }
  
  return(forecast)
}


# ------------------------------------------------------------
# CALCULATE FORECASTS FOR 3 ALPHA VALUES
# ------------------------------------------------------------

sales_data$ES_010 <- exponential_smoothing(
  sales_data$Sales,
  alpha_1
)

sales_data$ES_030 <- exponential_smoothing(
  sales_data$Sales,
  alpha_2
)

sales_data$ES_050 <- exponential_smoothing(
  sales_data$Sales,
  alpha_3
)


# Detailed table for checking calculations
# No need to put the full table in the main report

View(
  sales_data[, c(
    "t",
    "Month",
    "Sales",
    "ES_010",
    "ES_030",
    "ES_050"
  )]
)


# ============================================================
# EXPONENTIAL SMOOTHING ERROR MEASURES
# ============================================================

ES010_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$ES_010[evaluation_rows]
)

ES030_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$ES_030[evaluation_rows]
)

ES050_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$ES_050[evaluation_rows]
)


# Show raw error results in Console

print(ES010_errors)
print(ES030_errors)
print(ES050_errors)


# ============================================================
# NEXT PERIOD FORECASTS
# ============================================================

# Alpha = 0.10
ES010_next <-
  tail(sales_data$ES_010, 1) +
  alpha_1 * (
    tail(sales_data$Sales, 1) -
      tail(sales_data$ES_010, 1)
  )


# Alpha = 0.30
ES030_next <-
  tail(sales_data$ES_030, 1) +
  alpha_2 * (
    tail(sales_data$Sales, 1) -
      tail(sales_data$ES_030, 1)
  )


# Alpha = 0.50
ES050_next <-
  tail(sales_data$ES_050, 1) +
  alpha_3 * (
    tail(sales_data$Sales, 1) -
      tail(sales_data$ES_050, 1)
  )


# ============================================================
# EXPONENTIAL SMOOTHING RESULTS TABLE
# ============================================================

ES_results <- data.frame(
  
  Method = c(
    "Exponential Smoothing",
    "Exponential Smoothing",
    "Exponential Smoothing"
  ),
  
  Alpha = c(
    0.10,
    0.30,
    0.50
  ),
  
  MAD = c(
    round(
      unname(ES010_errors["MAD"]),
      2
    ),
    
    round(
      unname(ES030_errors["MAD"]),
      2
    ),
    
    round(
      unname(ES050_errors["MAD"]),
      2
    )
  ),
  
  MSE = c(
    round(
      unname(ES010_errors["MSE"]),
      2
    ),
    
    round(
      unname(ES030_errors["MSE"]),
      2
    ),
    
    round(
      unname(ES050_errors["MSE"]),
      2
    )
  ),
  
  MAPE = c(
    paste0(
      round(
        unname(ES010_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(ES030_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(ES050_errors["MAPE"]),
        3
      ),
      "%"
    )
  ),
  
  Next_Forecast = c(
    round(ES010_next, 2),
    round(ES030_next, 2),
    round(ES050_next, 2)
  )
)


print(ES_results)

View(ES_results)


# ============================================================
# EXPORT ES RESULTS TO EXCEL
# ============================================================

write_xlsx(
  ES_results,
  "ES_results.xlsx"
)


# ============================================================
# EXPONENTIAL SMOOTHING PLOT 1
# Alpha = 0.10
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Exponential Smoothing (Alpha = 0.10) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$ES_010,
  col = "blue",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "ES Alpha = 0.10"
  ),
  col = c(
    "black",
    "blue"
  ),
  lty = c(1, 2),
  lwd = c(2.5, 2),
  bty = "n"
)


# ============================================================
# EXPONENTIAL SMOOTHING PLOT 2
# Alpha = 0.30
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Exponential Smoothing (Alpha = 0.30) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$ES_030,
  col = "red",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "ES Alpha = 0.30"
  ),
  col = c(
    "black",
    "red"
  ),
  lty = c(1, 2),
  lwd = c(2.5, 2),
  bty = "n"
)


# ============================================================
# EXPONENTIAL SMOOTHING PLOT 3
# Alpha = 0.50
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Exponential Smoothing (Alpha = 0.50) vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$ES_050,
  col = "darkgreen",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "ES Alpha = 0.50"
  ),
  col = c(
    "black",
    "darkgreen"
  ),
  lty = c(1, 2),
  lwd = c(2.5, 2),
  bty = "n"
)

# ============================================================
# 7. TREND-ADJUSTED EXPONENTIAL SMOOTHING (FIT)
# ============================================================

# Four different Alpha-Beta cases

FIT_alpha <- c(
  0.10,
  0.30,
  0.50,
  0.70
)

FIT_beta <- c(
  0.10,
  0.20,
  0.30,
  0.50
)


# ============================================================
# TREND-ADJUSTED EXPONENTIAL SMOOTHING FUNCTION
# ============================================================

trend_adjusted_es <- function(x, alpha, beta) {
  
  n <- length(x)
  
  # Smoothed forecast level
  F <- rep(
    NA,
    n
  )
  
  # Smoothed trend
  T <- rep(
    NA,
    n
  )
  
  # Forecast Including Trend
  FIT <- rep(
    NA,
    n
  )
  
  
  # ----------------------------------------------------------
  # INITIAL VALUES
  # ----------------------------------------------------------
  
  # Initial forecast level
  F[1] <- x[1]
  
  # Initial trend
  T[1] <- 0
  
  
  # ----------------------------------------------------------
  # CALCULATE FORECASTS
  # ----------------------------------------------------------
  
  for (i in 2:n) {
    
    # Step 1:
    # Calculate smoothed forecast level
    
    F[i] <-
      alpha * x[i - 1] +
      (1 - alpha) * (
        F[i - 1] + T[i - 1]
      )
    
    
    # Step 2:
    # Calculate smoothed trend
    
    T[i] <-
      beta * (
        F[i] - F[i - 1]
      ) +
      (1 - beta) * T[i - 1]
    
    
    # Step 3:
    # Forecast Including Trend
    
    FIT[i] <-
      F[i] + T[i]
  }
  
  
  # ----------------------------------------------------------
  # NEXT PERIOD FORECAST
  # ----------------------------------------------------------
  
  F_next <-
    alpha * x[n] +
    (1 - alpha) * (
      F[n] + T[n]
    )
  
  
  T_next <-
    beta * (
      F_next - F[n]
    ) +
    (1 - beta) * T[n]
  
  
  FIT_next <-
    F_next + T_next
  
  
  return(
    list(
      Forecast = FIT,
      Level = F,
      Trend = T,
      Next_Forecast = FIT_next
    )
  )
}


# ============================================================
# CASE 1
# Alpha = 0.10
# Beta  = 0.10
# ============================================================

FIT_case1 <- trend_adjusted_es(
  sales_data$Sales,
  FIT_alpha[1],
  FIT_beta[1]
)

sales_data$FIT_Case1 <-
  FIT_case1$Forecast


# ============================================================
# CASE 2
# Alpha = 0.30
# Beta  = 0.20
# ============================================================

FIT_case2 <- trend_adjusted_es(
  sales_data$Sales,
  FIT_alpha[2],
  FIT_beta[2]
)

sales_data$FIT_Case2 <-
  FIT_case2$Forecast


# ============================================================
# CASE 3
# Alpha = 0.50
# Beta  = 0.30
# ============================================================

FIT_case3 <- trend_adjusted_es(
  sales_data$Sales,
  FIT_alpha[3],
  FIT_beta[3]
)

sales_data$FIT_Case3 <-
  FIT_case3$Forecast


# ============================================================
# CASE 4
# Alpha = 0.70
# Beta  = 0.50
# ============================================================

FIT_case4 <- trend_adjusted_es(
  sales_data$Sales,
  FIT_alpha[4],
  FIT_beta[4]
)

sales_data$FIT_Case4 <-
  FIT_case4$Forecast


# ============================================================
# VIEW DETAILED FORECASTS
# ============================================================

# This table is only for checking calculations
# No need to put the full table in the report

View(
  sales_data[, c(
    "t",
    "Month",
    "Sales",
    "FIT_Case1",
    "FIT_Case2",
    "FIT_Case3",
    "FIT_Case4"
  )]
)


# ============================================================
# FIT ERROR MEASURES
# ============================================================

# Use the same historical evaluation period
# already defined earlier in the project

FIT1_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$FIT_Case1[evaluation_rows]
)

FIT2_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$FIT_Case2[evaluation_rows]
)

FIT3_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$FIT_Case3[evaluation_rows]
)

FIT4_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$FIT_Case4[evaluation_rows]
)


# Show raw results in Console

print(FIT1_errors)
print(FIT2_errors)
print(FIT3_errors)
print(FIT4_errors)


# ============================================================
# NEXT PERIOD FORECASTS
# ============================================================

FIT1_next <- FIT_case1$Next_Forecast

FIT2_next <- FIT_case2$Next_Forecast

FIT3_next <- FIT_case3$Next_Forecast

FIT4_next <- FIT_case4$Next_Forecast


# ============================================================
# FIT RESULTS TABLE
# ============================================================

FIT_results <- data.frame(
  
  Case = c(
    "Case 1",
    "Case 2",
    "Case 3",
    "Case 4"
  ),
  
  Alpha = c(
    0.10,
    0.30,
    0.50,
    0.70
  ),
  
  Beta = c(
    0.10,
    0.20,
    0.30,
    0.50
  ),
  
  MAD = c(
    round(
      unname(FIT1_errors["MAD"]),
      2
    ),
    
    round(
      unname(FIT2_errors["MAD"]),
      2
    ),
    
    round(
      unname(FIT3_errors["MAD"]),
      2
    ),
    
    round(
      unname(FIT4_errors["MAD"]),
      2
    )
  ),
  
  MSE = c(
    round(
      unname(FIT1_errors["MSE"]),
      2
    ),
    
    round(
      unname(FIT2_errors["MSE"]),
      2
    ),
    
    round(
      unname(FIT3_errors["MSE"]),
      2
    ),
    
    round(
      unname(FIT4_errors["MSE"]),
      2
    )
  ),
  
  MAPE = c(
    paste0(
      round(
        unname(FIT1_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(FIT2_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(FIT3_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(FIT4_errors["MAPE"]),
        3
      ),
      "%"
    )
  ),
  
  Next_Forecast = c(
    round(FIT1_next, 2),
    round(FIT2_next, 2),
    round(FIT3_next, 2),
    round(FIT4_next, 2)
  )
)


print(FIT_results)

View(FIT_results)


# ============================================================
# EXPORT FIT RESULTS TO EXCEL
# ============================================================

write_xlsx(
  FIT_results,
  "FIT_results.xlsx"
)


# ============================================================
# IDENTIFY BEST FIT CASE
# Based on lowest MAD
# ============================================================

FIT_MAD_numeric <- c(
  unname(FIT1_errors["MAD"]),
  unname(FIT2_errors["MAD"]),
  unname(FIT3_errors["MAD"]),
  unname(FIT4_errors["MAD"])
)

best_FIT_case <- which.min(
  FIT_MAD_numeric
)

print(
  paste(
    "Best FIT Case =",
    best_FIT_case
  )
)

print(
  paste(
    "Best FIT MAD =",
    round(
      FIT_MAD_numeric[best_FIT_case],
      2
    )
  )
)

# ============================================================
# FIT PLOT 1
# Alpha = 0.10
# Beta  = 0.10
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "FIT Case 1 (Alpha = 0.10, Beta = 0.10)",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$FIT_Case1,
  col = "blue",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "FIT Case 1"
  ),
  col = c(
    "black",
    "blue"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# FIT PLOT 2
# Alpha = 0.30
# Beta  = 0.20
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "FIT Case 2 (Alpha = 0.30, Beta = 0.20)",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$FIT_Case2,
  col = "red",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "FIT Case 2"
  ),
  col = c(
    "black",
    "red"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# FIT PLOT 3
# Alpha = 0.50
# Beta  = 0.30
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "FIT Case 3 (Alpha = 0.50, Beta = 0.30)",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$FIT_Case3,
  col = "darkgreen",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "FIT Case 3"
  ),
  col = c(
    "black",
    "darkgreen"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)


# ============================================================
# FIT PLOT 4
# Alpha = 0.70
# Beta  = 0.50
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "FIT Case 4 (Alpha = 0.70, Beta = 0.50)",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)

axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)

lines(
  sales_data$Month,
  sales_data$FIT_Case4,
  col = "purple",
  lwd = 2,
  lty = 2
)

grid(
  col = "lightgray",
  lty = "dotted"
)

legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "FIT Case 4"
  ),
  col = c(
    "black",
    "purple"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2
  ),
  bty = "n"
)

# ============================================================
# 8. TREND PROJECTION
# ============================================================

# Fit linear trend model
trend_model <- lm(
  Sales ~ t,
  data = sales_data
)


# Show model summary
summary(trend_model)


# ------------------------------------------------------------
# EXTRACT TREND EQUATION
# ------------------------------------------------------------

trend_intercept <- unname(coef(trend_model)[1])
trend_slope <- unname(coef(trend_model)[2])


print(
  paste(
    "Trend Equation: Sales =",
    round(trend_intercept, 2),
    "+",
    round(trend_slope, 2),
    "* t"
  )
)


# ------------------------------------------------------------
# CALCULATE TREND FORECASTS FOR HISTORICAL DATA
# ------------------------------------------------------------

sales_data$Trend_Forecast <- predict(
  trend_model,
  newdata = sales_data
)


# View detailed results
# No need to put full table in main report

View(
  sales_data[, c(
    "t",
    "Month",
    "Sales",
    "Trend_Forecast"
  )]
)


# ============================================================
# TREND PROJECTION ERROR MEASURES
# ============================================================

Trend_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$Trend_Forecast[evaluation_rows]
)


print(Trend_errors)


# ============================================================
# NEXT PERIOD FORECAST
# ============================================================

next_t <- max(
  sales_data$t
) + 1


Trend_next <- predict(
  trend_model,
  newdata = data.frame(
    t = next_t
  )
)


print(
  paste(
    "Next Trend Forecast =",
    round(
      Trend_next,
      2
    )
  )
)


# ============================================================
# TREND RESULTS TABLE
# ============================================================


Trend_results <- data.frame(
  
  Method = "Trend Projection",
  
  Intercept = round(
    trend_intercept,
    2
  ),
  
  Slope = round(
    trend_slope,
    2
  ),
  
  MAD = round(
    unname(Trend_errors["MAD"]),
    2
  ),
  
  MSE = round(
    unname(Trend_errors["MSE"]),
    2
  ),
  
  MAPE = paste0(
    round(
      unname(Trend_errors["MAPE"]),
      3
    ),
    "%"
  ),
  
  Next_Forecast = round(
    as.numeric(Trend_next),
    2
  )
)

rownames(Trend_results) <- NULL

print(Trend_results)

View(Trend_results)




# ============================================================
# EXPORT TREND RESULTS TO EXCEL
# ============================================================

write_xlsx(
  Trend_results,
  "Trend_results.xlsx"
)


# ============================================================
# TREND PROJECTION PLOT
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Trend Projection vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)


axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)


lines(
  sales_data$Month,
  sales_data$Trend_Forecast,
  col = "blue",
  lwd = 2.5,
  lty = 2
)


grid(
  col = "lightgray",
  lty = "dotted"
)


legend(
  "topleft",
  legend = c(
    "Actual Sales",
    "Trend Projection"
  ),
  col = c(
    "black",
    "blue"
  ),
  lty = c(
    1,
    2
  ),
  lwd = c(
    2.5,
    2.5
  ),
  bty = "n"
)

# ============================================================
# 9. SEASONALITY ANALYSIS
# ============================================================


# ------------------------------------------------------------
# STEP 1: CALCULATE ACTUAL / TREND RATIO
# ------------------------------------------------------------

# Remove the effect of trend
sales_data$Seasonal_Ratio <-
  sales_data$Sales /
  sales_data$Trend_Forecast


# ------------------------------------------------------------
# STEP 2: CREATE MONTH NUMBER
# ------------------------------------------------------------

sales_data$Month_Number <- as.numeric(
  format(
    sales_data$Month,
    "%m"
  )
)


# ------------------------------------------------------------
# STEP 3: CALCULATE AVERAGE SEASONAL RATIO
# FOR EACH MONTH
# ------------------------------------------------------------

seasonal_indices <- aggregate(
  
  Seasonal_Ratio ~ Month_Number,
  
  data = sales_data,
  
  FUN = mean
)


# Sort January to December
seasonal_indices <- seasonal_indices[
  order(
    seasonal_indices$Month_Number
  ),
]



# ------------------------------------------------------------
# STEP 4: NORMALIZE SEASONAL INDICES
# ------------------------------------------------------------

# Make sure average seasonal index = 1

seasonal_indices$Seasonal_Index <-
  seasonal_indices$Seasonal_Ratio /
  mean(
    seasonal_indices$Seasonal_Ratio
  )

# ------------------------------------------------------------
# STEP 5: ADD MONTH NAMES
# ------------------------------------------------------------

seasonal_indices$Month <- month.name[
  seasonal_indices$Month_Number
]


# ------------------------------------------------------------
# STEP 6: CREATE CLEAN RESULTS TABLE
# ------------------------------------------------------------

Seasonality_results <- data.frame(
  
  Month = seasonal_indices$Month,
  
  Seasonal_Index = round(
    seasonal_indices$Seasonal_Index,
    3
  ),
  
  Seasonal_Effect = paste0(
    
    round(
      (
        seasonal_indices$Seasonal_Index - 1
      ) * 100,
      2
    ),
    
    "%"
  )
)


print(Seasonality_results)

View(Seasonality_results)


# ============================================================
# CHECK SEASONAL INDICES
# ============================================================

# Average should be approximately 1

print(
  mean(
    seasonal_indices$Seasonal_Index
  )
)


# Sum should be approximately 12

print(
  sum(
    seasonal_indices$Seasonal_Index
  )
)


# ============================================================
# ADD SEASONAL INDEX BACK TO ORIGINAL DATA
# ============================================================

sales_data$Seasonal_Index <-
  seasonal_indices$Seasonal_Index[
    match(
      sales_data$Month_Number,
      seasonal_indices$Month_Number
    )
  ]


# ============================================================
# DESEASONALIZED SALES
# ============================================================

sales_data$Deseasonalized_Sales <-
  sales_data$Sales /
  sales_data$Seasonal_Index


# Detailed table for checking only
# No need to put the full 48 rows in the report

View(
  sales_data[, c(
    "t",
    "Month",
    "Sales",
    "Trend_Forecast",
    "Seasonal_Index",
    "Deseasonalized_Sales"
  )]
)


# ============================================================
# DETAILED SEASONAL RATIO TABLE BY YEAR
# ============================================================

# Create Year column
sales_data$Year <- as.numeric(
  format(
    sales_data$Month,
    "%Y"
  )
)


# ------------------------------------------------------------
# CREATE TABLE WITH MONTHS
# ------------------------------------------------------------

Seasonality_Details <- data.frame(
  
  Month_Number = 1:12,
  
  Month = month.name
)


# ------------------------------------------------------------
# ADD ONE RATIO COLUMN FOR EACH YEAR
# ------------------------------------------------------------

years <- sort(
  unique(
    sales_data$Year
  )
)

for (yr in years) {
  
  year_data <- sales_data[
    sales_data$Year == yr,
  ]
  
  ratios <- year_data$Seasonal_Ratio[
    match(
      1:12,
      year_data$Month_Number
    )
  ]
  
  Seasonality_Details[[paste0("Ratio_", yr)]] <- ratios
}


# ------------------------------------------------------------
# ADD FINAL SEASONAL INDEX
# ------------------------------------------------------------

Seasonality_Details$Seasonal_Index <-
  seasonal_indices$Seasonal_Index[
    match(
      Seasonality_Details$Month_Number,
      seasonal_indices$Month_Number
    )
  ]


# ------------------------------------------------------------
# REMOVE MONTH NUMBER FROM FINAL DISPLAY
# ------------------------------------------------------------

Seasonality_Details$Month_Number <- NULL


# ------------------------------------------------------------
# ROUND NUMERIC VALUES
# ------------------------------------------------------------

numeric_columns <- sapply(
  Seasonality_Details,
  is.numeric
)

Seasonality_Details[
  numeric_columns
] <- round(
  Seasonality_Details[
    numeric_columns
  ],
  3
)


# View table
print(Seasonality_Details)

View(Seasonality_Details)

write_xlsx(
  Seasonality_Details,
  "Seasonality_Details.xlsx"
)



# ============================================================
# EXPORT SEASONALITY RESULTS TO EXCEL
# ============================================================

write_xlsx(
  Seasonality_results,
  "Seasonality_results.xlsx"
)


# ============================================================
# SEASONAL INDICES PLOT
# ============================================================

barplot(
  
  seasonal_indices$Seasonal_Index,
  
  names.arg = substr(
    seasonal_indices$Month,
    1,
    3
  ),
  
  main = "Monthly Seasonal Indices",
  
  xlab = "Month",
  
  ylab = "Seasonal Index",
  
  col = "steelblue",
  
  ylim = c(
    0,
    max(
      seasonal_indices$Seasonal_Index
    ) * 1.15
  )
)


# Reference line at Seasonal Index = 1

abline(
  h = 1,
  col = "red",
  lty = 2,
  lwd = 2
)


grid(
  nx = NA,
  ny = NULL,
  col = "lightgray",
  lty = "dotted"
)


# ============================================================
# 10. COMBINED TREND AND SEASONAL FORECAST
# ============================================================


# ------------------------------------------------------------
# STEP 1: CALCULATE HISTORICAL SEASONALIZED FORECAST
# ------------------------------------------------------------

sales_data$Seasonal_Trend_Forecast <-
  sales_data$Trend_Forecast *
  sales_data$Seasonal_Index


# ------------------------------------------------------------
# VIEW DETAILED RESULTS
# ------------------------------------------------------------

# For checking calculations only
# No need to put all 48 rows in the main report

results <- sales_data[, c(
  "t",
  "Month",
  "Sales",
  "Trend_Forecast",
  "Seasonal_Index",
  "Seasonal_Trend_Forecast"
)]

View(results)

write_xlsx(
  results,
  "sales_results.xlsx"
)

# ============================================================
# SEASONALIZED TREND ERROR MEASURES
# ============================================================

Seasonal_Trend_errors <- calculate_errors(
  sales_data$Sales[evaluation_rows],
  sales_data$Seasonal_Trend_Forecast[evaluation_rows]
)


# Show raw results
print(Seasonal_Trend_errors)


# ============================================================
# NEXT PERIOD SEASONALIZED FORECAST
# ============================================================

# Detect next month automatically

next_month_date <- seq(
  from = max(sales_data$Month),
  by = "month",
  length.out = 2
)[2]


# Get next month number

next_month_number <- as.numeric(
  format(
    next_month_date,
    "%m"
  )
)


# Find Seasonal Index for next month

next_seasonal_index <-
  seasonal_indices$Seasonal_Index[
    match(
      next_month_number,
      seasonal_indices$Month_Number
    )
  ]


# Trend_next was already calculated in Section 8
# Seasonalized Forecast = Trend Forecast × Seasonal Index

Seasonal_Trend_next <-
  as.numeric(Trend_next) *
  next_seasonal_index


# Show next forecast information

print(
  paste(
    "Next Month =",
    format(next_month_date, "%B %Y")
  )
)

print(
  paste(
    "Next Month Seasonal Index =",
    round(next_seasonal_index, 3)
  )
)

print(
  paste(
    "Next Seasonalized Trend Forecast =",
    round(Seasonal_Trend_next, 2)
  )
)


# ============================================================
# SEASONALIZED TREND RESULTS TABLE
# ============================================================

Seasonal_Trend_results <- data.frame(
  
  Method = "Combined Trend and Seasonal Forecast",
  
  MAD = round(
    unname(
      Seasonal_Trend_errors["MAD"]
    ),
    2
  ),
  
  MSE = round(
    unname(
      Seasonal_Trend_errors["MSE"]
    ),
    2
  ),
  
  MAPE = paste0(
    round(
      unname(
        Seasonal_Trend_errors["MAPE"]
      ),
      3
    ),
    "%"
  ),
  
  Next_Month = format(
    next_month_date,
    "%B %Y"
  ),
  
  Seasonal_Index = round(
    next_seasonal_index,
    3
  ),
  
  Trend_Forecast = round(
    as.numeric(Trend_next),
    2
  ),
  
  Next_Forecast = round(
    Seasonal_Trend_next,
    2
  )
)


rownames(Seasonal_Trend_results) <- NULL


print(Seasonal_Trend_results)

View(Seasonal_Trend_results)


# ============================================================
# EXPORT RESULTS TO EXCEL
# ============================================================

write_xlsx(
  Seasonal_Trend_results,
  "Seasonal_Trend_results.xlsx"
)


# ============================================================
# SEASONALIZED TREND PLOT
# ============================================================

plot(
  sales_data$Month,
  sales_data$Sales,
  type = "l",
  main = "Combined Trend and Seasonal Forecast vs Actual Sales",
  xlab = "Year",
  ylab = "Sales (Millions of Dollars)",
  col = "black",
  lwd = 2.5,
  xaxt = "n"
)


axis.Date(
  1,
  at = seq(
    min(sales_data$Month),
    max(sales_data$Month),
    by = "year"
  ),
  format = "%Y"
)


lines(
  sales_data$Month,
  sales_data$Seasonal_Trend_Forecast,
  col = "darkorange",
  lwd = 2.5,
  lty = 2
)


grid(
  col = "lightgray",
  lty = "dotted"
)


legend(
  "topleft",
  
  legend = c(
    "Actual Sales",
    "Combined Trend and Seasonal Forecast"
  ),
  
  col = c(
    "black",
    "darkorange"
  ),
  
  lty = c(
    1,
    2
  ),
  
  lwd = c(
    2.5,
    2.5
  ),
  
  bty = "n"
)



# ============================================================
# 11. FINAL COMPARISON OF FORECASTING METHODS
# ============================================================


# ============================================================
# STEP 1: CREATE TABLE WITH ALL TESTED CASES
# FINAL COMPARISON INCLUDES:
# MA, WMA, ES, FIT
# ============================================================

Final_Comparison_Results <- data.frame(
  
  Method = c(
    
    rep(
      "Moving Average",
      3
    ),
    
    rep(
      "Weighted Moving Average",
      3
    ),
    
    rep(
      "Exponential Smoothing",
      3
    ),
    
    rep(
      "Trend-Adjusted Exponential Smoothing",
      4
    )
  ),
  
  
  Setting = c(
    
    "n = 3",
    "n = 6",
    "n = 12",
    
    "0.50, 0.30, 0.20",
    "0.60, 0.30, 0.10",
    "0.70, 0.20, 0.10",
    
    "Alpha = 0.10",
    "Alpha = 0.30",
    "Alpha = 0.50",
    
    "Alpha = 0.10, Beta = 0.10",
    "Alpha = 0.30, Beta = 0.20",
    "Alpha = 0.50, Beta = 0.30",
    "Alpha = 0.70, Beta = 0.50"
  ),
  
  
  MAD = c(
    
    unname(MA3_errors["MAD"]),
    unname(MA6_errors["MAD"]),
    unname(MA12_errors["MAD"]),
    
    unname(WMA1_errors["MAD"]),
    unname(WMA2_errors["MAD"]),
    unname(WMA3_errors["MAD"]),
    
    unname(ES010_errors["MAD"]),
    unname(ES030_errors["MAD"]),
    unname(ES050_errors["MAD"]),
    
    unname(FIT1_errors["MAD"]),
    unname(FIT2_errors["MAD"]),
    unname(FIT3_errors["MAD"]),
    unname(FIT4_errors["MAD"])
  ),
  
  
  MSE = c(
    
    unname(MA3_errors["MSE"]),
    unname(MA6_errors["MSE"]),
    unname(MA12_errors["MSE"]),
    
    unname(WMA1_errors["MSE"]),
    unname(WMA2_errors["MSE"]),
    unname(WMA3_errors["MSE"]),
    
    unname(ES010_errors["MSE"]),
    unname(ES030_errors["MSE"]),
    unname(ES050_errors["MSE"]),
    
    unname(FIT1_errors["MSE"]),
    unname(FIT2_errors["MSE"]),
    unname(FIT3_errors["MSE"]),
    unname(FIT4_errors["MSE"])
  ),
  
  
  MAPE = c(
    
    paste0(
      round(
        unname(MA3_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(MA6_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(MA12_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(WMA1_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(WMA2_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(WMA3_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(ES010_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(ES030_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(ES050_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(FIT1_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(FIT2_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(FIT3_errors["MAPE"]),
        3
      ),
      "%"
    ),
    
    paste0(
      round(
        unname(FIT4_errors["MAPE"]),
        3
      ),
      "%"
    )
  )
)


# ============================================================
# STEP 2: ROUND NUMERIC VALUES
# ============================================================

Final_Comparison_Results$MAD <- round(
  Final_Comparison_Results$MAD,
  2
)

Final_Comparison_Results$MSE <- round(
  Final_Comparison_Results$MSE,
  2
)


print(Final_Comparison_Results)

View(Final_Comparison_Results)

write_xlsx(Final_Comparison_Results, "Final_Comparison_Results.xlsx")



# ============================================================
# STEP 3: FIND BEST CASE FROM EACH METHOD FAMILY
# BASED ON LOWEST MAD
# ============================================================

best_MA <- Final_Comparison_Results[
  Final_Comparison_Results$Method == "Moving Average",
][
  which.min(
    Final_Comparison_Results$MAD[
      Final_Comparison_Results$Method == "Moving Average"
    ]
  ),
]


best_WMA <- Final_Comparison_Results[
  Final_Comparison_Results$Method == "Weighted Moving Average",
][
  which.min(
    Final_Comparison_Results$MAD[
      Final_Comparison_Results$Method == "Weighted Moving Average"
    ]
  ),
]


best_ES <- Final_Comparison_Results[
  Final_Comparison_Results$Method == "Exponential Smoothing",
][
  which.min(
    Final_Comparison_Results$MAD[
      Final_Comparison_Results$Method == "Exponential Smoothing"
    ]
  ),
]


best_FIT <- Final_Comparison_Results[
  Final_Comparison_Results$Method ==
    "Trend-Adjusted Exponential Smoothing",
][
  which.min(
    Final_Comparison_Results$MAD[
      Final_Comparison_Results$Method ==
        "Trend-Adjusted Exponential Smoothing"
    ]
  ),
]


# ============================================================
# STEP 4: CREATE SUMMARY OF BEST CASE FROM EACH METHOD
# ============================================================

Best_Methods_Summary <- rbind(
  best_MA,
  best_WMA,
  best_ES,
  best_FIT
)

rownames(Best_Methods_Summary) <- NULL


print(Best_Methods_Summary)

View(Best_Methods_Summary)

write_xlsx(Best_Methods_Summary, "Best_Methods_Summary.xlsx")


# ============================================================
# STEP 5: IDENTIFY OVERALL BEST METHOD
# PRIMARY CRITERION = LOWEST MAD
# ============================================================

best_by_MAD <- Best_Methods_Summary[
  which.min(
    Best_Methods_Summary$MAD
  ),
]


best_by_MSE <- Best_Methods_Summary[
  which.min(
    Best_Methods_Summary$MSE
  ),
]


print("Best Method Based on MAD:")
print(best_by_MAD)

print("Best Method Based on MSE:")
print(best_by_MSE)


# ============================================================
# STEP 6: CREATE SHORT LABELS FOR CHARTS
# ============================================================

Final_Comparison_Results$Chart_Label <- c(
  
  "MA n=3",
  "MA n=6",
  "MA n=12",
  
  "WMA 0.50/0.30/0.20",
  "WMA 0.60/0.30/0.10",
  "WMA 0.70/0.20/0.10",
  
  "ES a=0.10",
  "ES a=0.30",
  "ES a=0.50",
  
  "FIT a=.10 b=.10",
  "FIT a=.30 b=.20",
  "FIT a=.50 b=.30",
  "FIT a=.70 b=.50"
)


# ============================================================
# STEP 7: MAD COMPARISON CHART
# ============================================================

MAD_plot_data <- Final_Comparison_Results[
  order(
    Final_Comparison_Results$MAD,
    decreasing = TRUE
  ),
]


old_par <- par(
  no.readonly = TRUE
)


par(
  mar = c(
    5,
    13,
    4,
    3
  )
)


bar_positions <- barplot(
  
  MAD_plot_data$MAD,
  
  names.arg = MAD_plot_data$Chart_Label,
  
  horiz = TRUE,
  
  las = 1,
  
  cex.names = 0.75,
  
  main = "MAD Comparison Across Forecasting Methods",
  
  xlab = "MAD",
  
  col = "lightblue",
  
  xlim = c(
    0,
    max(MAD_plot_data$MAD) * 1.20
  )
)


text(
  x = MAD_plot_data$MAD,
  y = bar_positions,
  labels = round(
    MAD_plot_data$MAD,
    2
  ),
  pos = 4,
  cex = 0.70
)


par(old_par)


# ============================================================
# STEP 9: MSE COMPARISON CHART
# ============================================================

MSE_plot_data <- Final_Comparison_Results[
  order(
    Final_Comparison_Results$MSE,
    decreasing = TRUE
  ),
]


old_par <- par(
  no.readonly = TRUE
)


par(
  mar = c(
    5,
    13,
    4,
    3
  )
)


bar_positions <- barplot(
  
  MSE_plot_data$MSE,
  
  names.arg = MSE_plot_data$Chart_Label,
  
  horiz = TRUE,
  
  las = 1,
  
  cex.names = 0.75,
  
  main = "MSE Comparison Across Forecasting Methods",
  
  xlab = "MSE",
  
  col = "lightgreen",
  
  xlim = c(
    0,
    max(MSE_plot_data$MSE) * 1.20
  )
)


text(
  x = MSE_plot_data$MSE,
  y = bar_positions,
  labels = round(
    MSE_plot_data$MSE,
    0
  ),
  pos = 4,
  cex = 0.70
)


par(old_par)


# ============================================================
# STEP 10: MAPE COMPARISON CHART
# ============================================================

# Convert MAPE from text like "5.123%" to numeric
MAPE_numeric <- as.numeric(
  gsub(
    "%",
    "",
    Final_Comparison_Results$MAPE
  )
)


MAPE_plot_data <- Final_Comparison_Results[
  order(
    MAPE_numeric,
    decreasing = TRUE
  ),
]


MAPE_plot_data$MAPE_numeric <- as.numeric(
  gsub(
    "%",
    "",
    MAPE_plot_data$MAPE
  )
)


old_par <- par(
  no.readonly = TRUE
)


par(
  mar = c(
    5,
    13,
    4,
    3
  )
)


bar_positions <- barplot(
  
  MAPE_plot_data$MAPE_numeric,
  
  names.arg = MAPE_plot_data$Chart_Label,
  
  horiz = TRUE,
  
  las = 1,
  
  cex.names = 0.75,
  
  main = "MAPE Comparison Across Forecasting Methods",
  
  xlab = "MAPE (%)",
  
  col = "lightblue",
  
  xlim = c(
    0,
    max(MAPE_plot_data$MAPE_numeric) * 1.20
  )
)


text(
  x = MAPE_plot_data$MAPE_numeric,
  y = bar_positions,
  labels = paste0(
    round(
      MAPE_plot_data$MAPE_numeric,
      2
    ),
    "%"
  ),
  pos = 4,
  cex = 0.70
)


par(old_par)




