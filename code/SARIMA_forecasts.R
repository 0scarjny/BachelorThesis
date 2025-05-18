# -----------------------------------------------------------------------------
# Script: SARIMA Forecasts
# Author: Oscar Janossy
# Date: 2025-05-19
# Description: Preprocessing of data and rtaining/forecating of system imbalance using SARIMA
# -----------------------------------------------------------------------------

# Load required packages
library(xts)
library(forecast)
library(future)
library(future.apply)
library(progressr)

# ----------------------------------------------------------------------------
# 1. Data Preprocessing
# ----------------------------------------------------------------------------

## 1.1 Read raw data from CSV
df <- read.csv(
              file             = "Documents/VS Code/Thesis/data/data.csv",
              header           = TRUE,
              stringsAsFactors = FALSE )


## 1.2 Parse dates and create xts object
df$start_date <- as.POSIXct(df$start_date,
                            format = "%Y-%m-%d %H:%M:%S",
                            tz     = "UTC")  

data <- xts(df$Imbalance, order.by = df$start_date)

# ----------------------------------------------------------------------------
# 2. Create training, validation, and test sets (xts)
# ----------------------------------------------------------------------------
n    <- nrow(data)

# define the cut-points
train_end  <- floor(0.70 * n)
valid_end  <- floor(0.85 * n)  # 0.70 + 0.15

# slice into train / validation / test
train      <- data[1:train_end] 
validation <- data[(train_end + 1):valid_end]
test       <- data[(valid_end + 1):n]


# Check the time spans
index(train)[1]         ; index(train)[length(train)]
index(validation)[1]    ; index(validation)[length(validation)]
index(test)[1]          ; index(test)[length(test)]

# ----------------------------------------------------------------------------
# 3. Create ts objects
# ----------------------------------------------------------------------------


## 3.1 Define start time for ts
start_time <- df$start_date[1]
ts_start    <- c(as.numeric(format(start_time, "%Y")),
                 as.numeric(format(start_time, "%j"))) 

## 3.2 Convert to ts object
data_ts <- ts(df$Imbalance,
             start     = ts_start,
             frequency = 48)   # daily observations



## 3.3 Extract ts parameters cand compute the cut-point times of the series
ts_params <- tsp(data_ts)     # a vector c(start, end, frequency)
start_t   <- ts_params[1]
freq      <- ts_params[3]

train_end_time <- start_t + (train_end   - 1) / freq
valid_end_time <- start_t + (valid_end - 1) / freq


## 3.4 Split ts into train, validation, and test sets
train_ts <- window(data_ts, end = train_end_time)
validation_ts <- window(data_ts,
                        start = train_end_time + 1/freq,
                        end   = valid_end_time)
test_ts <- window(data_ts,
                  start = valid_end_time + 1/freq)

train_val_ts <- window(data_ts,
                       end   = valid_end_time)


# Check that everything is done as expected
tsp(train_ts)      
tsp(validation_ts)
tsp(test_ts)



# ----------------------------------------------------------------------------
# 4. SARIMA Optimal Order Grid Search
# ----------------------------------------------------------------------------



evaluate_sarima_order <- function(train_ts,
                                   valid_ts,
                                   p_vals   = 0:2,
                                   d_vals   = 0:1,
                                   q_vals   = 0:2,
                                   P_vals   = 0:1,
                                   D_vals   = 0:1,
                                   Q_vals   = 0:1,
                                   period   = frequency(train_ts),
                                   cores    = parallel::detectCores() - 1) {
  
  # order searched (p,d,q,P,D,Q)
  param_grid <- expand.grid(p = p_vals,d = d_vals,q = q_vals,
                            P = P_vals,D = D_vals, Q = Q_vals)
  
  # Setup parallel processing
  plan(multisession, workers = cores)
  
  # Enable progress reporting during the search
  handlers(global = TRUE)
  handlers("txtprogressbar")
  
  # Process each row with progress reporting
  with_progress({
    # Create a progress bar
    p <- progressor(steps = nrow(param_grid))
    
    # Function to process each row
    process_row <- function(i, p, train_ts, valid_ts, period) {
      pars <- param_grid[i, ]
      
      # Update progress
      p(sprintf("Model %d/%d", i, nrow(param_grid)))
      
      # 1) it on training set
      fit1 <- try(
        Arima(train_ts,
              order    = c(pars$p, pars$d, pars$q),
              seasonal = list(order = c(pars$P, pars$D, pars$Q),
                              period = period)),
        silent = TRUE
      )
      
      if (inherits(fit1, "try-error")) {
        return(data.frame(pars, MAE = NA, RMSE = NA))
      }
      
      # 2) refit same model on validation
      fit2 <- try(Arima(valid_ts, model = fit1), silent = TRUE)
      if (inherits(fit2, "try-error")) {
        return(data.frame(pars, MAE = NA, RMSE = NA))
      }
      
      # 3) extract MAE & RMSE from the refitted model
      acc <- accuracy(fit2)
      # first row is 'Training set' on valid_ts
      mae_val  <- acc[1, "MAE"]
      rmse_val <- acc[1, "RMSE"]
      
      # Return a print of the model performance
      results_msg <- sprintf("Model (p=%d,d=%d,q=%d,P=%d,D=%d,Q=%d) → MAE=%.3f, RMSE=%.3f",
                             pars$p, pars$d, pars$q, pars$P, pars$D, pars$Q, 
                             mae_val, rmse_val)
      message(results_msg)
      
      return(data.frame(pars, MAE = mae_val, RMSE = rmse_val))
    }
    
    # Run in parallel
    results <- future_lapply(
      1:nrow(param_grid),
      function(i) process_row(i, p, train_ts, valid_ts, period),
      future.seed = TRUE
    )
  })
  
  # Combine the results and sort by RMSE and the MAE.
  df <- do.call(rbind, results)
  return(df[order(df$RMSE, df$MAE), ])
}


# ----------------------------------------------------------------------------
# 5. Run SARIMA Grid Search (+- 4hours)
# ----------------------------------------------------------------------------

# Set d and D to 1 as optimal order found during analysis and experimentation
res <- evaluate_sarima_order(train_ts,
                              validation_ts,
                              p_vals = 0:3, d_vals = 1, q_vals = 0:3,
                              P_vals = 0:3, D_vals = 1,   Q_vals = 1:2,
                              period = 48)

#Save the results of the search
write.csv(res,
          file = "Documents/VS Code/Thesis/sarima_order.csv",
          row.names = TRUE)


# ----------------------------------------------------------------------------
# 6. Final Model Fitting and Forecasting
# One step ahead predictions are made without reestimation. https://robjhyndman.com/hyndsight/out-of-sample-one-step-forecasts/
# ----------------------------------------------------------------------------



## 6.1 Fit SARIMA on combined train+validation
best_fit <- Arima(
  train_val_ts,
  order    = c(3, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 48)
)
summary(best_fit)



## 6.2 One-step-ahead forecasting without reestimation
full_fit <- Arima(data_ts, model = best_fit)
onestep <- fitted(full_fit)
forecast_test <- onestep[(valid_end + 1):length(onestep)]

#Check that the first forecasted value is the same as using forecast function
forecast_test[1]
forecast(best_fit, h=1)$mean


## 6.3 Index the result with datetime format
forecast_xts <- `colnames<-`(xts(as.numeric(forecast_test), order.by = index(test)), "sarima")


## 6.4 Save forecasts to CSV
write.zoo(forecast_xts, file = "Documents/VS Code/Thesis/data/sarima_r.csv",
          sep = ",", col.names = TRUE, index.name = 'start_date')

