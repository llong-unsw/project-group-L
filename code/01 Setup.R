#------------------------------------------------------------------------#
# file: 01 setup
#
# author: Louis
# 
# description: loads library and custom functions 
#------------------------------------------------------------------------#


# clear environment -------------------------------------------------------
rm(list = ls())
gc()

# load packages -----------------------------------------------------------

# relevant packages for script
package_list <- c(
  "tidyverse"
  , "lubridate"
  , "data.table"
  , "crayon"
  , "zoo"
  , "tsibble"
  , "timeDate"
  , "glmnet"
  , "randomForest"
  , "forecast"
  , "h2o"
  , "scales"
  , "grid"
  , "gridExtra"
  , "fastDummies"
  , "e1071"
  , "knitr"
  , "kableExtra"
)

# list of packages not installed
required_packages <- setdiff(
  package_list
  , rownames(installed.packages())
)

# install any package not installed
lapply(required_packages, install.packages)

# load required packages
suppressMessages(
  invisible(
    lapply(package_list, library, character.only = T) 
  )
)  

# initiate h2o for hyperparameter tuning
h2o.init(max_mem_size = "5g") # max 5 gigabytes

# specify functions
year <- lubridate::year
month <- lubridate::month
day <- lubridate::day
hour <- lubridate::hour
minute <- lubridate::minute
select <- dplyr::select

# custom functions --------------------------------------------------------

# PRIMARY_KEY_CHECK
# ensure that there are no duplicates
primary_key_check <- function(df, pk) {
  
  # start with data.frame
  df %>% 
    
    # confirm data.table
    as.data.table() %>% 
    
    # count rows by primary key
    .[, .(N = .N), by = pk] %>% 
    
    # filter where count exceeds 1
    .[N > 1] %>% 
    
    # count rows
    nrow() %>% 
    
    # print message if greater than 1
    {
      if (. > 0) {
        message(
          paste0(
            "Warning: the primary key ("
            , paste(pk, collapse = ", ")
            , ") is not unique\n"
            , .
            , " cases of duplications found"
          )
        )
      }
    }
  
}

compute_mape <- function(actual, pred){
  mean(
    abs(
      (actual - pred)/actual
    )
  )
}

compute_mse <- function(actual, pred) {
  mean(
    (actual - pred)^2
    , na.rm = TRUE
  )
}

compute_mae <- function(actual, pred) {
  mean(
    abs(actual - pred)
  )
}

compute_rmse <- function(actual, pred) {
  compute_mse(actual, pred) %>% 
    sqrt() 
}

compute_rsquared <- function(actual, pred) {
  rss <- sum((actual - pred)^2) # residual sum of squares
  tss <- sum((actual - mean(actual))^2) # total sum of squares
  
  1 - rss/tss
}

summarise_model_performance <- function(
    actual
    , pred
    , model_name = NULL
    , format = T
) {
  
  # create table of performance metrics
  data.table(
    "model_name"  = model_name
    , "MAPE"      = compute_mape(actual, pred)
    , "MSE"       = compute_mse(actual, pred)
    , "RMSE"      = compute_rmse(actual, pred)
    , "MAE"       = compute_mae(actual, pred)
    , "R-Squared" = compute_rsquared(actual, pred)
  ) %>% 
    
    # format if specified
    {
      if(format) {
        .[, `:=` (
          MAPE          = label_percent(accuracy = 1e-2)(MAPE)
          , MSE         = label_comma()(MSE)
          , RMSE        = label_comma()(RMSE)
          , MAE         = label_comma()(MAE)
          , `R-Squared` = label_percent(accuracy = 1e-2)(`R-Squared`)
        )
        ]
      }
    } %>% 
    
    # return object
    .[]
}

plot_predictions <- function(
    # model outputs  
  actual
  , pred
  
  # start and end date of data - default to holdout set
  , start_date = "2020-08-01"
  , end_date   = "2022-08-01" 
  
  # model name (for title)
  , model = "lasso"
  
  # filter to month year for more clarity
  , input_month = 1
  , input_year = 2021
) {
  
  datetime_seq <- seq(
    from = as.POSIXct(paste0(start_date, "01:00:00"))
    , to = as.POSIXct(paste0(end_date, " 00:00:00"))
    , by = 60 * 60 # every hour
  ) 
  
  # combine data into one table
  p <- data.table(
    datetime_hour = datetime_seq
    , actual = pull(as.data.table(actual))
    , pred   = pull(as.data.table(pred)) 
  ) %>%
    
    suppressWarnings() %>% 
    
    # filter to specified month and year
    .[month(datetime_hour) == input_month & year(datetime_hour) == input_year] %>% 
    
    # create ggplot
    ggplot(., aes(x = datetime_hour)) + 
    
    # plot for actuals
    geom_line(aes(y = actual, colour = "actual")) + 
    
    # plot for predictions
    geom_line(aes(y = pred, colour = "pred")) +
    
    # plot labels
    labs(
      x = "Datetime (hour)"
      , y = "Electricity demand"
      , title = paste0(
        "Actual vs. predicted demand using "
        , model
        , " in "
        , month(input_month, label = T), " ", input_year
      )
    ) +
    
    scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
    
    # set theme
    theme_classic()
  
  return(p)
}

model_svr <- function(
    training_data
    , test_data
    , model_data
    , param_epsilon
    , param_kernel = "linear" 
    , id = 1
) {
  
  # run svr model
  svr_model <- svm(
    TOTALDEMAND 
    ~ TEMPERATURE
    + demand_lag_1
    + demand_lag_2
    + demand_lag_3
    + demand_lag_4
    + demand_lag_5
    + demand_lag_24
    
    , data = training_data
    , kernel = param_kernel 
    , epsilon = param_epsilon
    , tolerance = 0.1 # help control runtime
  )
  
  # fit in test set
  svr_fit_scaled <- predict(
    svr_model
    , newdata = test_data[
      , .(TOTALDEMAND
          , TEMPERATURE
          , demand_lag_1
          , demand_lag_2
          , demand_lag_3
          , demand_lag_4
          , demand_lag_5
          , demand_lag_24)
    ]
  )
  
  # unscale data
  demand_mean <- mean(model_data$TOTALDEMAND)
  demand_sd <- sd(model_data$TOTALDEMAND)
  svr_fit <- (svr_fit_scaled * demand_sd) + demand_mean
  
  # performance metrics
  svr_performance <- summarise_model_performance(
    actual =  model_data[model_set == "test", TOTALDEMAND]
    , pred = svr_fit
    , model_name = paste0("svr_", id)
  )
  
  # return object
  return(
    list(
      model = svr_model
      , fitted_vals = svr_fit
      , performance = svr_performance
    )
  )
}

model_rf <- function(
  # data and column splits
  training_data
  , test_data
  , x_cols
  , y_cols
  
  # tuning specificities
  , rf_grid
  , rf_search_criteria
  , id = 1
  , seed_num = 123
) {
  
  # grid identifier
  rf_grid_id <- paste0("rf_grid", id)
  
  # perform hyperparameter tuning
  rf_models <- h2o.grid(
    grid_id = rf_grid_id
    , algorithm = "randomForest"
    , x = x_cols
    , y = y_cols 
    , seed = seed_num
    , training_frame = training_data
    , hyper_params = rf_grid
    , search_criteria = rf_search_criteria
  )
  
  # extract the best model
  rf_grid_performance <- h2o.getGrid(
    grid_id = rf_grid_id
    , sort_by = "mse"
    , decreasing = F
  )
  
  rf_best_model <- h2o.getModel(rf_grid_performance@model_ids[[1]])
  
  ## create predictions
  rf_pred_training <- h2o.predict(rf_best_model, newdata = training_data) 
  rf_pred_test     <- h2o.predict(rf_best_model, newdata = test_data) 
  
  
  ## assess model performance
  rf_metrics <- rbind(
    summarise_model_performance(training_data[, y_cols], rf_pred_training, paste0("training_", id))
    , summarise_model_performance(test_data[, y_cols], rf_pred_test, paste0("test_", id))
  )
  
  # plot variance importance
  rf_var <- h2o.varimp(rf_best_model)
  
  # collect key outputs
  out <- list(
    best_model = rf_best_model
    , metrics = rf_metrics
    , var = rf_var
    , pred = list(
      train = as.data.table(rf_pred_training)
      , test = as.data.table(rf_pred_test)
    )
  )
  
  # return key elements
  return(out)
}

