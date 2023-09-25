#------------------------------------------------------------------------#
# file: 03 modelling
#
# author: Louis
# 
# description: modelling component of electricity demand
#------------------------------------------------------------------------#


# set up ------------------------------------------------------------------

# flag to run dependencies (only first time)
run_dependencies = F

# run required scripts
if (run_dependencies) {
  source("Code/01 Setup.R")
  source("Code/02 Data exploration.R")
}

## key parameters

# split data (remaining will be in holdout set)
TRAINING_CUTOFF = as.POSIXct("2018-08-01 00:00:00") # cutoff date for training set
TEST_CUTOFF     = as.POSIXct("2020-08-01 00:00:00") # cutoff date for test set

# set of lagged variables to consider
LAG_SET = c(1:5, 24)
SEED_NUM = 123

# data prep -------------------------------------------------------------

## aggregate data to an hourly level
data %>% 
  
  # set to data.table object
  as.data.table() %>% 
  
  # make a copy
  data.table::copy() %>% 
  
  # select required columns
  .[
    , .(
      DATETIME_HOUR = DATEHOUR
      , HOUR
      , MONTH
      
      , TOTALDEMAND
      , TEMPERATURE
      
      , RAINFALL = DAILY_RAINFALL_AMT_MM
      , SOLAR_EXPOSURE = DAILY_GLBL_SOLAR_EXPOSR
      , HOT_DAY
      , HOT_NIGHT
      , COLD_DAY
      , COLD_NIGHT
      , EXTREME_HOT_DAY
      , EXTREME_HOT_NIGHT
      , EXTREME_COLD_DAY 
      , EXTREME_COLD_NIGHT
      , PUBLIC_HOLIDAY = pub_holiday_flag
    )
  ] %>% 
  
  .[, weekday := lubridate::wday(DATETIME_HOUR, week_start = 1)] %>% 
  
  dummy_cols(
    select_columns = c("MONTH", "weekday", "HOUR")
    , remove_first_dummy = T
  ) %>% 

  # remove columns after dummies are created
  .[, `:=` (
    HOUR = NULL
    , weekday = NULL
    , MONTH = NULL
  )
  ] %>% 
  
  .[order(DATETIME_HOUR)] %>% 
  
  # save output
  force() -> dt_demand_hour

data %>% 
  
  as.data.table() %>% 
  
  .[, .(datetime_hour = DATEHOUR, aemo_demand = FINAL_FORECAST)] %>% 
  
  force() -> dt_aemo

## add split between training, test and holdout sets
# start with hourly table
dt_demand_hour %>% 
  
  # create a copy
  data.table::copy() %>% 
  
  .[, `:=` (
    model_set = fcase(
      # if before Aug 2016, set as training set
      DATETIME_HOUR <= TRAINING_CUTOFF, "training"
      
      # if between Aug 2016 and Aug 2017, set as test set
      , DATETIME_HOUR <= TEST_CUTOFF, "test"
      
      # otherwise, set as holdout set
      , default = "holdout"
    )
    
  )
  ] %>%

  # save output  
  force() -> dt_model

## create variables for lagged demands
lapply(LAG_SET, function(x) {
  dt_model[, paste0("demand_lag_", x) := lag(TOTALDEMAND, x)]
})

# start from time period where there is no missing lag data
dt_model <- dt_model[-c(1:max(LAG_SET)), ]

# list of independent and dependent variables
x_cols <- c(
  "TEMPERATURE"
  , "HOT_DAY"
  , "HOT_NIGHT"
  , "COLD_DAY"
  , "COLD_NIGHT"
  , "EXTREME_HOT_DAY"
  , "EXTREME_HOT_NIGHT"
  , "EXTREME_COLD_DAY"
  , "EXTREME_COLD_NIGHT"
  , "PUBLIC_HOLIDAY"
  , "RAINFALL"
  , "SOLAR_EXPOSURE"
  , paste0("HOUR_", 1:23)
  , paste0("weekday_", 2:7)
  , paste0("MONTH_", 2:12)
  , paste0("demand_lag_", LAG_SET)
)

# dependent variable
y_cols <- "TOTALDEMAND"

# lasso regression --------------------------------------------------------
factor_cols = c(
  "HOT_DAY"
  , "HOT_NIGHT"
  , "COLD_DAY"
  , "COLD_NIGHT"
  , "EXTREME_HOT_DAY"
  , "EXTREME_HOT_NIGHT"
  , "EXTREME_COLD_DAY"
  , "EXTREME_COLD_NIGHT"
  , "PUBLIC_HOLIDAY"
  , paste0("HOUR_", 1:23)
  , paste0("weekday_", 2:7)
  , paste0("MONTH_", 2:12)
)

model_cols <- c(x_cols, y_cols)

dt_model %>% 
  
  data.table::copy() %>% 
  
  # convert factor columns to numeric type 
  .[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols] %>% 
  
  # scale all variables to ensure shrinkage parameter works properly
  .[, (model_cols) := lapply(.SD, scale), .SDcols = model_cols] %>% 
  
  # save output
  force() -> dt_lasso

# will use cross-validation, so only need one training set
lasso_training_x <- dt_lasso[model_set %in% c("training", "test"), .SD, .SDcols = x_cols] %>% as.matrix()
lasso_training_y <- dt_lasso[model_set %in% c("training", "test"), .SD, .SDcols = y_cols] %>% as.matrix()

## set of lambdas to test
lambda_grid <- 10^seq(-2, 1, by = 0.1)

# perform k-fold cross validation to determine best lambda
lasso_cv <- cv.glmnet(
  x = lasso_training_x
  , y = lasso_training_y
  , alpha = 1 # specification fo lasso regression
  , lambda = lambda_grid
  , nfolds = 5
)

# minimum lambda
lasso_lambda_min <- lasso_cv$lambda.min
print(lasso_lambda_min)

# lambda vs. MSE
plot(lasso_cv)

# run model with min lambda
lasso_best_model <- glmnet(
  x = lasso_training_x
  , y = lasso_training_y
  , alpha = 1 # specification fo lasso regression
  , lambda = lasso_lambda_min
)

# inspect coefficients
print(lasso_best_model$beta)

# lambda pathway plot
plot(
  lasso_cv$glmnet.fit
  , "lambda"
  , label = F
)

# arima -------------------------------------------------------------------

## assumptions
# 1. stationary data
# 2. univariate

ts_demand <- ts(
  data %>% 
    
    # ungroup grouped variables
    ungroup() %>% 
    
    # only need to split into training and holdout set
    filter(DATEHOUR < as.Date(TEST_CUTOFF)) %>% 
    
    # select demand from dataframe
    select(TOTALDEMAND) 
  
  , frequency = 24 * 365.25 # 24 hours x 365.25 days in a year
)  

## exploratory data analysis
# 1. autocorrelation
# 2. cyclic behaviour
# 3. trend estimation and decomposition


# decompose into seasonality, trend and noise
ts_decomp <- decompose(ts_demand)
plot(ts_decomp)

# correlation with previous states
par(mfrow = c(2, 1))
acf(ts_demand, lag.max = 48) # autocorrelation
pacf(ts_demand, lag.max = 48) # partial autocorrelation

## modelling
# arima_best_fit <- list(aicc = Inf) # start with max aicc
# 
# for (i in 1:max(LAG_SET)) {
#   
#   # automatically fit best arima model
#   arima_fit <- auto.arima(
#     ts_demand
#     , xreg = fourier(ts_demand, K = i) # fourier models to handle long periods of seasonality
#     , seasonal = F # seasonal models only
#   )
#   
#   # if aicc improves, update best model
#   if (arima_fit$aicc < arima_best_fit$aicc) {
#     arima_best_fit <- arima_fit
#     best_k <- i
#   } else {
#     break
#   }
# }

# function advised from https://robjhyndman.com/hyndsight/forecasting-weekly-data/
arima_model <- tbats(ts_demand)

# inspect model
print(arima_model)

# forecast over test set period
test_set_length <- dt_model[model_set == "test", .N]

arima_forecast <- forecast(
  arima_model
  , h = test_set_length
)

plot(arima_forecast)

# performance metrics
summarise_model_performance(
  actual = dt_model[model_set == "test", TOTALDEMAND]
  , pred = arima_forecast$mean
  , model_name = "ARIMA"
)

# compare fitted with actuals 
plot_predictions(
  actual = dt_model[model_set == "test", .(TOTALDEMAND)]
  , pred = data.table(arima_forecast$mean)
  , model = "ARIMA"
) 

# support vector regression -----------------------------------------------

# use inputs defined in lasso as variables are scaled
svr_train <- dt_lasso[model_set == "training", .SD, .SDcols = model_cols]
svr_test  <- dt_lasso[model_set == "test"    , .SD, .SDcols = model_cols]

svr_grid <- list(
  elsilon = seq(0, 1, 0.5)
  , cost = 10^seq(0, 2, 1)
)

svr_model_1 <- tune(
  svm
  , TOTALDEMAND ~ TEMPERATURE + demand_lag_1
  , data = svr_train
  , ranges = svr_grid
)



# random forest -----------------------------------------------------------

## split into training and test for hyperparameter testing 
rf_train <- dt_model[model_set == "training", .SD, .SDcols = model_cols] %>% as.h2o()
rf_test  <- dt_model[model_set == "test"    , .SD, .SDcols = model_cols] %>% as.h2o()

# get actuals in test set
actuals_demand_test <- rf_test[, y_cols]

# get aemo test set
aemo_demand_test <- dt_aemo[
  datetime_hour <= TEST_CUTOFF & datetime_hour > TRAINING_CUTOFF
  , .(aemo_demand)
]

## perform hyperparameter tuning
# set hyperparameter grid
rf_hyper_grid <- list(
  ntrees        = seq(100, 500, by = 100)
  , mtries      = 3:5
  , sample_rate = c(0.5, 0.8, by = 0.1)
  , max_depth   = 5:10
)

# adjust search criteria to control runtime
rf_search_criteria <- list(
  strategy = "RandomDiscrete"
  
  # stops after 10 min
  , max_runtime_secs = 600
  
  # stops if mse has not improved after 10 models
  , stopping_metric = "mse"
  , stopping_tolerance = 1e-4
  , stopping_rounds = 5
)

## run on base model (all factors included)
rf_model_1 <- model_rf(
  training_data = rf_train
  , test_data = rf_test
  , x_cols = x_cols
  , y_cols = y_cols
  , rf_grid = rf_hyper_grid
  , rf_search_criteria = rf_search_criteria
  , id = 7
  , seed_num = SEED_NUM
)

# inspect outputs
rf_model_1$metrics
h2o.varimp_plot(rf_model_1$best_model, num_of_features = 20)

par(mfrow = c(2, 1))

plot_predictions(
  actual = actuals_demand_test
  , pred = rf_model_1$pred$test
  , model = "random forest"
  , start_date = as.Date("2018-01-01")
  , end_date   = as.Date("2020-01-01")
  , input_month = 1
  , input_year = 2019
)

plot_predictions(
  actual = actuals_demand_test
  , pred = aemo_demand_test
  , model = "aemo"
  , start_date = as.Date("2018-01-01")
  , end_date   = as.Date("2020-01-01")
  , input_month = 1
  , input_year = 2019
)

## subset features based on variance importance
x_new <- rf_model_1$var %>% 
  
  # arbitrary cutoff for feature selection
  filter(scaled_importance >= 0.01) %>% 
  
  # get list of variables
  pull(variable)

# re-run model
rf_model_2 <- model_rf(
  training_data = rf_train
  , test_data = rf_test
  , x_cols = x_new
  , y_cols = y_cols
  , rf_grid = rf_hyper_grid
  , rf_search_criteria = rf_search_criteria
  , id = 2
  , seed_num = SEED_NUM
)

# inspect outputs
rf_model_2$metrics
h2o.varimp_plot(rf_model_2$best_model, num_of_features = 20)

plot_predictions(
  actual = rf_test[, y_cols]
  , pred = rf_model_2$pred$test
  , model = "random forest"
)

rbind(
  rf_model_1$metrics
  , rf_model_2$metrics
)

# model selection ---------------------------------------------------------

aemo_holdout_demand <- dt_aemo[datetime_hour > TEST_CUTOFF, aemo_demand]
actual_holdout_demand <- dt_model[model_set == "holdout", TOTALDEMAND]

## aemo benchmark
dt_aemo_summary <- summarise_model_performance(
  actual = actual_holdout_demand
  , pred = aemo_holdout_demand
  , model_name = "aemo"
)

plot_aemo <- plot_predictions(
  actual = actual_holdout_demand
  , pred = aemo_holdout_demand
  , model = "aemo"
) 

## forecast validation set

# lasso model
# predict using holdout set
lasso_holdout_x <- dt_lasso[model_set == "holdout" , .SD, .SDcols = x_cols] %>% as.matrix()
lasso_holdout_y <- dt_lasso[model_set == "holdout" , .SD, .SDcols = y_cols] %>% as.matrix()

lasso_pred_scaled <- predict(
  lasso_best_model
  , lasso_holdout_x
)

# unscale data
demand_mean <- mean(dt_model$TOTALDEMAND)
demand_sd <- sd(dt_model$TOTALDEMAND)
lasso_pred <- (lasso_pred_scaled * demand_sd) + demand_mean


# performance metrics
dt_lasso_summary <- summarise_model_performance(
  actual = actual_holdout_demand
  , pred = lasso_pred
  , model_name = "lasso"
)

# compare fitted with actuals 
plot_lasso <- plot_predictions(
  actual = actual_holdout_demand
  , pred = lasso_pred
  , model = "lasso"
) 

# arima

# support vector regression

# random forest