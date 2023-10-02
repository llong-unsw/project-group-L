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

# overwrite saved models
update_models <- F

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

# table for aemo demand forecast
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

# get mean and variance for unscaling
demand_mean <- dt_model[, mean(TOTALDEMAND)]
demand_sd   <- dt_model[, sd(TOTALDEMAND)]

# lasso regression --------------------------------------------------------
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

if (update_models) {

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
  
  # run model with min lambda
  lasso_best_model <- glmnet(
    x = lasso_training_x
    , y = lasso_training_y
    , alpha = 1 # specification fo lasso regression
    , lambda = lasso_lambda_min
  )
  
  # collect key outputs
  lasso_model_list <- list(
    best_model = lasso_best_model
    , min_lambda = lasso_lambda_min
    , cv = lasso_cv
  )
  
  # save model
  saveRDS(lasso_model_list, "models/lasso.RDS")
}

# read lasso model
lasso_model_list <- readRDS("models/lasso.RDS")

# save best lasso model
lasso_best_model <- lasso_model_list$best_model

# support vector regression -----------------------------------------------

# use inputs defined in lasso as variables are scaled
svr_train <- dt_lasso[
  # control training data size for svm's
  model_set == "training" & DATETIME_HOUR >= as.Date("2014-08-01")
  , .SD, .SDcols = model_cols
]
svr_test  <- dt_lasso[model_set == "test", .SD, .SDcols = model_cols]

# combination of elsilon and cost parameters to test
svr_grid <- CJ(
  epsilon = seq(0, 1, by = 0.5)
  , kernel = c("linear", "radial")
) %>% 
  
  .[, id := .I]

if (update_models) {
  # run svr models with each combination of tuning parameters
  lapply(1:svr_grid[, .N], function(x) {
    epsilon = svr_grid[x, epsilon]
    kernel  = svr_grid[x, kernel]
    
    svr_model = model_svr(
      training_data = svr_train
      , test_data = svr_test
      , model_data = dt_model
      , param_epsilon = epsilon
      , param_kernel = kernel
      , id = x
    )
    
    # save each model separately due to large file size
    saveRDS(svr_model, paste0("models/svr_", x, ".RDS"))
  })
}

# read models
svr_models <- lapply(1:svr_grid[, .N], function(x) {
  readRDS(paste0("models/svr_", x, ".RDS"))
})

# summary table comparing performances with different tuning parameters
svr_summary_table <- rbindlist(
  list(
    svr_models[[1]]$performance
    , svr_models[[2]]$performance
    , svr_models[[3]]$performance
    , svr_models[[4]]$performance
    , svr_models[[5]]$performance
    , svr_models[[6]]$performance
  )
)

# compare fitted with actuals 
# best model
plot_predictions(
  actual = dt_model[model_set == "test", TOTALDEMAND]
  , pred = svr_models[[2]]$fitted_vals
  , model = "svr model 2"
  , start_date = as.Date("2018-08-01")
  , end_date = as.Date("2020-08-01")
  , input_year = 2019
  , input_month = 1
) 

# worst model
plot_predictions(
  actual = dt_model[model_set == "test", TOTALDEMAND]
  , pred = svr_models[[6]]$fitted_vals
  , model = "svr model 6"
  , start_date = as.Date("2018-08-01")
  , end_date = as.Date("2020-08-01")
  , input_year = 2019
  , input_month = 1
) 

# save best model
svr_best_model <- svr_models[[2]]$model

# random forest -----------------------------------------------------------

## split into training and test for hyperparameter testing 
rf_train <- dt_model[model_set == "training", .SD, .SDcols = model_cols]
rf_test  <- dt_model[model_set == "test"    , .SD, .SDcols = model_cols]

# get actuals in test set
actuals_demand_test <- rf_test[, .SD, .SDcols = y_cols]

## perform hyperparameter tuning
# set hyperparameter grid
rf_hyper_grid <- list(
  ntrees        = seq(100, 500, by = 100)
  , mtries      = seq(5, 35, by = 5)
  , sample_rate = c(0.5, 0.8, by = 0.1)
  , max_depth   = 5:10
)

# adjust search criteria to control runtime
rf_search_criteria <- list(
  strategy = "RandomDiscrete"
  
  # stops after 10 min
  , max_runtime_secs = 600
  
  # stops if mse has not improved after 5 models
  , stopping_metric = "mse"
  , stopping_tolerance = 1e-4
  , stopping_rounds = 5
)

if (update_models) {

  ## run on base model (all factors included)
  rf_model_1 <- model_rf(
    training_data = rf_train %>% as.h2o()
    , test_data = rf_test %>% as.h2o()
    , x_cols = x_cols
    , y_cols = y_cols
    , rf_grid = rf_hyper_grid
    , rf_search_criteria = rf_search_criteria
    , id = 1
    , seed_num = SEED_NUM
  )
  
  ## subset features based on variance importance
  x_new <- rf_model_1$var %>% 
    
    # arbitrary cutoff for feature selection
    filter(scaled_importance >= 0.01) %>% 
    
    # get list of variables
    pull(variable)
  
  # re-run model
  rf_model_2 <- model_rf(
    training_data = rf_train %>% as.h2o()
    , test_data = rf_test %>% as.h2o()
    , x_cols = x_new
    , y_cols = y_cols
    , rf_grid = rf_hyper_grid
    , rf_search_criteria = rf_search_criteria
    , id = 2
    , seed_num = SEED_NUM
  )

  rf_list <- list(
    model_1 = rf_model_1
    , model_2 = rf_model_2
    , x_new = x_new
  )
  
  # save model
  saveRDS(rf_list, "models/random_forest.RDS")
  h2o.saveModel(object = rf_model_1$best_model, path = "models", force = TRUE)
  h2o.saveModel(object = rf_model_2$best_model, path = "models", force = TRUE)
}

# read random forest models
rf_list <- readRDS("models/random_forest.RDS")
rf_list$model_1$best_model <- h2o.loadModel("models/rf_grid1_model_132")
rf_list$model_2$best_model <- h2o.loadModel("models/rf_grid2_model_23")

rf_model_1 <- rf_list$model_1
rf_model_2 <- rf_list$model_2

# save final model
rf_best_model <- rf_model_2$best_model

# model selection ---------------------------------------------------------

# set up holdout data set
aemo_holdout_demand <- dt_aemo[datetime_hour > TEST_CUTOFF, aemo_demand]
actual_holdout_demand <- dt_model[model_set == "holdout", TOTALDEMAND]

## aemo benchmark
dt_aemo_summary <- summarise_model_performance(
  actual = actual_holdout_demand
  , pred = aemo_holdout_demand
  , model_name = "aemo (benchmark)"
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

# support vector regression
svr_holdout <- dt_lasso[model_set == "holdout", .SD, .SDcols = model_cols]

# fit in test set
svr_pred_scaled <- predict(
  svr_best_model
  , newdata = svr_holdout[
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
svr_pred <- (svr_pred_scaled * demand_sd) + demand_mean

# performance metrics
dt_svr_summary <- summarise_model_performance(
  actual = actual_holdout_demand
  , pred = svr_pred
  , model_name = "svr"
)

# random forest
rf_holdout <- dt_model[model_set == "holdout", .SD, .SDcols = model_cols] %>% as.h2o()
rf_pred <- h2o.predict(rf_model_2$best_model, newdata = rf_holdout) 

# performance metrics
dt_rf_summary <- summarise_model_performance(
  actual = rf_holdout[, "TOTALDEMAND"]
  , pred = rf_pred
  , model_name = "random forest"
)





