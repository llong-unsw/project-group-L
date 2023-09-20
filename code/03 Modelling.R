#------------------------------------------------------------------------#
# file: 03 modelling
#
# author: Louis
# 
# description: loads library and custom functions 
#------------------------------------------------------------------------#

# run required scripts
source("Code/01 Setup.R")
source("Code/Data exploration.R")




# setup ---------------------------------------------------------------

# __data prep -------------------------------------------------------------

## aggregate data to an hourly level
data %>% 
  
  # set to data.table object
  as.data.table() %>% 
  
  # make a copy
  data.table::copy() %>% 
  
  # round datetime hour from datetime
  .[, DATETIME_HOUR := floor_date(ymd_hms(DATEHOUR), "hour")] %>% 
  
  # convert indicator variables to factor for aggregation in the next step
  mutate(
    across(
      where(is.factor)
      , ~as.numeric(as.character(.))
    )
  ) %>% 
  
  # summarise to an hourly level for modelling
  .[
    , .(
      TOTALDEMAND = sum(TOTALDEMAND, na.rm = T)
      , TEMPERATURE = mean(TEMPERATURE, na.rm = T)
      
      # as these values are daily, can take the mean over an hour
      , RAINFALL = mean(DAILY_RAINFALL_AMT_MM, na.rm = T)
      , SOLAR_EXPOSURE = mean(DAILY_GLBL_SOLAR_EXPOSR, na.rm = T)
      
      # these flags should be consistent throughout the hour,
      # but aggregate to ensure uniqueness just in case
      , summer_flag = max(summer_flag)
      , winter_flag = max(winter_flag)
      , DuringDay = max(DuringDay)
      , BUSINESS_DAY = max(BUSINESS_DAY)
      
      # if at any point of the hour, temperature exceeds set threshold for these 
      # classifications, set the full hour under this classification
      , HOT_DAY = max(HOT_DAY)
      , HOT_NIGHT = max(HOT_NIGHT)
      , COLD_DAY = max(COLD_DAY)
      , COLD_NIGHT = max(COLD_NIGHT)
      , EXTREME_HOT_DAY = max(EXTREME_HOT_DAY)
      , EXTREME_HOT_NIGHT = max(EXTREME_HOT_NIGHT)
      , EXTREME_COLD_DAY = max(EXTREME_COLD_DAY)
      , EXTREME_COLD_NIGHT = max(EXTREME_COLD_NIGHT)
      , PUBLIC_HOLIDAY = max(PUBLIC_HOLIDAY)
    )
    
    # aggregate by datetime rounded to the nearest hour
    , keyby = DATETIME_HOUR
  ] %>% 
  
  # save output
  force() -> dt_demand_hour

# # convert dummy variables back to factor type
# factor_cols <- c(
#   "SUMMER"
#   , "WINTER"
#   , "DuringDay"
#   , "BUSINESS_DAY"
#   , "HOT_DAY"
#   , "HOT_NIGHT"
#   , "COLD_DAY"
#   , "COLD_NIGHT"
#   , "EXTREME_HOT_DAY"
#   , "EXTREME_HOT_NIGHT"
#   , "EXTREME_COLD_DAY"
#   , "EXTREME_COLD_NIGHT"
#   , "PUBLIC_HOLIDAY"
# )
# 
# dt_demand_hour[
#   , (factor_cols) := lapply(.SD, as.factor)
#   , .SDcols = factor_cols
# ]

# list of independent and dependent variables
x_cols <- c(
  "TEMPERATURE"
  , "summer_flag"
  , "winter_flag"
  , "DuringDay"
  , "BUSINESS_DAY"
  , "PUBLIC_HOLIDAY"
  , "RAINFALL"
  , "SOLAR_EXPOSURE"
)

y_cols <- "TOTALDEMAND"

## add split between training, test and holdout sets
# start with hourly table
dt_demand_hour %>% 
  
  # create a copy
  data.table::copy() %>% 
  
  # create a column for date
  .[, demand_date := as.Date(DATETIME_HOUR)] %>%
  
  .[, `:=` (
    model_set = fcase(
      # if before Aug 2016, set as training set
      demand_date < as.Date("2016-08-01"), "training"
      
      # if between Aug 2016 and Aug 2017, set as test set
      , demand_date < as.Date("2017-08-01"), "test"
      
      # otherwise if Aug 2017 or after, set as holdout set
      , default = "holdout"
    )
    
  )
  ] %>%

  # drop demand date column as no longer needed
  .[, demand_date := NULL] %>% 
  
  # save output  
  force() -> dt_model


# __time series -----------------------------------------------------------

## convert to time series object

# placeholder: fill any na's with zero
dt_model[is.na(dt_model)] <- 0

dt_model %>% 
  
  # filter to 2016 for better visibility
  # .[lubridate::year(DATETIME_HOUR) == 2016] %>%
  
  # select required columns
  .[
    , .SD
    , .SDcols = c(
      # time element
      "DATETIME_HOUR"
      
      # response variable
      , y_cols
    )
  ] %>% 
  
  # convert to time series object
  as_tsibble(index = "DATETIME_HOUR") %>% 
  as.ts(frequency = 24 * 365) %>% # number of data points per year
  
  # save output
  force() -> ts_demand


## decompose into trend, seasonality and error
ts_decomp <- stl(ts_demand, s.window = "p")
plot(ts_decomp)

## correlation with previous states
par(mfrow = c(2, 1))
acf(ts_demand) # autocorrelation
pacf(ts_demand) # partial autocorrelation


## compare raw and deseasonalised trends 
ts_decomp_adj <- seasadj(ts_decomp)

par(mfrow = c(2, 1))
plot(ts_demand, type="l")  # original series
plot(ts_decomp_adj, type="l") # deseasonalised series

## de-trend the data
# estimate required order of differentiating
num_diff <- nsdiffs(ts_demand)

ts_demand %>% 
  
  # # take the log transformation to stabilise variance
  # log() %>% 
  
  # apply difference
  diff(num_diff) %>% 
  
  force() -> ts_diff

# compare difference
par(mfrow = c(2, 1))
plot(ts_demand, type="l")  # original series
plot(ts_diff, type="l") # deseasonalised series

## time delay embedding
lag_order <- 24 # 24 hours of lag
horizon <- 1 # forecast horizon

# include the last 24 hours of data as variables
ts_embed <- embed(ts_diff, lag_order)

# add on other variables
dt_model %>% 
  
  .[-c(1:(lag_order)), ] %>% 
  
  # select required columns
  .[, .SD, .SDcols = x_cols] %>% 
  
  # convert to matrix form
  as.matrix() %>% 
  
  force() -> x_mat

# combine both lagged variables + external variables
x_mat_final <- cbind(ts_embed, x_mat)

# lasso regression --------------------------------------------------------
# TODO: scale variables


## set of lambdas to test
lambda_grid <- 10 ^ seq(-3, 3, by = 0.2)

# set inputs as matrices for lasso regression
lasso_x <- as.matrix(dt_lasso[, .SD, .SDcols = x_cols])
lasso_y <- as.matrix(dt_lasso[, .SD, .SDcols = y_cols])

# perform k-fold cross validation to determine best lambda
lasso_cv <- cv.glmnet(
  x = lasso_x[idx_split$training[1]:idx_split$test[2], ]
  , y = lasso_y[idx_split$training[1]:idx_split$test[2], ]
  , alpha = 1 # specification fo lasso regression
  # , lambda = lambda_grid
  , nfolds = 5
)

# minimum lambda
lasso_lambda_min <- lasso_cv$lambda.min
print(lasso_lambda_min)

# lambda vs. MSE
plot(lasso_cv)

# run model with min lambda
lasso_best_model <- glmnet(
  x = lasso_x[idx_split$training[1]:idx_split$test[2], ]
  , y = lasso_y[idx_split$training[1]:idx_split$test[2], ]
  , alpha = 1 # specification fo lasso regression
  , lambda = lasso_lambda_min
)

# inspect coefficients
lasso_best_model$beta

# lambda pathway plot
plot(
  lasso_cv$glmnet.fit 
  , "lambda"
  , label = F
)

lasso_demand_train <- predict(lasso_best_model, lasso_x[idx_split$training[1]:idx_split$test[2], ])

dt_lasso[idx_split$training[1]:idx_split$test[2]] %>% 
  
  cbind(., lasso_demand_train) %>% 
  
  .[year(DATETIME_HOUR) == 2018 & month(DATETIME_HOUR) == 1] %>% 
  
  ggplot(., mapping = aes(x = DATETIME_HOUR)) + 
  
  geom_line(aes(y = TOTALDEMAND)) + 
  geom_line(aes(y = s0))




# random forest -----------------------------------------------------------
set.seed(123)


## split into training and test 
set_idx <- list(
  training = 1:(dt_model[model_set == "training", .I] - lag_order)
  , test = rownames(dt_model[model_set == "test"]) %>% 
    rownames() %>% 
    as.numeric()
  - lag_order
  , holdout = seq(
    dt_model[model_set == "test", .N] + 1
    , dt_model[model_set == "holdout", .N]
  ) - lag_order
)

x_train_rf <- x_mat_final[set_idx$training, ]

randomForest()

