#------------------------------------------------------------------------#
# file: 01 setup
#
# author: Louis
# 
# description: loads library and custom functions 
#------------------------------------------------------------------------#

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
  
)

# list of packages not installed
required_packages <- setdiff(
  package_list
  , rownames(installed.packages())
)

# install any package not installed
lapply(required_packages, install.packages)

# load required packages
lapply(package_list, library, character.only = T) %>% 
  
  # don't print on console  
  invisible()


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

compute_mape <- function(actual,pred){
  mean(abs((actual - pred)/actual))
}

compute_mse <- function(actual,pred) {
  mean(
    (actual - pred)^2
    , na.rm = TRUE
  ) %>% 
    sqrt()
}

compute_mae <- function() {
  
}

compute_mpe <- function() {
  
}

compute_rsquared <- function() {
  
}
