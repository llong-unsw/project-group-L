#------------------------------------------------------------------------#
# file: data exploration
#
# author: Hao
# 
# last modified by: Hao
# 
# description: load initial data sets and perform preliminary data
#              preparation and exploration
#------------------------------------------------------------------------#

# load packages -----------------------------------------------------------

# relevant packages for script
package_list <- c(
  "tidyverse"
  , "lubridate"
  , "data.table"
  , "crayon"
  ,"ggplot2"
)

# list of packages not installed
required_packages <- setdiff(
  package_list
  , rownames(installed.packages())
)

# install any package not installed
lapply(required_packages, install.packages)

# load required packages
lapply(package_list, library, character.only = T)



# read files --------------------------------------------------------------
temperature_nsw    <- fread("data/temperature_nsw.csv")
totaldemand_nsw    <- fread("data/totaldemand_nsw.csv")
forecastdemand_nsw <- fread("data/forecastdemand_nsw.csv")
solar_nsw <- fread("data/solar_nsw.csv")
rainfall_nsw <- fread("data/rainfall_nsw.csv")


# data preparation --------------------------------------------------------

##need to change to full outer join
data <- totaldemand_nsw %>% full_join(temperature_nsw,by=c('DATETIME')) %>% select(-LOCATION,-REGIONID);

##filter rows that do not have actual demand
data <- data %>% filter(!is.na(TOTALDEMAND))

##arrange by date in ascending
data <- data %>% arrange(DATETIME);

##fill in missing temperature (down fill)
data <- data %>% fill(TEMPERATURE)


##create table with period of initial forecast
initial_forecast <- forecastdemand_nsw %>% 
  filter(PERIODID==1) %>% 
  select(DATETIME,
         FORECASTDEMAND)%>%
  rename('INITIAL_FORECAST'='FORECASTDEMAND')

##create table with period of final forecast
final_forecast_period <- forecastdemand_nsw %>% 
  group_by(DATETIME) %>% 
  summarise_at(vars(PERIODID),
               list(PERIODID = max))
final_forecast <- forecastdemand_nsw %>%
  inner_join(final_forecast_period,by=c('DATETIME','PERIODID')) %>%
  select(DATETIME,
         FORECASTDEMAND) %>%
  rename('FINAL_FORECAST'='FORECASTDEMAND')

##join initial and final forecast to table data
data <- data %>% 
  left_join(initial_forecast,by=c('DATETIME')) %>%
  left_join(final_forecast,by=c('DATETIME'))

##fill in missing initial/final forecast demand (down fill)
data <- data %>% 
  fill(INITIAL_FORECAST) %>%
  fill(FINAL_FORECAST)

##convert DATETIME to date -- no longer required, reading in as right field on auto
##data$DATETIME = ymd_hms(data$DATETIME);

##create year/month/day/hour/minute fields
data <- data %>% mutate(YEAR=year(DATETIME), MONTH=month(DATETIME),DAY=day(DATETIME));
data <- data %>% mutate(HOUR=hour(DATETIME), MINUTE=minute(DATETIME));

##season
data <- data %>%mutate(SEASON = case_when(
  MONTH %in%  9:11 ~ "SPRING",
  MONTH %in%  c(12, 1, 2)  ~ "SUMMER",
  MONTH %in%  3:5  ~ "AUTUMN",
  TRUE ~ "WINTER"));

##Season indicator
data$SPRING <- if_else(data$SEASON == 'SPRING',1,0)
data$SUMMER <- if_else(data$SEASON == 'SUMMER',1,0)
data$AUTUMN <- if_else(data$SEASON == 'AUTUMN',1,0)
data$WINTER <- if_else(data$SEASON == 'WINTER',1,0)

##create boolean for 8am~8pm timeslot
data <- data %>%mutate(timeframe_8amto8pm = case_when(
  HOUR %in%  8:20 ~ TRUE,
  TRUE ~ FALSE));
##Get days of week, monday = 1,...,sunday=7
data<- data%>%mutate(DaysOfWeek=lubridate::wday(DATETIME,week_start = getOption("lubridate.week.start", 1)));
##get weekday&weekend
data <- data %>%mutate(WEEKDAYWEEKEND = case_when(
  DaysOfWeek %in%  1:5 ~ "WEEKDAY",
  DaysOfWeek %in%  6:7  ~ "WEEKEND"
))

##Create DATE and MONTHDATE field for joining/plotting
data$DATE <- as.Date(substr(data$DATETIME,1,10))
data$YEARMONTH<- as.Date(paste(substr(data$DATE,1,7), "-01", sep=""))

##Join solar and rainfall
rainfall_nsw <- rainfall_nsw %>% 
  unite(DATE, Year, Month, Day, sep="-") %>%
  select(DATE, `Rainfall amount (millimetres)`) %>%
  rename('DAILY_RAINFALL_AMT_MM'=`Rainfall amount (millimetres)`)

solar_nsw <- solar_nsw %>% 
  unite(DATE, Year, Month, Day, sep="-") %>%
  select(DATE,`Daily global solar exposure (MJ/m*m)`) %>%
  rename('DAILY_GLBL_SOLAR_EXPOSR'=`Daily global solar exposure (MJ/m*m)`)

solar_nsw$DATE <- as.Date(solar_nsw$DATE)
rainfall_nsw$DATE <- as.Date(rainfall_nsw$DATE)

data <- data %>% 
  left_join(rainfall_nsw,by=c('DATE')) %>%
  left_join(solar_nsw,by=c('DATE'))


##Deduplicate
data <- data %>% distinct()


# data exploration --------------------------------------------------------

##Calculate lag of current and subsequent datetime
data_exp <- data %>%  mutate(DATETIME_DIFF = DATETIME - lag(DATETIME))
table(data_exp$DATETIME_DIFF)

##Calculate MAE and MSE
data_exp <- data_exp %>%
  mutate(absoluteError = abs(FINAL_FORECAST - TOTALDEMAND),
         SE = (FINAL_FORECAST - TOTALDEMAND)**2)
##Output MAE and MSE
MAE <- mean(data_exp$absoluteError)
MSE <- mean(data_exp$SE)
##Output as dataframe
Error_measure <- c('MAE','MSE')
Values <- c(MAE,MSE)
MAE_MSE <- data.frame(Error_measure,Values)
rm(MAE,MSE,Error_measure,Values)
print(MAE_MSE)
