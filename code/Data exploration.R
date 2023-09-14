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
  ,"zoo"
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

#Check for duplicates
totaldemand_nsw[duplicated(totaldemand_nsw)] #39 cases of dupes
temperature_nsw[duplicated(temperature_nsw)] #0 cases
forecastdemand_nsw[duplicated(forecastdemand_nsw)] #0 cases
solar_nsw[duplicated(solar_nsw)] #0 cases
rainfall_nsw[duplicated(rainfall_nsw)] #0 cases

#Deduplicate totaldemand table
totaldemand_nsw <- totaldemand_nsw %>% unique()
totaldemand_nsw[duplicated(totaldemand_nsw)] #no more dupes

################
##TOTAL DEMAND##
################

#Sort by datetime in ascending
totaldemand_nsw <- totaldemand_nsw %>% arrange(DATETIME)

#Determine the intervals for datematrix for totaldemand

totaldemand_nsw_test <- totaldemand_nsw %>%  mutate(DATETIME_DIFF = DATETIME - lag(DATETIME))
table(totaldemand_nsw_test$DATETIME_DIFF)
#2 cases of 10 minutes, rest 5 minutes
#Inferring 2 cases of null at specific 5 min intervals

##Create date matrix separated by 5 minutes intervals
datematrix5m <- data.frame(DATETIME=seq(as.POSIXct(min(totaldemand_nsw$DATETIME)), 
                             as.POSIXct(max(totaldemand_nsw$DATETIME)), 
                             by="5 mins"))

##Join datematrix to total demand
data <- datematrix5m %>% left_join(totaldemand_nsw, by = c('DATETIME'))

#fill in na's via interpolation
data$TOTALDEMAND <- na.approx(data$TOTALDEMAND)

##Confirm 2010-04-30 05:45:00 and 2010-04-30 05:25:00 are no longer null,
##instead having the mean of previous and subsequent timestamp's totaldemand
##view(data %>% filter(DATETIME >= '2010-04-29') %>% filter(DATETIME <= '2010-05-01'))

data %>% filter(is.na(TOTALDEMAND)) #should return 0

###############
##Temperature##
###############

#Sort by datetime in ascending
temperature_nsw <- temperature_nsw %>% arrange(DATETIME)

#Determine the intervals for datematrix for temperature
temperature_nsw_test <- temperature_nsw %>%  mutate(DATETIME_DIFF = DATETIME - lag(DATETIME))
table(temperature_nsw_test$DATETIME_DIFF)
#Irregular intervals

#example of massive temperature gap
temperature_nsw_test$index <- 1:nrow(temperature_nsw_test)
temperature_nsw_test %>% filter(DATETIME_DIFF == 5430)
temperature_nsw_test %>% filter(index %in% 128365:128369)



#Create date matrix with 1 min interval
datematrix1m <- data.frame(DATETIME=seq(as.POSIXct(min(totaldemand_nsw$DATETIME)), 
                                        as.POSIXct(max(totaldemand_nsw$DATETIME)), 
                                        by="1 mins"))
temperature_nsw <- datematrix1m %>% left_join(temperature_nsw,by=c('DATETIME'))
#fill in na's via interpolation
#use this as reference: https://journals.ametsoc.org/view/journals/clim/26/19/jcli-d-12-00633.1.xml
temperature_nsw$TEMPERATURE <- na.approx(temperature_nsw$TEMPERATURE)

##Join total demand to temperature
data <- data %>% left_join(temperature_nsw,by=c('DATETIME')) %>% select(-LOCATION,-REGIONID);

##############################
##INITIAL AND FINAL FORECAST##
##############################
#Sort by datetime in ascending
forecastdemand_nsw <- forecastdemand_nsw %>% arrange(DATETIME,PERIODID)

##create table with period of initial forecast
initial_forecast <- forecastdemand_nsw %>% 
  filter(PERIODID==1) %>% 
  select(DATETIME,
         FORECASTDEMAND)%>%
  rename('INITIAL_FORECAST'='FORECASTDEMAND')

##Check time interval
initial_forecast_test <- initial_forecast %>%  mutate(DATETIME_DIFF = DATETIME - lag(DATETIME))
table(initial_forecast_test$DATETIME_DIFF)
#Mostly 30 minutes, but some large timegaps. Will use down fill

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

##Check time interval
final_forecast_test <- final_forecast %>%  mutate(DATETIME_DIFF = DATETIME - lag(DATETIME))
table(final_forecast_test$DATETIME_DIFF)
#Mostly 30 minutes, but some large timegaps. Will use down fill

#example of massive temperature gap
final_forecast_test$index <- 1:nrow(final_forecast_test)
final_forecast_test %>% filter(DATETIME_DIFF == 25290)
final_forecast_test %>% filter(index %in% 144718:144720)
#2018-04-25 then the next prediction is on 2018-05-12


##join initial and final forecast to table data
data <- data %>% 
  left_join(initial_forecast,by=c('DATETIME')) %>%
  left_join(final_forecast,by=c('DATETIME'))

##fill in missing initial/final forecast demand (down fill), since forecasts 
##are made half-hourly
data <- data %>% 
  fill(INITIAL_FORECAST) %>%
  fill(FINAL_FORECAST)

#NA checks
data %>% filter(is.na(TOTALDEMAND))
data %>% filter(is.na(TEMPERATURE))
data %>% filter(is.na(INITIAL_FORECAST))
data %>% filter(is.na(FINAL_FORECAST))

############################
##DERIVE ADDITIONAL FIELDS##
############################

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
  TRUE ~ FALSE))
data$DuringDay <- if_else(data$timeframe_8amto8pm == TRUE,1,0)

##Get days of week, monday = 1,...,sunday=7
data<- data%>%mutate(DaysOfWeek=lubridate::wday(DATETIME,week_start = getOption("lubridate.week.start", 1)));
##get weekday&weekend
data <- data %>%mutate(WEEKDAYWEEKEND = case_when(
  DaysOfWeek %in%  1:5 ~ "WEEKDAY",
  DaysOfWeek %in%  6:7  ~ "WEEKEND"
))
data$BUSINESS_DAY <- if_else(data$WEEKDAYWEEKEND == 'WEEKDAY',1,0)

##hot/cold day/night
#use as reference temp http://www.bom.gov.au/climate/change/about/extremes.shtml
data$HOT_DAY <- if_else(data$TEMPERATURE > 35 & data$DuringDay == 1, 1, 0)
data$HOT_NIGHT <- if_else(data$TEMPERATURE > 20 & data$DuringDay == 0, 1, 0)
data$COLD_DAY <- if_else(data$TEMPERATURE < 15 & data$DuringDay == 1, 1, 0)
data$COLD_NIGHT <- if_else(data$TEMPERATURE < 5 & data$DuringDay == 0, 1, 0)

##Extemes
data$EXTREME_HOT_DAY <- if_else(data$TEMPERATURE > 40 & data$DuringDay == 1, 1, 0)
data$EXTREME_HOT_NIGHT <- if_else(data$TEMPERATURE > 25 & data$DuringDay == 0, 1, 0)
data$EXTREME_COLD_DAY <- if_else(data$TEMPERATURE < 10 & data$DuringDay == 1, 1, 0)
data$EXTREME_COLD_NIGHT <- if_else(data$TEMPERATURE < 0 & data$DuringDay == 0, 1, 0)

##Create DATE and MONTHDATE field for joining/plotting
data$DATE <- as.Date(substr(data$DATETIME,1,10))
data$YEARMONTH<- as.Date(paste(substr(data$DATE,1,7), "-01", sep=""))


###########################
##Join solar and rainfall##
###########################

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

#reference of filling nulls with interpolation
#https://www.sciencedirect.com/science/article/pii/S0167739X21003794
#https://www.sciencedirect.com/science/article/pii/S0895435621000056
#https://journalofbigdata.springeropen.com/articles/10.1186/s40537-021-00516-9

# data exploration --------------------------------------------------------

##Calculate MAE and MSE
data_exp <- data %>%
  mutate(absoluteError = abs(TOTALDEMAND - FINAL_FORECAST),
         SE = (TOTALDEMAND-FINAL_FORECAST)**2)
##Output MAE and MSE
MAE <- mean(data_exp$absoluteError)
MSE <- mean(data_exp$SE)
##Output as dataframe
Error_measure <- c('MAE','MSE')
Values <- c(MAE,MSE)
MAE_MSE <- data.frame(Error_measure,Values)
rm(MAE,MSE,Error_measure,Values)
print(MAE_MSE)

##Create boxplot of total demand by time 

##Create line chart of forecast versus actual

##Create line chart of residual

##Create total demand / seasonal boxplot

##Create pairwise comparison

##Create scatterplot of temperature versus prediction/actual

