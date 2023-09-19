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
  , "zoo"
  , "tsibble"
  , "timeDate"
  , "gridExtra"
  , "scales"
  , "grid"
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

#apply summary- seems to be okay
summary(totaldemand_nsw)

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

#Temperature has some questionable measures
summary(temperature_nsw) #comes from base data

#strange values start from -1643
table(temperature_nsw %>% 
        filter(TEMPERATURE <= 0) %>% 
        select(TEMPERATURE) %>% 
        arrange(TEMPERATURE))

summary(temperature_nsw$TEMPERATURE)

#Remove temperatures below -23 (doesn't make any sense for NSW otherwise)
#Reference: http://www.bom.gov.au/climate/extreme/records.shtml

temperature_nsw <- temperature_nsw %>% filter(TEMPERATURE >=-23)
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

#Join date matrix (1 min interval) to temperature data
temperature_nsw <- datematrix1m %>% left_join(temperature_nsw,by=c('DATETIME'))

#fill in na's via interpolation
#use this as reference: https://journals.ametsoc.org/view/journals/clim/26/19/jcli-d-12-00633.1.xml
temperature_nsw$TEMPERATURE <- na.approx(temperature_nsw$TEMPERATURE)

##Join total demand to temperature
data <- data %>% left_join(temperature_nsw,by=c('DATETIME')) %>% select(-LOCATION,-REGIONID);

##############################
##INITIAL AND FINAL FORECAST##
##############################

#summary for forecast demand table - seems to be fine
summary(forecastdemand_nsw)

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
  summarise(PERIODID = max(PERIODID))

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

################################
##Aggregate the data to hourly##
################################

#Since the forecast made by models will be hourly

##create year/month/day/hour/minute fields
data <- data %>% mutate(YEAR=year(DATETIME), 
                        MONTH=month(DATETIME),
                        DAY=day(DATETIME),
                        HOUR=hour(DATETIME), 
                        MINUTE=minute(DATETIME));

##Create DATE and MONTHDATE field for joining/plotting
data$DATE <- as.Date(paste(year(data$DATETIME), 
                           month(data$DATETIME),
                           day(data$DATETIME), sep = "-"))
data$YEARMONTH<- as.Date(paste(year(data$DATETIME), 
                               month(data$DATETIME),
                               '01', sep = "-"))

#Aggregate the fields
#Agg total demand via sum
#Agg initial/final forecast via sum
#Agg temperature by mean (thought of using max/min, but took the unbiased approach)
data <- data %>%
  group_by(DATE,
           YEARMONTH,
           YEAR,
           MONTH,
           DAY,
           HOUR) %>%
  summarise(TOTALDEMAND=sum(TOTALDEMAND),
            INITIALFORECAST=sum(INITIAL_FORECAST),
            FINAL_FORECAST=sum(FINAL_FORECAST),
            TEMPERATURE=mean(TEMPERATURE))

###########################################
##DERIVE ADDITIONAL FIELDS (Date related)##
###########################################

##season
data <- data %>%mutate(SEASON = case_when(
  MONTH %in%  9:11 ~ "SPRING",
  MONTH %in%  c(12, 1, 2)  ~ "SUMMER",
  MONTH %in%  3:5  ~ "AUTUMN",
  TRUE ~ "WINTER"));

##Season indicator 
#For Summer and Winter only due to Autumn/Spring having little correlation to actual demand
data$summer_flag <- if_else(data$SEASON == 'SUMMER',1,0)
data$winter_flag <- if_else(data$SEASON == 'WINTER',1,0)

##create boolean for 8am~8pm timeslot, using it as cutoff for day/night
data <- data %>%mutate(DuringDay = case_when(
  HOUR %in%  8:20 ~ 1,
  TRUE ~ 0))

##Get days of week, monday = 1,...,sunday=7
data<- data%>%mutate(DaysOfWeek=lubridate::wday(DATE,week_start = getOption("lubridate.week.start", 1)));
##get weekday&weekend
data <- data %>%mutate(BUSINESS_DAY = case_when(
  DaysOfWeek %in%  1:5 ~ 1,
  DaysOfWeek %in%  6:7  ~ 0
))

##################################################
##DERIVE ADDITIONAL FIELDS (Temperature related)##
##################################################

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

#Create aggregated field for plotting
data <- data %>% mutate(EXTREMES = case_when(
  EXTREME_HOT_DAY == 1 ~ 'EXTREME_HOT_DAY',
  EXTREME_COLD_DAY == 1 ~ 'EXTREME_COLD_DAY',
  HOT_DAY == 1 ~ 'HOT_DAY',
  COLD_DAY == 1 ~ 'COLD_DAY',
  EXTREME_HOT_NIGHT == 1 ~ 'EXTREME_HOT_NIGHT',
  EXTREME_COLD_NIGHT == 1 ~ 'EXTREME_COLD_NIGHT',
  HOT_NIGHT == 1 ~ 'HOT_NIGHT',
  COLD_NIGHT == 1 ~ 'COLD_NIGHT')
)

###########################
##Join solar and rainfall##
###########################

#Combine time based fields
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

#Get only relevant timeframe
rainfall_nsw <- rainfall_nsw[year(rainfall_nsw$DATE) > 2009 & rainfall_nsw$DATE <= '2023-08-01',]
solar_nsw <- solar_nsw[year(solar_nsw$DATE) > 2009 & solar_nsw$DATE <= '2023-08-01',]

#Summary to check stats
summary(rainfall_nsw) #93 NA
summary(solar_nsw) #2 NA

#Filter out NA and sort by date
rainfall_nsw <- rainfall_nsw %>% filter(!is.na(DAILY_RAINFALL_AMT_MM)) %>% arrange(DATE)
solar_nsw <- solar_nsw %>% filter(!is.na(DAILY_GLBL_SOLAR_EXPOSR)) %>% arrange(DATE)

#Determine the intervals for datematrix for totaldemand

rainfall_nsw_test <- rainfall_nsw %>%  mutate(DATE_DIFF = (DATE) - lag((DATE)))
table(rainfall_nsw_test$DATE_DIFF)

solar_nsw_test <- solar_nsw %>%  mutate(DATE_DIFF = (DATE) - lag((DATE)))
table(rainfall_nsw_test$DATE_DIFF)

##Create date matrix separated by 1 day intervals
datematrix1d <- data.frame(DATE=seq(as.POSIXct(min(totaldemand_nsw$DATETIME)), 
                                        as.POSIXct(max(totaldemand_nsw$DATETIME)), 
                                        by="1 day"))

##Join datematrix to total demand
rainfall_nsw <- datematrix1d %>% left_join(rainfall_nsw, by = c('DATE'))
solar_nsw <- datematrix1d %>% left_join(solar_nsw, by = c('DATE'))

#Interpolate the NA's
rainfall_nsw$DAILY_RAINFALL_AMT_MM <- na.approx(rainfall_nsw$DAILY_RAINFALL_AMT_MM)
solar_nsw$DAILY_GLBL_SOLAR_EXPOSR <- na.approx(solar_nsw$DAILY_GLBL_SOLAR_EXPOSR)


#Join to base data
data <- data %>% 
  left_join(rainfall_nsw,by=c('DATE')) %>%
  left_join(solar_nsw,by=c('DATE'))


#######################
##Join public holiday##
#######################

nsw_public_holiday <- holiday_aus(year(min(data$DATE)):year(max(data$DATE)), state = "NSW") %>%
  rename(DATE=date)

data <- data %>%
  left_join(nsw_public_holiday,by=c('DATE'),relationship = "many-to-many")

data$PUBLIC_HOLIDAY <- if_else(is.na(data$holiday),0,1)

#check
data %>% filter(PUBLIC_HOLIDAY==1) %>% select(DATE,holiday) %>% unique()

#######################
##Clean up formatting##
#######################

#Create additional date-hour field
data <- data %>%
  mutate(DATEHOUR = ymd_h(paste(DATE, HOUR)))

#convert relevant col to factors
data <- data %>% 
  mutate(across(all_of(c(
                         "summer_flag",
                         "winter_flag",
                         "DuringDay",
                         "DaysOfWeek",
                         "BUSINESS_DAY",
                         "HOT_DAY",
                         "HOT_NIGHT",
                         "COLD_DAY",
                         "COLD_NIGHT",
                         "EXTREME_HOT_DAY",
                         "EXTREME_HOT_NIGHT",
                         "EXTREME_COLD_DAY",
                         "EXTREME_COLD_NIGHT",
                         "PUBLIC_HOLIDAY")), as.factor))

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

##Summary of measures from key tables
summary(data)

###########################################
##Boxplot of Hourly total demand by month##
###########################################

#Boxplot by 
boxplot_demand_month_hist <- ggplot(data) + geom_boxplot(aes(x = as.factor(MONTH), y = TOTALDEMAND, group = as.factor(MONTH)))+
  labs(title = "2010 Jan ~ 2022 Aug Hourly Total Demand by Month", 
       x = "Month", 
       y = "Total Demand") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))

#Can identify June/July having higher energy consumption compared to other months
#Comparing 2010 and 2021 (2022 data isn't complete)
boxplot_demand_month_2010 <- ggplot(data[data$YEAR==2010,]) + 
  geom_boxplot(aes(x = as.factor(MONTH), y = TOTALDEMAND, group = as.factor(MONTH)))+
  labs(title = "2010 Hourly Total Demand by Month", 
       x = "Month", 
       y = "Total Demand") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))

boxplot_demand_month_2021 <- ggplot(data[data$YEAR==2021,]) + 
  geom_boxplot(aes(x = as.factor(MONTH), y = TOTALDEMAND, group = as.factor(MONTH)))+
  labs(title = "2021 Monthly Total Demand", 
       x = "Month", 
       y = "Total Demand") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
#Again confirmed that July/Aug having the highest demand

grid.arrange(boxplot_demand_month_hist,
             boxplot_demand_month_2010,
             boxplot_demand_month_2021,
             ncol=1)

###########################################################
##Create line chart of forecast versus actual 2010 & 2021##
###########################################################
#Create dataset with mothly aggregate
data_mthly_agg <- data %>% group_by(YEARMONTH) %>% summarise(TOTALDEMAND = sum(TOTALDEMAND),
                                                             FINAL_FORECAST = sum())

#2010 ACTUAL VERSUS PREDICTION
demand_2010 <- ggplot(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,], aes(x=YEARMONTH)) + 
  geom_line(size=1.3, aes(y = TOTALDEMAND), color = "darkred") + 
  geom_line(size=1.3,aes(y = FINAL_FORECAST), color="steelblue", linetype="twodash") +
  labs(title = "2010 Monthly Total Demand", 
       x = "Year - Month", 
       y = "Total Demand") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))

#2021 ACTUAL VERSUS PREDICTION
demand_2021 <- ggplot(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,], aes(x=YEARMONTH)) + 
  geom_line(size=1.3,aes(y = TOTALDEMAND), color = "darkred") + 
  geom_line(size=1.3,aes(y = FINAL_FORECAST), color="steelblue", linetype="twodash") +
  labs(title='2021')+
  labs(title = "2021 Monthly Total Demand", 
       x = "Year - Month", 
       y = "Total Demand") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
#Highest variability is still in Summer/Winter months

#Combine as grid
grid.arrange(demand_2010,
             demand_2021,
             ncol=1)

#########################################################################
##Create 2x1 line chart of total demand/residual against month by years##
#########################################################################

#Comparing total demand by months throughout the years
actual_monthly_by_year <- ggplot(data_mthly_agg[data_mthly_agg$YEARMONTH != '2022-08-01',]
       , aes(x=as.factor(month(YEARMONTH)), 
                           y=TOTALDEMAND, 
                           group=as.factor(year(YEARMONTH)), 
                           color=as.factor(year(YEARMONTH)))) + 
  geom_line(size=1.3) +
  labs(title = "Total Energy Demand by Month", 
       x = "Month", 
       y = "Total Energy Demand ",
       color = "Year") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
#Variability increases at highest prior season change

#Comparing variability throughout the years
AEMO_residual <- ggplot(data_mthly_agg, aes(x=as.factor(month(YEARMONTH)), 
                           y=TOTALDEMAND-FINAL_FORECAST, 
                           group=as.factor(year(YEARMONTH)), 
                           color=as.factor(year(YEARMONTH)))) + 
  geom_line(size=1.3) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title = "Monthly Residual of AEMO Prediction versus Actual Energy Demand", 
       x = "Month", 
       y = "AEMO Forecast - Actual Total Energy Demand",
       color = "Year") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
#Variability increases at highest prior season change

#Combine as grid
grid.arrange(actual_monthly_by_year,
             AEMO_residual,
             ncol=1)

##########################################
##Create total demand / seasonal boxplot##
##########################################

ggplot(data) + 
  geom_boxplot(aes(x = SEASON, y = TOTALDEMAND, group = SEASON)) +
  labs(title='Demand by Season') +
  labs(title = "Total Demand by Season", 
       x = "Season", 
       y = "Total Hourly Demand") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
#Higher demand in Winter as previously identified

##############################################################
##Create 2x2 scatter plot of Demand against different fields##
##############################################################

##Create scatterplot of temperature versus total demand
#Sampling at 10% for run times sake
data_sample <- sample_frac(data, 0.1)

#create function for scatter 
scatter_base <- function(y,x,titlelab,xlab,ylab,legenedlab) { 
  season_scatter <- ggplot(data=NULL,aes(x, y)) + 
    geom_point(color='darkblue') +
    labs(title = titlelab, 
         x = xlab, 
         y=ylab) +
    theme(legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(0.5, 'cm'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.title = element_text(size=7), 
          legend.text = element_text(size=7),
          axis.title=element_text(size=8,face="bold")) +
    scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
}

#base temp demand scatter

base_demand_temp_scatter <- scatter_base(data_sample$TOTALDEMAND,
                                         data_sample$TEMPERATURE,
                                         "Against Hourly Temperature",
                                         "Average Temperature (Hourly)",
                                         "Total Energy Demand (Hourly)")


#Aggregate data to daily due to solar and rainfall being recorded daily
data_daily_solar_rainfall <- data %>%
  group_by(DATE) %>%
  summarise(Total_daily_Demand = sum(TOTALDEMAND),
            solar_daily = mean(DAILY_GLBL_SOLAR_EXPOSR),
            rainfall_daily = mean(DAILY_RAINFALL_AMT_MM))

data_daily_solar_rainfall_sample <- sample_frac(data_daily_solar_rainfall, 0.1)


#base rainfall demand scatter
#Little to none relevance
base_demand_rainfall_scatter <- scatter_base(data_daily_solar_rainfall_sample$Total_daily_Demand,
                                             data_daily_solar_rainfall_sample$rainfall_daily,
                                             "Against Daily Rainfall",
                                             "Total Rainfall (Daily)",
                                             "Total Energy Demand (Daily)")

#base solar demand scatter
#Little to none relevance
base_demand_solar_scatter <- scatter_base(data_daily_solar_rainfall_sample$Total_daily_Demand,
                                          data_daily_solar_rainfall_sample$solar_daily,
                                          "Against Daily Solar Exposure",
                                          "Total Solar Exposure (Daily)",
                                          "Total Energy Demand (Daily)")

#Combine as grid
grid.arrange(base_demand_temp_scatter,
             base_demand_rainfall_scatter,
             base_demand_solar_scatter,
             ncol=2,
             top = textGrob("Energy Demand against Various Factors",
                            gp=gpar(fontsize=18,font=1)))


#########################################################
##Create 2x2 scatter plot of Demand against temperature## 
##by different factors                                 ##
#########################################################

#create function for scatter and color
scatter_color <- function(y,x,color,titlelab,xlab,ylab,legenedlab) { 
  season_scatter <- ggplot(data=NULL,aes(x, y , color=color)) + 
    geom_point() +
    labs(title = titlelab, 
         x = xlab, 
         y=ylab,
         colour = legenedlab) +
    theme(legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(0.5, 'cm'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.title = element_text(size=7), 
          legend.text = element_text(size=7),
          axis.title=element_text(size=8,face="bold")) +
    scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
}


#Total demand by temperature, coloured by seasons
#Can keep seasonal indicator for Summer/Winter, Autumn/Spring isn't too significant visually
scatter_season <- scatter_color(data_sample$TOTALDEMAND,
              data_sample$TEMPERATURE,
              data_sample$SEASON,
              "By Seasons",
              "Average Temperature (Hourly)",
              "Total Energy Demand (Hourly)",
              "Season")

#By Extremes
#Cold day, Extreme cold day, and hot day are more noticeable.
scatter_extreme <- scatter_color(data_sample$TOTALDEMAND,
              data_sample$TEMPERATURE,
              data_sample$EXTREMES,
              "By Extreme Temperatures",
              "Average Temperature (Hourly)",
              "Total Energy Demand (Hourly)",
              "Extremeties") 

#By public holiday
#No noticeable difference
scatter_pub_hol <- scatter_color(data_sample$TOTALDEMAND,
              data_sample$TEMPERATURE,
              data_sample$PUBLIC_HOLIDAY,
              "By Public Holiday",
              "Average Temperature (Hourly)",
              "Total Energy Demand (Hourly)",
              "Public Holiday")

#By business day
scatter_bus_day <- scatter_color(data_sample$TOTALDEMAND,
              data_sample$TEMPERATURE,
              data_sample$BUSINESS_DAY,
              "By Business Day",
              "Average Temperature (Hourly)",
              "Total Energy Demand (Hourly)",
              "Business Day")

#Combine as grid
grid.arrange(scatter_season,
             scatter_extreme,
             scatter_pub_hol,
             scatter_bus_day,
             ncol=2,
             top = textGrob("Hourly Total Energy Demand against Average HourlyTemperature",
                            gp=gpar(fontsize=18,font=1)))





