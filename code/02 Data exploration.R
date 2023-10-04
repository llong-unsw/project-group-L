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
  , "patchwork"
  , "GGally"
  , "ggcorrplot"
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
solar_nsw          <- fread("data/solar_nsw.csv")
rainfall_nsw       <- fread("data/rainfall_nsw.csv")


# data preparation --------------------------------------------------------

#Check for duplicates
primary_key_check(totaldemand_nsw, pk = "DATETIME") #39 cases of dupes
primary_key_check(temperature_nsw, pk = "DATETIME") #0 cases
primary_key_check(forecastdemand_nsw, pk = c("DATETIME", "PERIODID")) #0 cases
primary_key_check(solar_nsw, pk = c("Year", "Month", "Day")) #0 cases
primary_key_check(rainfall_nsw, pk = c("Year", "Month", "Day")) #0 cases

#Deduplicate totaldemand table
totaldemand_nsw <- totaldemand_nsw %>% unique()
primary_key_check(totaldemand_nsw, pk = "DATETIME") #no more dupes

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

dt_aemo <- final_forecast %>% 
  mutate(datetime_hour = floor_date(ymd_hms(DATETIME), "hour")) %>% 
  group_by(datetime_hour) %>% 
  summarise(aemo_forecast = sum(FINAL_FORECAST))

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

nsw_public_holiday <- nsw_public_holiday %>% select(DATE) %>% mutate(holiday_flag = 1) %>% unique()

data <- data %>%
  left_join(nsw_public_holiday,by=c('DATE'))

data$PUBLIC_HOLIDAY <- if_else(is.na(data$holiday_flag),0,1)
data <- data %>% select(-holiday_flag) %>% rename(pub_holiday_flag=PUBLIC_HOLIDAY)

#check
data %>% filter(pub_holiday_flag==1) %>% select(DATE,pub_holiday_flag) %>% unique()

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
                         "pub_holiday_flag")), as.factor))

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


################################
##Create data frame for charts##
################################
#Create base data excl 2022-08 due to incomplete period (only has the first 5 minutes)
data_Excl_202208 <- data[data$DATEHOUR < '2022-08-01',]

################################################
##Figure 1 Line chart by date-hour, historical##
################################################

##Create base linechart function 
line_base <- function(y,x,titlelab,xlab,ylab) { 
  ggplot(data=NULL, aes(x=x)) + 
    geom_line(linewidth=1.3, aes(y = y), color = "steelblue") +
    labs(title = titlelab, 
         x = xlab, 
         y = ylab) +
    theme(legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(0.5, 'cm'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.title = element_text(size=7), 
          legend.text = element_text(size=7),
          axis.title=element_text(size=8,face="bold"))
}

#add to make y axis k
unit_k <- function(i){
  i+scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
}



#Create chart with date versus various variables
hourly_total_energy_demand_hist <- unit_k(line_base(data_Excl_202208$TOTALDEMAND,
                                                    data_Excl_202208$DATE,
                                                    "Total Energy Demand (MW) per Hour",
                                                    "Year-Month-Day Hour",
                                                    "Total Demand"))

hourly_average_temperature_hist <- line_base(data_Excl_202208$TEMPERATURE,
                                             data_Excl_202208$DATE,
                                             "Temperature (Celsius) per Hour",
                                             "Year-Month-Day Hour",
                                             "Temperature (Celsius)") 


daily_average_rainfall_hist <- line_base(data_Excl_202208$DAILY_RAINFALL_AMT_MM,
                                         data_Excl_202208$DATE,
                                         "Rainfall (mm) per Day",
                                         "Year-Month-Day Hour",
                                         "Rainfall (mm)") 

daily_average_solar_hist <- line_base(data_Excl_202208$DAILY_GLBL_SOLAR_EXPOSR,
                                      data_Excl_202208$DATE,
                                      "Solar Exposure (kj/m**2) per Day",
                                      "Year-Month-Day Hour",
                                      "Solar Exposure (kj/m**2)") 

#Display the charts for fig 1
grid.arrange(hourly_total_energy_demand_hist,
             hourly_average_temperature_hist,
             daily_average_rainfall_hist,
             daily_average_solar_hist,
             ncol=1,
             top = textGrob("Figure 1. yyyy-mm-dd hh of 2010-01 ~ 2022-07",
                            gp=gpar(fontsize=16,font=1))
)

#################################################
##Figure 2 Line chart by year-month, historical##
#################################################

#Create dataset with monthly aggregate, excl final month due to incomplete month
data_mthly_agg <- data %>%
  group_by(YEARMONTH) %>%
  summarise(TOTALDEMAND = sum(TOTALDEMAND),
            FINAL_FORECAST = sum(FINAL_FORECAST),
            TEMPERATURE = mean(TEMPERATURE),
            daily_solar = mean(DAILY_GLBL_SOLAR_EXPOSR),
            daily_rainfall = sum(DAILY_RAINFALL_AMT_MM)) %>% 
  filter(YEARMONTH != '2022-08-01')

#historical monthly aggregate
mthly_total_energy <- unit_k(line_base(data_mthly_agg$TOTALDEMAND,
                                       data_mthly_agg$YEARMONTH,
                                       "Monthly Total Energy Demand (MW)",
                                       "Year - Month",
                                       "Total Demand"))


mthly_average_temperature_hist <- line_base(data_mthly_agg$TEMPERATURE,
                                            data_mthly_agg$YEARMONTH,
                                            "Monthly Average Temperature (Celsius)",
                                            "Year - Month",
                                            "Temperature (Celsius)") 


mthly_total_rainfall_hist <- unit_k(line_base(data_mthly_agg$daily_rainfall,
                                              data_mthly_agg$YEARMONTH,
                                              "Monthly Total Rainfall (mm)",
                                              "Year - Month",
                                              "Rainfall (mm)")) 

mthly_average_solar_hist <- line_base(data_mthly_agg$daily_solar,
                                      data_mthly_agg$YEARMONTH,
                                      "Monthly Average Solar Exposure (kj/m**2)",
                                      "Year - Month",
                                      "Solar Exposure (kj/m**2)") 

#Display the charts for fig 2
grid.arrange(mthly_total_energy,
             mthly_average_temperature_hist,
             mthly_total_rainfall_hist,
             mthly_average_solar_hist,
             ncol=1,
             top = textGrob("Figure 2. 2010-01 ~ 2022-07",
                            gp=gpar(fontsize=16,font=1))
)

################################################################
##Figure 3 Line chart of variable versus month for 2010 & 2021##
################################################################

#monthly total demand 2010 and 2021
mthly_total_energy_2010 <- unit_k(line_base(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,]$TOTALDEMAND,
                                            data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,]$YEARMONTH,
                                            "2010 Monthly Total Energy Demand (MW)",
                                            "Year - Month",
                                            "Total Demand"))
mthly_total_energy_2021 <- unit_k(line_base(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,]$TOTALDEMAND,
                                            data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,]$YEARMONTH,
                                            "2021 Monthly Total Energy Demand (MW)",
                                            "Year - Month",
                                            "Total Demand"))

#monthly average temperature 2010 and 2021
mthly_average_temperature_hist_2010 <- line_base(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,]$TEMPERATURE,
                                                 data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,]$YEARMONTH,
                                                 "2010 Monthly Average Temperature (Celsius)",
                                                 "Year - Month",
                                                 "Temperature (Celsius)") 
mthly_average_temperature_hist_2021 <- line_base(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,]$TEMPERATURE,
                                                 data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,]$YEARMONTH,
                                                 "2021 Monthly Average Temperature (Celsius)",
                                                 "Year - Month",
                                                 "Temperature (Celsius)") 
#monthly total rainfall 2010 and 2021
mthly_total_rainfall_hist_2010 <- unit_k(line_base(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,]$daily_rainfall,
                                                   data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,]$YEARMONTH,
                                                   "2010 Monthly Total Rainfall (mm)",
                                                   "Year - Month",
                                                   "Rainfall (mm)")) 
mthly_total_rainfall_hist_2021 <- unit_k(line_base(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,]$daily_rainfall,
                                                   data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,]$YEARMONTH,
                                                   "2021 Monthly Total Rainfall (mm)",
                                                   "Year - Month",
                                                   "Rainfall (mm)")) 
#monthly average solar exposure 2010 and 2021
mthly_average_solar_hist_2010 <- line_base(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,]$daily_solar,
                                           data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,]$YEARMONTH,
                                           "2010 Monthly Average Solar Exposure (kj/m**2)",
                                           "Year - Month",
                                           "Solar Exposure (kj/m**2)") 
mthly_average_solar_hist_2021 <- line_base(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,]$daily_solar,
                                           data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,]$YEARMONTH,
                                           "2021 Monthly Average Solar Exposure (kj/m**2)",
                                           "Year - Month",
                                           "Solar Exposure (kj/m**2)") 
#Display the charts for fig 3
grid.arrange(mthly_total_energy_2010,
             mthly_total_energy_2021,
             mthly_average_temperature_hist_2010,
             mthly_average_temperature_hist_2021,
             mthly_total_rainfall_hist_2010,
             mthly_total_rainfall_hist_2021,
             mthly_average_solar_hist_2010,
             mthly_average_solar_hist_2021,
             ncol=2,
             top = textGrob("Figure 3. 2010 and 2021",
                            gp=gpar(fontsize=16,font=1))
)


####################################################################
##Figure 4 Line chart of forecast versus actual Hist & 2010 & 2021##
####################################################################
#Historical ACTUAL VERSUS PREDICTION
demand_hist <- ggplot()+
  geom_line(data=data_mthly_agg,aes(y=TOTALDEMAND,x= YEARMONTH,colour="Total Energy Demand"),size=1.3 )+
  geom_line(data=data_mthly_agg,aes(y=FINAL_FORECAST,x= YEARMONTH,colour="AEMO Forecast"),size=1.3,linetype="twodash") +
  scale_color_manual(name = "Lines", values = c("Total Energy Demand" = "darkred", "AEMO Forecast" = "steelblue"))+
  labs(title = "Historical Monthly Total Demand", 
       x = "Year - Month", 
       y = "Total Demand") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))

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
  geom_line(size=1.3, aes(y = TOTALDEMAND), color = "darkred") + 
  geom_line(size=1.3,aes(y = FINAL_FORECAST), color="steelblue", linetype="twodash") +
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


#Combine as grid
grid.arrange(demand_hist,
             demand_2010,
             demand_2021,
             ncol=1,
             top = textGrob("Figure 4 Comparing Monthly Total Energy Demand to AEMO Forecast",
                            gp=gpar(fontsize=18,font=1)))


###########################################################
##Figure 5 Residual of AEMO Forecast with actual forecast##
###########################################################
#Historical residual
residual_hist <- ggplot()+
  geom_line(data=data_mthly_agg,aes(y=TOTALDEMAND-FINAL_FORECAST,x= YEARMONTH),size=1.3,color = "darkred" )+
  labs(title = "Historical Residual", 
       x = "Year - Month", 
       y = "Residual") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))+
geom_hline(yintercept=0, linetype="dashed", color = "black")

#2010 ACTUAL VERSUS PREDICTION
residual_2010 <- ggplot(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2010,], aes(x=YEARMONTH)) + 
  geom_line(size=1.3, aes(y = TOTALDEMAND-FINAL_FORECAST), color = "darkred") + 
  labs(title = "2010 Residual", 
       x = "Year - Month", 
       y = "Residual") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#2021 ACTUAL VERSUS PREDICTION
residual_2021 <- ggplot(data_mthly_agg[year(data_mthly_agg$YEARMONTH) == 2021,], aes(x=YEARMONTH)) + 
  geom_line(size=1.3, aes(y = TOTALDEMAND-FINAL_FORECAST), color = "darkred") + 
  labs(title = "2021 Residual", 
       x = "Year - Month", 
       y = "Residual") +
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size=7),
        axis.title=element_text(size=8,face="bold")) +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


#Combine as grid
grid.arrange(residual_hist,
             residual_2010,
             residual_2021,
             ncol=1,
             top = textGrob("Figure 5 Comparing Monthly Residuals",
                            gp=gpar(fontsize=18,font=1)))

########################################################
##Figure 6 Boxplot of Energy Demand by Year/Month/Hour##
########################################################

#create function for boxplot 
box_base <- function(y,x,titlelab,xlab,ylab) { 
  ggplot(data=NULL) + 
    geom_boxplot(aes(x = x, y = y))+
    labs(title = titlelab, 
         x = xlab, 
         y = ylab) +
    theme(legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(0.5, 'cm'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.title = element_text(size=7), 
          legend.text = element_text(size=7),
          axis.title=element_text(size=8,face="bold"),
    )
}

#Boxplot by year
box_demand_year <- unit_k(box_base(data[data$YEAR <= 2021,]$TOTALDEMAND,
                                   as.factor(data[data$YEAR <= 2021,]$YEAR),
                                   "Average Hourly Total Energy Demand by Year",
                                   "Year",
                                   "Total Energy Demand")
)
#Boxplot by Month
box_demand_month <- unit_k(box_base(data[data$YEAR <= 2021,]$TOTALDEMAND,
                                    as.factor(data[data$YEAR <= 2021,]$MONTH),
                                    "Average Hourly Total Energy Demand by Month",
                                    "Month",
                                    "Total Energy Demand")
)
#Boxplot by Hour
box_demand_hour <- unit_k(box_base(data[data$YEAR <= 2021,]$TOTALDEMAND,
                                   as.factor(data[data$YEAR <= 2021,]$HOUR),
                                   "Average Hourly Total Energy Demand by Hour",
                                   "Hour",
                                   "Total Energy Demand")
)

#Create Fig 4                                
grid.arrange(box_demand_year,
             box_demand_month,
             box_demand_hour, 
             ncol=1,
             top = textGrob("Figure 6 Boxplot of Hourly Total demand by Year/Month/Hour",
                            gp=gpar(fontsize=16,font=1)))


###########################################################
##Figure 7 Density plot of Total Energy Demand and Season##
###########################################################
#Create base function for density plot
density_base <- function(x,group,titlelab,xlab,legenedlab) { 
  season_scatter <- ggplot(data=NULL,aes(x=x, color=group)) +
    geom_density() + 
    labs(title = titlelab, 
         x = xlab,
         color = legenedlab) +
    theme(legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(0.5, 'cm'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.title = element_text(size=7), 
          legend.text = element_text(size=7),
          axis.title=element_text(size=8,face="bold"))
}

#add to make x axis k
unit_k_x <- function(i){
  i+scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3))
}

season_demand_density <- unit_k_x(density_base(data_Excl_202208$TOTALDEMAND,
                                               data_Excl_202208$SEASON,
                                               "Density of Total Energy Demand per Hour by Season",
                                               "Total Energy Demand"
                                               , "Seasons")
)

month_demand_density <- unit_k_x(density_base(data_Excl_202208$TOTALDEMAND,
                                              as.factor(data_Excl_202208$MONTH),
                                              "Density of Total Energy Demand per Hour by Month",
                                              "Total Demand",
                                              "Months")
)

extreme_demand_density <- unit_k_x(density_base(data_Excl_202208$TOTALDEMAND,
                                              as.factor(data_Excl_202208$EXTREMES),
                                              "Density of Total Energy Demand per Hour by Extreme Temperature",
                                              "Total Demand",
                                              "Months")
)
#Draw Figure 7
grid.arrange(season_demand_density,
             month_demand_density,
             extreme_demand_density,
             ncol=1,
             top = textGrob("Figure 7 Density of total demand by Season/Month/Extreme Temperature",
                            gp=gpar(fontsize=18,font=1)))


############################################################
##Figure 8 scatter plot of Demand against different fields##
############################################################

##Create scatterplot of temperature versus total demand
data_sample <- sample_frac(data_Excl_202208, 1)

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
          axis.title=element_text(size=8,face="bold"))
}

#base temp demand scatter

base_demand_temp_scatter <- unit_k(scatter_base(data_sample$TOTALDEMAND,
                                                data_sample$TEMPERATURE,
                                                "Against Hourly Temperature",
                                                "Average Temperature (Hourly)",
                                                "Total Energy Demand (Hourly)"
)
)


#Aggregate data to daily due to solar and rainfall being recorded daily
data_daily_solar_rainfall <- data_Excl_202208 %>%
  group_by(DATE) %>%
  summarise(Total_daily_Demand = sum(TOTALDEMAND),
            solar_daily = mean(DAILY_GLBL_SOLAR_EXPOSR),
            rainfall_daily = mean(DAILY_RAINFALL_AMT_MM))

data_daily_solar_rainfall_sample <- sample_frac(data_daily_solar_rainfall, 1)


#base rainfall demand scatter
base_demand_rainfall_scatter <- unit_k(scatter_base(data_daily_solar_rainfall_sample$Total_daily_Demand,
                                                    data_daily_solar_rainfall_sample$rainfall_daily,
                                                    "Against Daily Rainfall",
                                                    "Total Rainfall (Daily)",
                                                    "Total Energy Demand (Daily)"
)
)

#base solar demand scatter
base_demand_solar_scatter <- scatter_base(data_daily_solar_rainfall_sample$Total_daily_Demand,
                                          data_daily_solar_rainfall_sample$solar_daily,
                                          "Against Daily Solar Exposure",
                                          "Total Solar Exposure (Daily)",
                                          "Total Energy Demand (Daily)")

#Draw Figure 8
grid.arrange(base_demand_temp_scatter,
             base_demand_rainfall_scatter,
             base_demand_solar_scatter,
             ncol=2,
             top = textGrob("Figure 8 Scatterplot of Energy Demand against Various Factors",
                            gp=gpar(fontsize=18,font=1)))



###############################
##Figure 9 Correlation matrix##
###############################
#create spring and autumn flag for correlation
data_Excl_202208$spring_flag <- if_else(data_Excl_202208$SEASON == 'SPRING',1,0)
data_Excl_202208$autumn_flag <- if_else(data_Excl_202208$SEASON == 'AUTUMN',1,0)

dat_var_correl <- data_Excl_202208 %>%
  ungroup() %>%
  select(TOTALDEMAND,
         DATEHOUR,
         TEMPERATURE,
         DAILY_RAINFALL_AMT_MM,
         DAILY_GLBL_SOLAR_EXPOSR,
         BUSINESS_DAY,
         DuringDay,
         pub_holiday_flag,
         spring_flag,
         summer_flag,
         autumn_flag,
         winter_flag,
         HOT_DAY,
         HOT_NIGHT,
         EXTREME_HOT_DAY,
         EXTREME_HOT_NIGHT,
         COLD_DAY,
         COLD_NIGHT,
         EXTREME_COLD_DAY,
         EXTREME_COLD_NIGHT) %>% 
  mutate_all(as.numeric)
var_corr <- round(cor(dat_var_correl), 1)
#Draw Figure 9
ggcorrplot(var_corr, p.mat = cor_pmat(dat_var_correl),
           hc.order = FALSE, 
           type = "upper",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE,
           tl.cex = 5, lab_size = 2,
           title = "Figure 9 Correlation Matrix of Base Data")

####################################################################
##Figure 10/11 Month and Hour correlation with total energy demand##
####################################################################
#Add Month and Hour indicators (numeric) 
data_Excl_202208[paste0("Month_", 1:12)] <- as.data.frame(t(sapply(data_Excl_202208$MONTH, tabulate, 12)))
data_Excl_202208 <- data_Excl_202208 %>% 
  mutate(dummy = 1, Hour1 = HOUR) %>%
  pivot_wider(names_from = Hour1, values_from = dummy,names_glue = "HOUR_{Hour1}", values_fill = 0)

#convert month and hour indicator to factors - not required at the moment
#data_Excl_202208[paste0("Month_", 1:12)] <- lapply(data_Excl_202208[paste0("Month_", 1:12)], factor)
#data_Excl_202208[paste0("HOUR_", 0:23)] <- lapply(data_Excl_202208[paste0("HOUR_", 0:23)], factor)

#Create month correlation matrix
dat_month_correl <- data_Excl_202208 %>%
  ungroup() %>%
  select(TOTALDEMAND,
         starts_with("Month_")
  ) %>% 
  mutate_all(as.numeric)
month_corr <- round(cor(dat_month_correl), 1)

#Draw Figure 10
ggcorrplot(month_corr, p.mat = cor_pmat(dat_month_correl),
           hc.order = FALSE, 
           type = "upper",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE,
           tl.cex = 5, lab_size = 2,
           title = "Figure 10 Correlation Matrix of Total Energy Demand by Month")

#Create hour correlation matrix
#Create month correlation matrix
dat_hour_correl <- data_Excl_202208 %>%
  ungroup() %>%
  select(TOTALDEMAND,
         starts_with("HOUR_")
  ) %>% 
  mutate_all(as.numeric)
hour_corr <- round(cor(dat_hour_correl), 1)

#Draw Figure 11
ggcorrplot(hour_corr, p.mat = cor_pmat(dat_hour_correl),
           hc.order = FALSE, 
           type = "upper",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE,
           tl.cex = 5, lab_size = 2,
           title = "Figure 11 Correlation Matrix of Total Energy Demand by Hour")

##################################################
##Figure 12 Lagged correlation of energy demand##
##################################################
# Create a data frame to store lagged correlations
lags <- 1:24  # Example lag values from 1 to 24 hours
lagged_correlations <- data.frame(Lag = lags)

# Function to calculate lagged correlations
calculate_lagged_correlations <- function(data, variable, lag) {
  lagged_variable <- data[[variable]][1:(nrow(data) - lag)]
  lagged_demand <- data$TOTALDEMAND[(lag + 1):nrow(data)]
  correlation <- cor(lagged_variable, lagged_demand, use = "complete.obs")
  return(correlation)
}

# Calculate lagged correlations for total demand 
variable_demand <- "TOTALDEMAND"
lagged_correlations$totaldemand_Correlation <- sapply(lags, function(lag) {
  calculate_lagged_correlations(data_Excl_202208, variable_demand, lag)
})

#plot total demand lag correlation
plot_demand <- ggplot(lagged_correlations, aes(x = Lag))+ 
  geom_line(aes(y = totaldemand_Correlation), color = "blue", linetype = "solid")+
  labs(
    title = "Lagged Correlations for Total demand",
    x = "Lag (Hours)",
    y = "Correlation"
  ) +
  theme_minimal()

grid.arrange(plot_demand, 
             ncol = 1,
             top = textGrob("Figure 12 Lagged Correlation of Total Energy Demand",
                            gp=gpar(fontsize=18,font=1)))




