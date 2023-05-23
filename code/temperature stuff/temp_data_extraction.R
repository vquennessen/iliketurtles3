# extract and clean up past air temp and SST data

# set working directory
setwd('~/Projects/iliketurtles')

# load libraries
library(ncdf4)
library(dplyr)
library(lubridate)

##### load in and clean up temp data ###########################################

# # load in ncdf4 object
# nc_data1 <- nc_open('data/adaptor.mars.internal-1665024804.1999784-29758-13-579b4eb1-0631-4829-bf0f-d82db7c690ba.nc')
# nc_data2 <- nc_open('data/adaptor.mars.internal-1665013288.9338732-20330-9-4f7bf825-ebce-4709-9351-a14d16742749.nc')

# if on laptop
# nc_data1 <- nc_open('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/data/adaptor.mars.internal-1665024804.1999784-29758-13-579b4eb1-0631-4829-bf0f-d82db7c690ba.nc')
# nc_data2 <- nc_open('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/data/adaptor.mars.internal-1665013288.9338732-20330-9-4f7bf825-ebce-4709-9351-a14d16742749.nc')

# new data, with precipitation
nc_data1 <- nc_open('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/data/adaptor.mars.internal-1673974675.696251-28094-9-5457cdae-0546-481c-adf3-fd373aa3e507.nc')

# extract time (since lat and long are all the same)
t1 <- ncvar_get(nc_data1, "time")

# extract air temp 2m above surface of whatever and SST
temp2m1 <- ncvar_get(nc_data1, "t2m") 
SST1 <- ncvar_get(nc_data1, "sst")
prec1 <- ncvar_get(nc_data1, 'tp')

# what is the fillvalue?
temp2m1_fill <- ncatt_get(nc_data1, "t2m", "_FillValue")
SST1_fill <- ncatt_get(nc_data1, "sst", "_FillValue")
prec1_fill <- ncatt_get(nc_data1, "tp", "_FillValue")

# close the netCDF file
nc_close(nc_data1)

# replace fill values with NAs
temp2m1[temp2m1 == temp2m1_fill$value] <- NA
SST1[SST1 == SST1_fill$value] <- NA
prec1[prec1 == prec1_fill$value] <- NA

# adjust temperatures to Celsius from Kelvin by subtracting 273.15
temp2m <- c(temp2m1 - 273.15)
SST <- c(SST1 - 273.15)

# turn time into datetime 
date_time <- c(as.POSIXct(t1*3600, origin = '1900-01-01 00:00', tz = "UTC"))

# create dataframe with times, temp2ms, and SSTs
weather <- data.frame(Hours = as.character(t1), 
                      date_time = date_time, 
                      temp2m = temp2m, 
                      SST = SST, 
                      prec = c(prec1))

# weather

##### load in and clean up nesting temp data ###################################

season1 <- read.csv('data/temperature_season_1.csv')
season2 <- read.csv('data/temperature_season_2.csv')
season3 <- read.csv('data/temperature_season_3.csv')

# put 3 seasons of data together
nest_temps <- rbind(season1, season2, season3)

nest_temps$Season_Nest <- paste(nest_temps$Season, '_', nest_temps$Nest, 
                                sep = '')

### clean up data

# make time object datetime, and rename as date_time
nest_temps$Time <- round(as.POSIXct(nest_temps$Time, 
                                    format = '%m/%d/%Y  %H:%M', 
                                    tz = 'UTC'), 
                                    units = 'hours')

# rename Temperature column to nest
nest_temps <- rename(nest_temps, nest_temp = Temperature)
nest_temps <- rename(nest_temps, date_time = Time)

# add hours column as number of hours since jan 1st 1900 00:00
origin <-  as.POSIXct(mdy_hms('01/01/1900 00:00:00'), 
                      format = '%m/%d/%Y %H:%M:%S')
nest_temps$Hours <- round(difftime(nest_temps$date_time, origin, tz = 'UTC', 
                                  units = "hours"))
nest_temps$Hours <- as.character(nest_temps$Hours)

##### join dataframes ##########################################################

# remove duplicated rows in data DF
weather2 <- weather[-c(7354, 16090, 24994), ]

# make season and nest character variables
# nest_temp$Nest <- as.factor(nest_temp$Nest)
# nest_temp$Season <- as.factor(nest_temp$Season)

# temporary <- nest_temp %>%
#   left_join(weather2, by = "Hours")

temps_days <- nest_temps %>%
  left_join(weather2) %>%
  mutate(date = date(date_time)) %>%
  na.omit(date) %>%
  group_by(Season, Nest, date) %>%
  summarize(avg_nest_temp = mean(nest_temp, na.rm = TRUE), 
            avg_air_temp = mean(temp2m, na.rm = TRUE), 
            avg_sst = mean(SST, na.rm = TRUE), 
            avg_prec = mean(prec, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Season, Nest) %>%
  mutate(incubation.prop = as.numeric(date - min(date)) /
           max(as.numeric(date - min(date)))) %>%
  ungroup() %>%
  mutate(airlag1 = NA, airlag2 = NA, airlag3 = NA, airlag4 = NA, airlag5 = NA, 
         sstlag1 = NA, sstlag2 = NA, sstlag3 = NA, sstlag4 = NA, sstlag5 = NA,
         preclag1 = NA, preclag2 = NA, preclag3 = NA, preclag4 = NA, preclag5 = NA,
         Season_Nest = paste(Season, '_', Nest, sep = ''))

temps_hours <- nest_temps %>%
  left_join(weather2) %>%
  na.omit(date_time) %>%
  group_by(Season, Nest) %>%
  summarize(avg_nest_temp = nest_temp, 
            avg_air_temp = temp2m, 
            avg_sst = SST, 
            avg_prec = prec,
            date_time = date_time) %>%
  ungroup() %>%
  group_by(Season, Nest) %>%
  mutate(incubation.prop = as.numeric(date_time - min(date_time)) /
           max(as.numeric(date_time - min(date_time)))) %>%
  ungroup() %>%
  mutate(airlag1 = NA, airlag2 = NA, airlag3 = NA, airlag4 = NA, airlag5 = NA, 
         sstlag1 = NA, sstlag2 = NA, sstlag3 = NA, sstlag4 = NA, sstlag5 = NA,
         preclag1 = NA, preclag2 = NA, preclag3 = NA, preclag4 = NA, preclag5 = NA,
         Season_Nest = paste(Season, '_', Nest, sep = ''))

# why are date_time.x and date_time.y different?
# xx <- temps_hours$date_time.x
# yy <- temps_hours$date_time.y
# setdiff(xx, yy)

# troubleshooting why some had NAs for incubation proportion - some dates as NAs 
# sub3_13 <- nest_temp %>%
#   left_join(weather2) %>%
#   mutate(date = date(date_time)) %>%
#   na.omit() %>%
#   group_by(Season, Nest, date) %>%
#   summarize(avg_nest_temp = mean(Temperature, na.rm = TRUE), 
#             avg_air_temp = mean(temp2m, na.rm = TRUE), 
#             avg_sst = mean(SST, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(Season, Nest) %>%
#   filter(Season == 3 & Nest == 13) %>%
#   mutate(incubation.prop = as.numeric(date - min(date)) /
#            max(as.numeric(date - min(date))))

# add in lags
for (i in 6:nrow(temps_days)) {
  
  # air temp lags - days
  temps_days$airlag1[i] <- temps_days$avg_air_temp[i - 1]
  temps_days$airlag2[i] <- temps_days$avg_air_temp[i - 2]
  temps_days$airlag3[i] <- temps_days$avg_air_temp[i - 3]
  temps_days$airlag4[i] <- temps_days$avg_air_temp[i - 4]
  temps_days$airlag5[i] <- temps_days$avg_air_temp[i - 5]
  
  # SST lags - days
  temps_days$sstlag1[i] <- temps_days$avg_sst[i - 1]
  temps_days$sstlag2[i] <- temps_days$avg_sst[i - 2]
  temps_days$sstlag3[i] <- temps_days$avg_sst[i - 3]
  temps_days$sstlag4[i] <- temps_days$avg_sst[i - 4]
  temps_days$sstlag5[i] <- temps_days$avg_sst[i - 5]
  
  # preciptation lags - days
  temps_days$preclag1[i] <- temps_days$avg_prec[i - 1]
  temps_days$preclag2[i] <- temps_days$avg_prec[i - 2]
  temps_days$preclag3[i] <- temps_days$avg_prec[i - 3]
  temps_days$preclag4[i] <- temps_days$avg_prec[i - 4]
  temps_days$preclag5[i] <- temps_days$avg_prec[i - 5]
  
}

# add in lags for hourly data
for (i in 6:nrow(temps_hours)) {
  
  # air temp lags - hours
  temps_hours$airlag1[i] <- temps_hours$avg_air_temp[i - 1]
  temps_hours$airlag2[i] <- temps_hours$avg_air_temp[i - 2]
  temps_hours$airlag3[i] <- temps_hours$avg_air_temp[i - 3]
  temps_hours$airlag4[i] <- temps_hours$avg_air_temp[i - 4]
  temps_hours$airlag5[i] <- temps_hours$avg_air_temp[i - 5]
  
  # SST lags - hours
  temps_hours$sstlag1[i] <- temps_hours$avg_sst[i - 1]
  temps_hours$sstlag2[i] <- temps_hours$avg_sst[i - 2]
  temps_hours$sstlag3[i] <- temps_hours$avg_sst[i - 3]
  temps_hours$sstlag4[i] <- temps_hours$avg_sst[i - 4]
  temps_hours$sstlag5[i] <- temps_hours$avg_sst[i - 5]
  
  # preciptation lags - hours
  temps_hours$preclag1[i] <- temps_hours$avg_prec[i - 1]
  temps_hours$preclag2[i] <- temps_hours$avg_prec[i - 2]
  temps_hours$preclag3[i] <- temps_hours$avg_prec[i - 3]
  temps_hours$preclag4[i] <- temps_hours$avg_prec[i - 4]
  temps_hours$preclag5[i] <- temps_hours$avg_prec[i - 5]
  
}


# remove rows with NAs 
# temps <- temps %>% na.omit()

# x <- unique(nest_temps$Season_Nest)     # 219 nests total
# y <- unique(temps_days$Season_Nest)     # 219 nests total
# z <- unique(temps_hours$Season_Nest)    # 219 nests total

# setdiff(x, y)
# "1_21"  "2_12"  "2_13"  "2_14"  "2_16"  "3_138" "3_212"

# save temps as object for further analysis
save(temps_days, file = 'data/temperature_days.Rda')
save(temps_hours, file = 'data/temperature_hours.Rda')
