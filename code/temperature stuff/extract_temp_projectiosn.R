# extract and clean up future projected air temp and SST data

# data: https://ipcc-browser.ipcc-data.org/browser/dataset?id=156

# set working directory
setwd('~/Projects/iliketurtles3')

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
sim1C <- nc_open('data/Panel_a2_Simulated_temperature_change_at_1C.nc') 
# extract time (since lat and long are all the same)
t1 <- ncvar_get(sim1C, "time")

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