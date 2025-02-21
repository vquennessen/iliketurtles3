# soil temperature data from era5
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land-monthly-means?tab=overview
# code from https://rpubs.com/boyerag/297592 

library(ggplot2) # package for plotting
library(tidyr)
library(dplyr)
library(lubridate)
library(rgdal) # package for geospatial analysis
library(raster) # package for raster manipulation
library(ncdf4) # package for netcdf manipulation
library(viridisLite)

##### FdN coordinates ##########################################################

nc_data <- nc_open('../data/era5_FdN.nc')

# Save the print(nc) dump to a text file
{
  sink('../data/era5_FdN.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude", verbose = F)
t <- ncvar_get(nc_data, "valid_time")

stl3.array <- ncvar_get(nc_data, "stl3") # store the data in a 3-dimensional array
dim(stl3.array) 

fillvalue <- ncatt_get(nc_data, "stl3", "_FillValue")
fillvalue
# $value
# [1] NaN

nc_close(nc_data) 

# replace fill values with NA
stl3.array[stl3.array == fillvalue$value] <- NA

r_brick <- brick(stl3.array, 
                xmn = min(lat), 
                xmx = max(lat), 
                ymn = min(lon), 
                ymx = max(lon), 
                crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

FdN_lon <- -32.438293
FdN_lat <- -3.856864
FdN_series <- raster::extract(r_brick, 
                      SpatialPoints(cbind(FdN_lon, FdN_lat)), 
                      method = 'simple')

# returns vector of NA, trying Fortaleza next (very similar latitude, on the 
# Brazilian mainland)

##### Fortaleza ################################################################

nc_data2 <- nc_open('../data/era5_NE_Brazil.nc')

# Save the print(nc) dump to a text file
{
  sink('../data/era5_NE_Brazil.txt')
  print(nc_data2)
  sink()
}

lon <- ncvar_get(nc_data2, "longitude")
lat <- ncvar_get(nc_data2, "latitude", verbose = F)
t <- ncvar_get(nc_data2, "valid_time")

stl3.array2 <- ncvar_get(nc_data2, "stl3") # store the data in a 3-dimensional array
dim(stl3.array2) 

fillvalue <- ncatt_get(nc_data2, "stl3", "_FillValue")
fillvalue
# $value
# [1] NaN

nc_close(nc_data2) 

# replace fill values with NA
stl3.array2[stl3.array2 == fillvalue$value] <- NA

r_brick <- brick(stl3.array2, 
                 xmn = min(lat), 
                 xmx = max(lat), 
                 ymn = min(lon), 
                 ymx = max(lon), 
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- flip(t(r_brick), direction='y')

Fortaleza_lon <- -38.5
Fortaleza_lat <- -3.75
Fortaleza_series <- raster::extract(r_brick, 
                      SpatialPoints(cbind(Fortaleza_lon, Fortaleza_lat)), 
                      method = 'simple')

years <- 1950:2024
months <- c('January', 'February', 'March', 'April', 'May', 'June')

Fortaleza_df <- data.frame(year = rep(years, each = length(months)),
                           month = rep(months, times = length(years)), 
                           stl3K = t(Fortaleza_series))

# make into C
Fortaleza_df$stl3C <- Fortaleza_df$stl3K - 273.15

Fortaleza_pretty <- Fortaleza_df %>%
  dplyr::select(year, month, stl3C) %>%
  pivot_wider(names_from = month, values_from = stl3C)


autocorrelation_coefficients <- 
  data.frame(Lag = 0:18,
             January = c(acf(Fortaleza_pretty$January)[[1]]), 
             February = c(acf(Fortaleza_pretty$February)[[1]]), 
             March = c(acf(Fortaleza_pretty$March)[[1]]), 
             April = c(acf(Fortaleza_pretty$April)[[1]]), 
             May = c(acf(Fortaleza_pretty$May)[[1]]), 
             June = c(acf(Fortaleza_pretty$June)[[1]])) %>%
  dplyr::filter(Lag == 1) %>%
  pivot_longer(cols = 2:7, values_to = 'AutoCoeff') %>%
  dplyr::mutate(Month = factor(name, 
                        levels = months))

mean_auto_coeff <- mean(autocorrelation_coefficients$AutoCoeff)
sd_auto_coeff <- sd(autocorrelation_coefficients$AutoCoeff)

ggplot(data = autocorrelation_coefficients, 
       aes(x = Month, y = AutoCoeff, col = factor(Lag))) +
  geom_hline(yintercept = 0.5, col = 'blue', lty = 2) +
  geom_hline(yintercept = -0.5, col = 'blue', lty = 2) +
  geom_hline(yintercept = 0, col = 'red', lty = 3, lwd = 1) +
  geom_hline(yintercept = mean_auto_coeff, col = 'green', lty = 2) +
  geom_point(size = 1.5) +
  scale_color_manual(values = viridis(18)) +
  ylab('Temporal autocorrelation coefficient')
