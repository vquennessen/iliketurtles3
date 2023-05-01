# playing around with NicheMapR
# https://mrke.github.io/NicheMapR/inst/doc/microclimate-model-tutorial.html

# set working directory
setwd("~/Projects/iliketurtles3/code")

# load libraries
# remotes::install_github("dklinges9/mcera5") 
library(mcera5)
# remotes::install_github("ilyamaclean/microclima")
library(microclima)
# remotes::install_github("mrke/NicheMapR")
library(NicheMapR)
library(ecmwfr)
library(lubridate)
library(dplyr)
library(tidync)

##### get ERA5 data with package mcera5 (only need to run once) ################
# assign your credentials 
# (register here: https://cds.climate.copernicus.eu/user/register)
# uid <- "######"
uid <- "191215"
# cds_api_key <- "#########-#####-####-####-############"
cds_api_key <- "7db05286-e777-4548-a5d5-d94a734a3ac1"

ecmwfr::wf_set_key(user = uid, key = cds_api_key, service = "cds")

# bounding coordinates
xmn <- -3.9
xmx <- -3.8
ymn <- -32.5
ymx <- -32.4

# temporal extent
st_time <- lubridate::ymd("2019:12:31")
en_time <- lubridate::ymd("2023:03:29")

# file name and location for downloaded .nc files
file_prefix <- "era5"
op <- "../data/Spatial_Data/"

# build a request (covering multiple years)
req <- build_era5_request(xmin = xmn, xmax = xmx,
                          ymin = ymn, ymax = ymx,
                          start_time = st_time,
                          end_time = en_time,
                          outfile_name = file_prefix)
str(req)
request_era5(request = req, uid = uid, out_path = op, overwrite = TRUE)

# help
# https://rdrr.io/github/mrke/NicheMapR/man/micro_era5.html
# https://rdrr.io/github/mrke/NicheMapR/src/R/micro_era5.R - documentation
# https://github.com/mrke/NicheMapR/blob/master/R/micro_era5.R - other doc?
# 

# run micro_era5 for a location (make sure it's within the bounds of 
# your .nc files)
micro <- micro_era5(loc = c(-3.86, -32.43), # Praia do Leao, Fernando de Noronha
                    dstart = "31/12/2019",    # start date
                    dfinish = "29/03/2023",   # end date
                    zmin = 2,       # minimum elevation over sea level in meters
                    slope = 1,      # slope in degrees
                    aspect = 210,   # aspect of beach in degrees, north = 0
                                    # soil depths in cm
                    DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200), 
                    minshade = 0,   # assumed no shade
                    runshade = 0,   # only run once, for minimum shade value
                                    # folder with spatial data from mcera5 pkg
                    spatial = "../data/Spatial_Data/era5", 
                    run.gads = 2)

# save micro as object in data folder
save(micro, '../data/micro.Rdata')

##### run NicheMapR ############################################################

