# playing around with NicheMapR
# https://mrke.github.io/NicheMapR/inst/doc/microclimate-model-tutorial.html

# set working directory
setwd("~/Projects/iliketurtles3/code")

# load libraries
# remotes::install_github("mrke/NicheMapR")
library(NicheMapR)
# remotes::install_github("ilyamaclean/microclima")
library(microclima)
# remotes::install_github("dklinges9/mcera5") # - not on CRAN
library(mcera5)
library(ecmwfr)
library(lubridate)
library(dplyr)
library(tidync)

##### get ERA5 data with package mcera5 (only need to run once) ################
# assign your credentials 
# (register here: https://cds.climate.copernicus.eu/user/register)
uid <- "191215"
cds_api_key <- "7db05286-e777-4548-a5d5-d94a734a3ac1"
ecmwfr::wf_set_key(user = uid, key = cds_api_key, service = "cds")

# bounding coordinates
xmn <- -3.9
xmx <- -3.8
ymn <- -32.5
ymx <- -32.4

# temporal extent
st_time <- lubridate::ymd("2019:12:31")
en_time <- lubridate::ymd("2023:03:31")

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

# run micro_era5 for a location (make sure it's within the bounds of 
# your .nc files)

dstart <- "31/12/2019"
dfinish <- "31/03/2023"
loc <- c(-3.86, -32.43) # Praia do Leao, Fernando de Noronha
micro <- micro_era5(loc = loc, 
                    dstart = dstart, 
                    dfinish = dfinish, 
                    spatial = 'c:/Spatial_Data/era5')

##### run NicheMapR ############################################################

