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
dstart <- "31/12/2019"
en_time <- lubridate::ymd("2022:12:31")
dfinish <- "31/12/2022"

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
micro <- micro_era5(
  loc = c(-3.86, -32.43),                 # Praia do Leao, Fernando de Noronha
  dstart = "31/12/2019",                  # start date
  dfinish = "31/12/2022",                 # end date
  dem = NA,                               # default
  dem2 = dem,                             # default
  dem.res = 30,                           # default
  zmin = 2,                               # Bentley et al. 2020
  pixels = 100,                           # default
  nyears = as.numeric(substr(dfinish, 7, 10)) - as.numeric(substr(dstart, 7, 10)) + 1,
  REFL = 0.15,                            # default
  slope = 1,                              # Bentley et al. 2020
  aspect = 210,                           # aspect of beach in degrees, north = 0
  DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200),  # soil depths in cm
  minshade = 0,                           # Bentley et al. 2020
  maxshade = 90,                          # default
  Refhyt = 2,                             # default
  Usrhyt = 0.01,                          # default
  Z01 = 0,                                # default 
  Z02 = 0,                                # default
  ZH1 = 0,                                # default
  ZH2 = 0,                                # default
  runshade = 0,                           # only run once, for min shade value
  run.gads = 0,                           # default
  solonly = 0,                            # default
  Soil_Init = NA,                         # default
  write_input = 0,                        # default
  writecsv = 0,                           # default
  windfac = 1,                            # default
  warm = 0,                               # default
  ERR = 1.5,                              # default
  RUF = 0.004,                            # Kearney et al. 2014
  ZH = 0,                                 # default
  D0 = 0,                                 # default
  EC = 0.0167238,                         # default
  SLE = 0.96,                             # Kearney et al. 2014
  Thcond = 8.8,                           # Kearney et al. 2014
  Density = 2660,                         # Kearney et al. 2014
  SpecHeat = 800,                         # Kearney et al. 2014
  BulkDensity = 1300,                     # Kearney et al. 2014
  PCTWET = 0.1,                           # Kearney et al. 2014
  rainwet = 1.5,                          # default
  cap = 1,                                # default
  CMH2O = 1,                              # default
  hori = rep(NA, 24),                     # default
  runmoist = 0,                           # Bentley et al. 2020
  PE = rep(1.1, 19),                      # default
  KS = rep(0.0037, 19),                   # default
  BB = rep(4.5, 19),                      # default
  BD = rep(BulkDensity, 19),              # default
  DD = rep(Density, 19),                  # default
  maxpool = 10000,                        # default
  rainmult = 1,                           # default
  evenrain = 0,                           # default
  SoilMoist_Init = c(0.1, 0.12, 0.15, 0.2, 0.25, 0.3, 0.3, 0.3, 0.3, 0.3), # default
  L = c(0, 0, 8.2, 8.0, 7.8, 7.4, 7.1, 6.4, 5.8, 4.8, 4.0, 1.8, 0.9, 0.6, 0.8, 0.4 ,0.4, 0, 0) * 10000, # default
  R1 = 0.001,                             # default
  RW = 2.5e+10,                           # default
  RL = 2e+06,                             # default
  PC = -1500,                             # default
  SP = 10,                                # default
  IM = 1e-06,                             # default
  MAXCOUNT = 500,                         # default
  LAI = 0.1,                              # default
  microclima.LAI = 0,                     # default
  LOR = 1,                                # default
  snowmodel = 0,                          # Bentley et al. 2020
  snowtemp = 1.5,                         # default
  snowdens = 0.375,                       # default
  densfun = c(0.5979, 0.2178, 0.001, 0.0038), # default
  snowmelt = 1,                           # default
  undercatch = 1,                         # default
  rainmelt = 0.0125,                      # default
  shore = 0,                              # default
  tides = 0,                              # default
  deepsoil = NA,                          # default
  rainhour = 0,                           # default
  rainhourly = 0,                         # default
  rainoff = 0,                            # default
  lamb = 0,                               # default
  IUV = 0,                                # default
  soilgrids = 0,                          # default
  IR = 0,                                 # default
  message = 0,                            # default
  fail = nyears * 24 * 365,               # default
  spatial = "../data/Spatial_Data/era5",  # folder with spatial data from mcera5
  save = 1,                               # default
  snowcond = 0,                           # default
  intercept = max(maxshade) / 100 * 0.3,  # default
  grasshade = 0,                          # default
  scenario = 0,                           # default
  terra_source = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/data", # default
  coastal = F,                            # default
  hourlydata = NA,                        # default
  dailyprecip = NA,                       # default
  weather.elev = 'era5',                  # default
  cad.effects = TRUE)                     # default

# save micro as object in data folder
save(micro, file = '../data/micro.Rdata')

##### run NicheMapR ############################################################

