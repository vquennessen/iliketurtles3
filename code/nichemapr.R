# playing around with NicheMapR
# https://mrke.github.io/NicheMapR/inst/doc/microclimate-model-tutorial.html

# set working directory
setwd("~/Projects/iliketurtles3/code")

# load libraries
library(NicheMapR)
# remotes::install_github("ilyamaclean/microclima")
# remotes::install_github("dklinges9/mcera5") - not on CRAN
library(mcera5)

## load global climate data to current working directory; uses 0.5 Gb of data
# get.global.climate(folder = ".")

# set longitude and latitude - 5 decimal points needed? - no diff in output
longlat <- c(-3.86, -32.43) # praia do leao, fernando de noronha, brazil

# run function
micro <- micro_era5(loc = longlat)

# output #######################################################################
# no climate data for this site, using dummy data so solar is still produced 
# extracting soil moisture data
# Sorry, there is no environmental data for this location
# running microclimate model for 12 days by 1 years at site 
# long -3.86 lat -32.43 
# 
# Note: the output column `SOLR` in metout and shadmet is for unshaded 
# horizontal plane solar radiation 
# 
# runtime 0.0799999999871943 seconds
################################################################################