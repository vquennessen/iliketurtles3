### machine runs

# set working directory
setwd('~/Projects/iliketurtles3/code')

# load libraries
library(parallel)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)

# source functions
source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OPMs_to_betas.R')
source('evolution.R')
source('emergence_success.R')
source('proportion_male.R')
source('conservation.R')

# models
# models <- c('P_base', 'P_evol_piv', 'P_evol_piv_high_H',
#             'P_evol_threshold', 'P_evol_threshold_high_H',
#             'GM_base', 'GM_evol_piv', 'GM_evol_piv_high_H',
#             'GM_evol_threshold', 'GM_evol_threshold_high_H')
models <- c('P_evol_piv')

# years to run the model for
years <- 100

# total temp increases
# scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
scenarios <- c(2.5)

# OPM values to get full fertilization of females
# OPMs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
OPMs <- c(0.35)

# mating function beta values
betas <- as.numeric(OPMs_to_betas(OPMs))

# intensity of conservation actions
intensity <- c(0.10)

# frequency of conservation actions (years)
frequency <- c(5)

# number of simulations to run
nsims <- c(5)

# make dataframe of all combinations of arguments
DF <- expand.grid(models, 
                  scenarios, 
                  betas, 
                  years, 
                  nsims, 
                  intensity, 
                  frequency)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(DF[i, ])
  
}

########### do the runs ########################################################
lapply(X = arguments, FUN = run_base_model)
