### cluster runs

# load libraries
library(parallel)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)

# set working directory
# setwd('~/Projects/iliketurtles3/code')

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OSRs_to_betas.R')
source('evolution.R')
source('emergence_success.R')
source('proportion_male.R')
source('conservation.R')

# models
# models <- c('P_evol_piv', 'P_evol_piv_high_H',
#             'P_evol_threshold', 'P_evol_threshold_high_H',
#             'GM_evol_piv', 'GM_evol_piv_high_H',
#             'GM_evol_threshold', 'GM_evol_threshold_high_H')
# models <- c('P_conservation', 'GM_conservation')
# models <- c('P_base', 'GM_base')
models <- c('P_base', 'GM_base')

# years to run the model for
years <- 100

# total temp increases
scenarios <- years / 100 * c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
# scenarios <- c(0.5)

# OSR values to get full fertilization of females
OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
# OSRs <- c(0.49, 0.35, 0.3, 0.25, 0.15)

# mating function beta values
betas <- as.numeric(OSRs_to_betas(OSRs))

# how many clutches to do conservation action on
# intensity <- c(0.1, 0.2, 0.3, 0.4, 0.5)
intensity <- c(1)

# how often to do the conservation action (years)
# frequency <- c(1, 2, 3, 4, 5)
frequency <- c(1)

# number of simulations to run
num_sims <- c(10000)

# make dataframe of all combinations of arguments
DF <- expand.grid(models, 
                  scenarios, 
                  betas, 
                  years, 
                  num_sims, 
                  intensity, 
                  frequency) %>% 
  arrange(Var2, Var3)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(DF[i, ])
  
}

########### do the runs ########################################################
mclapply(X = arguments, 
         FUN = run_base_model, 
         mc.cores = 30)
