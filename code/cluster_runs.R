### cluster runs

# load libraries
library(parallel)
library(dplyr)

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

# # models
# models <- c('P_evol_piv', 'P_evol_piv_high_H',
#             'P_evol_threshold', 'P_evol_threshold_high_H',
#             'GM_evol_piv', 'GM_evol_piv_high_H',
#             'GM_evol_threshold', 'GM_evol_threshold_high_H')
models <- c('GM_evol_threshold_high_H')

# total temp increases
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
# scenarios <- c(4, 5)

# OSR values to get full fertilization of females
OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
# OSRs <- c(0.5)

# mating function beta values
betas <- as.numeric(OSRs_to_betas(OSRs))

# how many clutches to do conservation action on
intensity <- c(0.1)

# how often to do the conservation action (years)
frequency <- c(1)

# years to run the model for
years <- 100

# number of simulations to run
num_sims <- c(10000)

# make dataframe of all combinations of arguments
# DF <- expand.grid(models, scenarios, betas, intensity, frequency, years, num_sims)
DF <- expand.grid(models, scenarios, betas, years, num_sims)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(DF[i, ])
  
}

########### do the runs ########################################################
mclapply(X = arguments, 
         FUN = run_base_model, 
         mc.cores = 50)
