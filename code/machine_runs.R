### machine runs

# set working directory
setwd('~/Projects/iliketurtles3/code')

# load libraries
library(dplyr)
library(parallel)

# source functions
source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OSRs_to_betas.R')

# total temp increases
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
# scenarios <- c(0.5)

# OSR values to get full fertilization of females
OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
# OSRs <- c(0.5)

# mating function beta values
betas <- as.numeric(OSRs_to_betas(OSRs))

# number of simulations to run
num_sims <- c(10)

# models
models <- c('P_base', 'P_evol', 'P_evol_high_H', 
            'GM_base', 'GM_evol', 'GM_evol_high_H')

# make dataframe of all combinations of arguments
DF <- expand.grid(scenarios, betas, num_sims, models)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(as.numeric(DF[i, ]))
  
}

########### do the runs ########################################################
# lapply(X = scenarios, 
#        FUN = run_base_model,
#        num_sims, 
#        betas)  
# 
# mapply(FUN = run_base_model, 
#        scenarios,
#        num_sims, 
#        betas) 

lapply(X = arguments, FUN = run_base_model)
