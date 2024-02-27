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

# total temp increases
# scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
scenarios <- c(0.5)

# mating function betas
# betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
betas <- c(1)

# number of simulations to run
num_sims <- c(100)

# make dataframe of all combinations of arguments
DF <- expand.grid(scenarios, betas, num_sims)

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
