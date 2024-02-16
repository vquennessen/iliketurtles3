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

# scenarios <- c(10.5, 12)  # total temp increases
scenarios <- c(1.5, 3)  # total temp increases

betas <- c(1, 1.94)
# betas <- c(1, 1.35)

# number of simulations to run
num_sims <- c(5)

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
