### cluster runs

# load libraries
library(parallel)

# set working directory
setwd('~/Projects/iliketurtles3/code')

# load libraries
library(dplyr)

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population.R')
source('reproduction.R')
source('pop_dynamics.R')

# scenarios <- c(10.5, 12)  # total temp increases
scenarios <- c(1.5)  # total temp increases

betas <- c(16.94, 34.14)
# betas <- c(1, 1.35)

# number of simulations to run
num_sims <- c(12, 22, 32, 42, 52, 62, 72, 82, 92, 102)

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

mclapply(X = arguments, FUN = run_base_model)