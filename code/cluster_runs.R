### cluster runs

# load libraries
library(parallel)

# set working directory
# setwd('~/Projects/iliketurtles3/code')

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population2.R')
source('reproduction.R')
source('pop_dynamics.R')

# number of simulations to run
# num_sims <- 10000

# number of cores to run
mc <- 16

# scenarios <- c(3.5)  # total temp increases
scenarios <- c(1.5, 3, 4.5, 6, 7.5, 9)  # total temp increases

# betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
# betas <- c(34.14)

########### do the runs ########################################################
mclapply(X = scenarios, 
         FUN = run_base_model, 
         mc.cores = mc)    
