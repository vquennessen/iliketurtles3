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

# number of simulations to run
num_sims <- 150

# number of cores to run
mc <- 24

# scenarios <- c(3.5)  # total temp increases
scenarios <- c(3)  # total temp increases

betas <- c(34.14)
# betas <- c(34.14)

########### do the runs ########################################################
mclapply(X = scenarios, 
         FUN = run_base_model, 
         mc.cores = mc/2, 
         num_sims, 
         betas)  