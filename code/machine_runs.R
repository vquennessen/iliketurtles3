# machine runs

# set working directory
setwd('~/Projects/iliketurtles3/code')

# source needed functions
source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population2.R')
source('reproduction.R')
source('pop_dynamics.R')

# model parameters
# num_sims <- 10000
# scenarios <- c(1.5, 3, 4.5, 6, 7.5, 9, 10.5, 12)  # total temp increases
scenarios <- c(10.5, 12)  # total temp increases

# betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
# # betas <- c(34.14)

for (s in 1:length(scenarios)) {
  
  run_base_model(scenarios[s])
  
}
