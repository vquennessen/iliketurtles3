# machine runs

# set working directory
setwd('~/Projects/iliketurtles3/code')

# source needed functions
source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population2.R')
source('reproduction.R')

# model parameters
num_sims <- 10000
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)  # total temp increases
betas <- c(1, 1.12, 1.71, 3.46, 6.93, 11.54, 17.29, 34.49)

for (s in 1:length(scenarios)) {
  
  for (b in 1:length(betas)) {
    
    run_base_model(num_sims, scenarios[s], betas[b])

  }
  
}
