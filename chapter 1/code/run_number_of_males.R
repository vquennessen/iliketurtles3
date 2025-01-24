# run number_of_males function

# set working directory
setwd('~/Projects/multiple_paternity_power_analyses/')

# function parameters
hatchlings_mu <- 100.58                  # number of eggs per nest, mean
hatchlings_sd <- 22.61                   # number of eggs per nest, SD
max_males <- 5                           # max # of M F can mate with
n_sims <- 1e6                            # number of simulations to run

n_sizes <- c(32, 96)                     # sample sizes to calculate probs for  
fertilization_modes <- c('random',       # fertilization modes
                         'exponential', 
                         'dominant50', 
                         'dominant70', 
                         'dominant90', 
                         'mixed_dominant')
min_nest_size <- 10                      # number of hatchlings in nest if 0