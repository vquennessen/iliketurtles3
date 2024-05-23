# run power analyses for Q1

# set working directory
setwd('~/Projects/iliketurtles3/code/power analysis/')

# load libraries
library(viridis)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# source function
source('hatchlings_to_sample.R')

# function parameters
hatchlings_mu <- 100.58                  # number of eggs per nest, mean
hatchlings_sd <- 22.61                   # number of eggs per nest, SD
max_males <- 5                           # max # of M F can mate with
n_sims <- 1000000                        # number of simulations to run
n_sizes <- c(32, 96)                     # sample sizes to calculate probs for  
computer <- 'desktop'                    # computer
fertilization_modes <- c('random',       # fertilization modes
                         'exponential', 
                         'dominant50', 
                         'dominant70', 
                         'dominant90', 
                         'dominant_mixed')

# dimensions
nf <- length(fertilization_modes)
nm <- max_males - 1
ns <- length(n_sizes)
      
# initialize total dataframe of probabilities
probs <- data.frame(NULL)               

# intialize list of figures
figs <- list(NULL)

# run the function for each fertilization mode
for (f in 1:nf) {
  
  # generate output
  output <- hatchlings_to_sample(hatchlings_mu,      # number of eggs per nest, mean
                                 hatchlings_sd,      # number of eggs per nest, SD
                                 max_males,          # max M each F can mate with
                                 fertilization_mode = fertilization_modes[f], 
                                 n_sims,             # number of simulations to run
                                 n_sizes,            # sample sizes to run 
                                 computer)           # which computer is this
  
  # append figure to figs list
  figs[[f]] <- output[[1]]
  
  # append DFsamples to probs
  probs <- rbind(probs, output[[2]])
  
}

# save objects into power analyses output folder

# save table
save(probs, 
     file = paste('~/Projects/iliketurtles3/output/power analysis/probabilities',
                  n_sims, '.Rdata', sep = ''))

# save figures
save(figs, 
     file = paste('~/Projects/iliketurtles3/output/power analysis/figures',
                  n_sims, '.Rdata', sep = ''))
