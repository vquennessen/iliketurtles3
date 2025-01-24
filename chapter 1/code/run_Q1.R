# run power analyses for Q1

# # set working directory - desktop
# setwd('~/Projects/multiple_paternity_power_analyses')

# set working directory - cluster
setwd('/home/quennessenv/multiple_paternity_power_analyses')

# load libraries
library(viridis)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grDevices)

# source function
source('code/hatchlings_to_sample.R')

# function parameters
hatchlings_mu <- 100.58                  
hatchlings_sd <- 22.61                   
max_fathers <- 5                           
n_sims <- 1e5    
# sample_sizes <- c(32, 96)                       
# paternal_contribution_mode <- 'random'      
                                 
sample_sizes <- c(32, 96)                       
paternal_contribution_modes <- c('random',       
                         'exponential', 
                         'dominant50', 
                         'dominant70', 
                         'dominant90', 
                         'mixed_dominant')
      
# initialize data frames of probabilities
proportion_correct_all <- data.frame(NULL)
proportion_correct_samples <- data.frame(NULL)
proportion_correct_samples_pretty <- data.frame(NULL)

# intialize list of figures
figs <- list(NULL)

# run the function for each paternal contribution mode
for (p in 1:length(paternal_contribution_modes)) {
  
  # generate output
  output <- hatchlings_to_sample(hatchlings_mu,
                                 hatchlings_sd,      
                                 max_fathers,          
                                 n_sims,             
                                 sample_sizes,              
                                 paternal_contribution_mode = paternal_contribution_modes[p])

  # append figure to figs list
  figs[[p]] <- output[[1]]
  
  # append DF to proportion_correct
  proportion_correct_all <- rbind(proportion_correct_all, 
                                  output[[2]])
  
  # append DFsamples to proportion_correct
  proportion_correct_samples <- rbind(proportion_correct_samples, 
                                      output[[3]])
  
  # append DFsamples2 to proportion_correct
  proportion_correct_samples_pretty <- rbind(proportion_correct_samples_pretty, 
                                             output[[4]])
  
  # spit out progress update
  print(paste('Done with ', 
              paternal_contribution_modes[p], 
              ' paternal contribution mode!', 
              sep = ''))
  
}

# save objects into power analyses output folder

# save figures
save(figs, 
     file = paste('figures/', 
                  n_sims, 
                  '.Rdata', 
                  sep = ''))

# save table
save(proportion_correct_all, 
     file = paste('output/proportion_correct_all_', 
                  n_sims, 
                  '.Rdata', 
                  sep = ''))

# save table
save(proportion_correct_samples, 
     file = paste('output/proportion_correct_samples_', 
                  n_sims, 
                  '.Rdata', 
                  sep = ''))

# save table
save(proportion_correct_samples_pretty, 
     file = paste('output/proportion_correct_samples_pretty_', 
                  n_sims, 
                  '.Rdata', 
                  sep = ''))
