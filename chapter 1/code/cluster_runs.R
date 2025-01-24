### cluster runs
# cluster
setwd('/home/quennessenv/multiple_paternity_power_analyses')

# load libraries
library(dplyr)
library(parallel)

# source function
source('code/clutches_to_sample.R')
source('code/run_Q2.R')

# load probabilities object
load('output/proportion_correct_all_1e+05.Rdata')

# model parameters
sample_sizes <- c(32, 96)                     

paternal_contribution_modes <- c('random', 
                                 'exponential', 
                                 'dominant50', 
                                 'dominant70', 
                                 'dominant90', 
                                 'mixed_dominant')     

#make dataframe of all combinations of arguments
DF <- expand.grid(sample_sizes, paternal_contribution_modes)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(DF[i, ])
  
}

########### do the runs ########################################################
mclapply(X = arguments, 
         FUN = run_Q2, 
         mc.cores = 50)
