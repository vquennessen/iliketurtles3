### machine runs

# set working directory
setwd('~/Projects/iliketurtles3/code/power analysis/')

# load libraries
library(dplyr)
library(parallel)

# source function
source('nests_to_sample.R')
source('run_Q2.R')

# load probabilities object
load('number_of_males.Rdata')

# model parameters
# sample_sizes <- c(32, 96)                     # sample sizes of hatchlings
sample_sizes <- c(32)                     # sample sizes of hatchlings

# fertilization_modes <- c('random',            # fertilization modes
#                          'exponential', 
#                          'dominant50', 
#                          'dominant70', 
#                          'dominant90', 
#                          'mixed_dominant')     

fertilization_modes <- c('random')  

#make dataframe of all combinations of arguments
DF <- expand.grid(sample_sizes, fertilization_modes)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(DF[i, ])
  
}

# do the runs
lapply(X = arguments, FUN = run_Q2)
