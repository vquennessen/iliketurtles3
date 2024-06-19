# run Q2

# set working directory
setwd('~/Projects/iliketurtles3/code/power analysis/')

# load libraries
library(dplyr)
library(ggplot2)

# source function
source('nests_to_sample.R')

# model parameters
sample_size <- 32                             # sample size of hatchlings
nsims <- 1e5                                  # number of simulations
pop_size <- 100                               # total population size
breeding <- 'random'                          # fertilization mode

# population parameters
Mprob <- c(0.463, 0.318, 0.157, 0.034, 0.028) # probabilities for mating with 1 - max males    
Fprob <- c(22/30, 7/30, 1/30)                 # probabilities for mating with 1 - max females    
nests_mu <- 4.59                              # average # of nests per F
nests_sd <- 2.09                              # sd # of nests per F

# load probabilities object
load('number_of_males.Rdata')

# extract probabilities for 2-5 males
id_probs <- number_of_males %>%
  filter(Sample_size == sample_size) %>%
  filter(Fertilization_mode == breeding) %>%
  select(Males_contributing, Males_identified, Probability)

# run sample_nests
output <- nests_to_sample(nsims, pop_size, breeding, 
                          Mprob, Fprob, nests_mu, nests_sd, id_probs)

# save output
save(output, 
     file = paste(sample_size, '_nests_to_sample_', nsims, '.Rdata', sep = ''))
