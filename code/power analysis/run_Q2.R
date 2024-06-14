# run Q2

# load libraries
library(dplyr)
library(ggplot2)

# load probabilities object
load("~/Projects/iliketurtles3/output/power analysis/probabilities1e+06.Rdata")

# model parameters
pop_size <- 100                               # total population size
Mprob <- c(0.463, 0.318, 0.157, 0.034, 0.028) # probabilities for mating with 1 - max males    
Fprob <- c(22/30, 7/30, 1/30)                 # probabilities for mating with 1 - max females    
nests_mu <- 4.59                              # average # of nests per F
nests_sd <- 2.09                              # sd # of nests per F
eggs_mu <- 100.58                             # average # of eggs per nest
eggs_sd <- 22.68                              # sd # of eggs per nest
breeding <- 'random'                          # fertilization mode
sample_size <- 32                             # sample size of hatchlings
nsims <- 100                               # number of simulations

# run sample_nests
output <- sample_nests(pop_size, Mprob, Fprob, 
                       nests_mu, nests_sd, eggs_mu, eggs_sd, breeding, 
                       sample_sizes, nsims)