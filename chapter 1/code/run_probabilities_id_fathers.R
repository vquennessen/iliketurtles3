# run probabiliti_id_fathers function

# set working directory - desktop
setwd('~/Projects/multiple_paternity_power_analyses')

# # set working directory - cluster
# setwd('/home/quennessenv/multiple_paternity_power_analyses')

# source code
source('code/probability_id_fathers.R')

# function parameters
hatchlings_mu = 100.58
hatchlings_sd = 22.61
max_fathers = 5
n_sims = 1e5
sample_sizes = c(32, 96)
paternal_contribution_modes = c('random', 
                                'exponential', 
                                'dominant50', 
                                'dominant70', 
                                'dominant90',
                                'mixed_dominant')
min_nest_size = 10

output <- probability_id_fathers(hatchlings_mu,
                       hatchlings_sd,
                       max_fathers,
                       n_sims,
                       sample_sizes,
                       paternal_contribution_modes,
                       min_nest_size)

probabilities <- output[[1]]

# save probabilities DF
save(probabilities,
     file = paste('output/probabilities_', n_sims, '.Rdata', sep = ''))

probabilities_pretty <- output[[2]]

# save prettier probabilities DF
save(probabilities_pretty,
     file = paste('output/probabilities_pretty_', n_sims, '.Rdata', sep = ''))
