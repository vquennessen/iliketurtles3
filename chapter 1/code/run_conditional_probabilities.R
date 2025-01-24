# run conditional_probabilities function

# set working directory - desktop
setwd('~/Projects/multiple_paternity_power_analyses')

# # set working directory - cluster
# setwd('/home/quennessenv/multiple_paternity_power_analyses')

# load probabilities object
load("output/probabilities_1e+05.Rdata")

# source code
source('code/conditional_probabilities.R')

# run function
output <- conditional_probabilities(probabilities)

# conditional_probabilities
conditional_probabilities <- output[[1]]
save(conditional_probabilities, 
     file = 'output/conditional_probabilities.Rdata', )

# prettier conditional_probabilities
conditional_probabilities_pretty <- output[[2]]
save(conditional_probabilities_pretty, 
     file = 'output/conditional_probabilities_pretty.Rdata')