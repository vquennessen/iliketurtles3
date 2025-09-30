# plot initialize population test results

# set working directory
setwd('~/Projects/iliketurtles3/code')

# load libraries
library(dplyr)
library(magrittr)
library(ggplot2)

# load in SAD object
load('../output/SAD.Rdata')

# plot proportion ~ age
props <- ggplot(data = SAD, 
                aes(x = Age, y = Proportion, col = factor(Beta))) +
  facet_wrap(facets = vars(Sex), 
             scale = 'free') +
  geom_point()

props