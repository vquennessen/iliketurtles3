# plot individual heatmaps 

# make figures representing output

# TODO 
# make this script just for extracting all the probabilities of population
# persistence, then use separate scripts to make the figures

# set working directory
setwd('~/Projects/iliketurtles3')

# source functions
source('code/mating function/OSRs_to_betas.R')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)


# 
# # make scenario and osr a factor variable
# base_persistence$Scenario <- factor(base_persistence$Scenario, 
#                                     levels = unique(base_persistence$Scenario))

# load in persistence object
load("~/Projects/iliketurtles3/output/base_persistence.Rdata")

# load in persistence object
load("~/Projects/iliketurtles3/output/evolution_persistence.Rdata")

# what's the plot to plot
DF <- base_persistence %>%
  filter(Stochasticity == 'temperature stochasticity') %>%
  filter(Model == 'P_base') 

# individual plot, total abundance
fig1 <- ggplot(data = DF, 
               aes(x = OSR, 
                   y = Scenario, 
                   fill = Probability_total)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                       mid = hcl.colors(5, "viridis")[3],
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 0.5,    #same midpoint for plots (mean of the range)
                       breaks = c(0, 0.25, 0.5, 0.75, 1), #breaks in the scale bar
                       limits = c(0, 1),
                       na.value = 'gray') +
  guides(fill = guide_colourbar(title = "Probability")) +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle(paste(paths[p], ': Probability of population persistence \n 
                      (> 10% of starting total abundance) to year 100',
                sep = '')) +
  theme(panel.background = element_blank())

# save to file
ggsave(plot = fig1,
       filename = paste(DF$Stochasticity[1], '_', DF$Model[1], '_',
                        '_Y', DF$Survive_to, '_total_persistence.png', 
                        sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 3.5)

# individual plot - abundance mature
fig2 <- ggplot(data = DF, 
               aes(x = OSR, 
                   y = Scenario, 
                   fill = Probability_mature)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                       mid = hcl.colors(5, "viridis")[3],
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 0.5,    #same midpoint for plots (mean of the range)
                       breaks = c(0, 0.25, 0.5, 0.75, 1), #breaks in the scale bar
                       limits = c(0, 1),
                       na.value = 'gray') +
  guides(fill = guide_colourbar(title = "Probability")) +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle(paste(paths[p], ': Probability of population persistence \n 
                (> 10% of starting mature abundance) to year 100', sep = '')) +
  theme(panel.background = element_blank())

# save to file
ggsave(plot = fig2,
       filename = paste(DF$Stochasticity[1], '_', DF$Model[1], '_',
                        '_Y', DF$Survive_to, '_mature_persistence.png', 
                        sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 3.5)
