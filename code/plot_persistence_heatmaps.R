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
library(tidyr)

# EDIT dataframes to load up ###################################################

# load in persistence object
load("~/Projects/iliketurtles3/output/base_persistence_100.Rdata")

# load("~/Projects/iliketurtles3/output/base_persistence_25.Rdata")
# bp25 <- base_persistence
# 
# load("~/Projects/iliketurtles3/output/base_persistence_50.Rdata")
# bp50 <- base_persistence
# 
# load("~/Projects/iliketurtles3/output/base_persistence_75.Rdata")
# bp75 <- base_persistence

# load in persistence object
# load("~/Projects/iliketurtles3/output/evolution_persistence.Rdata")

all_combos <- base_persistence
# all_combos <- rbind(base_persistence, evolution_persistence)
# all_combos <- rbind(bp25, bp50, bp75)

################################################################################

# make scenario and osr a factor variable
all_combos$Scenario <- factor(all_combos$Scenario, 
                           levels = unique(all_combos$Scenario))
all_combos$Stochasticity <- factor(all_combos$Stochasticity, 
                              levels = unique(all_combos$Stochasticity))

# # shorter time scales
# short <- all_combos
  
# pivotal <- all_combos %>%
#   filter(Stochasticity == 'temperature stochasticity') %>%
#   filter(Model %in% c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
#                       'GM_base', 'GM_evol_piv', 'GM_evol_piv_high_H'))
#   
# threshold <- all_combos %>%
#   filter(Stochasticity == 'temperature stochasticity') %>%
#   filter(Model %in% c('P_base', 'P_evol_threshold', 'P_evol_threshold_high_H', 
#                       'GM_base', 'GM_evol_threshold', 'GM_evol_threshold_high_H'))

# EDIT #########################################################################
DF_to_use <- all_combos %>% 
  filter(Stochasticity == 'temperature stochasticity') %>%
  select(Population, Scenario, OSR, Survive_to, 
         Probability_mature, Probability_total_mean) %>%
  pivot_longer(cols = c('Probability_mature', 'Probability_total_mean'), 
               names_to = 'abundance', 
               values_to = 'Probability') %>%
  mutate(Abundance = paste(ifelse(abundance == 'Probability_mature', 
                                  'Mature', 'Total'), 
         'Abundance', sep = ' '))
  # mutate(pretty_survive_to = paste('Year', Survive_to, sep = ' '))
  
name_to_use <- paste('base_persistence')
# short_stochasticities <- unique(DF_to_use$Stochasticity_short)
var_rows <- 7 # abundance pretty
var_columns <- 1 # populations

##### plotting abundance mature ################################################
fig3 <- ggplot(data = DF_to_use, 
               aes(x = OSR, 
                   y = Scenario, 
                   fill = Probability)) +
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
  xlab('Minimum operational sex ratio required for 100% female reproductive success') +
  ylab(paste('Increase in temperature (\u00B0C) by year ', 
             DF_to_use$Survive_to, sep = '')) +
  ggtitle(paste(name_to_use, ': Probability of population persistence \n
          (> 10% of starting abundance) by year 100', 
                sep = '')) +
  facet_grid(
    rows = vars(Abundance),
    cols = vars(Population)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 10)) +
  theme(title = element_text(size = 13))

# save combined figure to file
ggsave(plot = fig3,
       filename = paste(name_to_use, '.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 7)

##### plotting abundance total #################################################

fig4 <- ggplot(data = DF_to_use, 
               aes(x = OSR, 
                   y = Scenario, 
                   fill = Probability_total_mean)) +
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
  ylab(paste('Increase in sand temperature (\u00B0C) by year ', 
             DF_to_use$Survive_to, sep = '')) +
  ggtitle(paste(name_to_use, ': Probability of population persistence \n
          (> 10% of starting total abundance) on shorter timescales', 
                sep = '')) +
  facet_grid(
    # rows = vars(DF_to_use[, var_rows]),
    cols = vars(DF_to_use[, var_columns])) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 10)) +
  theme(title = element_text(size = 13))

# save combined figure to file
ggsave(plot = fig4,
       filename = paste(name_to_use, '_total.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 5)
