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

# load in persistence object
load("~/Projects/iliketurtles3/output/evolution_persistence.Rdata")

# all_combos <- rbind(base_persistence, evolution_persistence)
all_combos <- base_persistence

################################################################################

# make scenario and osr a factor variable
all_combos$Scenario <- factor(all_combos$Scenario, 
                           levels = unique(all_combos$Scenario))

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
DF_to_use <- short
name_to_use <- paste(deparse(substitute(short)), '_Y', DF_to_use$Survive_to[1], 
                     sep = '')
short_stochasticities <- c('nTS', 'TS')
var_rows <- 1
var_columns <- 3
################################################################################

# variable levels
osrs <- unique(DF_to_use$OSR)
scenarios <- unique(DF_to_use$Scenario)
stochasticity <- unique(DF_to_use$Stochasticity)
models <- unique(DF_to_use$Model)

# generate automatically
model_names <- rep(models, each = length(stochasticity))
short_stochasticity_names <- rep(short_stochasticities, times = length(models))
paths <- as.vector(outer(stochasticity, models, paste, sep="/"))

# dimensions
P <- length(paths)
S <- length(scenarios)
OSRs <- length(osrs)

# # for each model
# for (p in 1:P) {
#   
# # EDIT which DF to plot ########################################################  
#   # # subset DF
#   # DF_to_plot <- base_persistence %>%
#   #   filter(Stochasticity == stochasticity[(p + 1) %% 2 + 1]) %>%
#   #   filter(Model == models[ceiling(p / 2)])
#   
#   # subset DF
#   DF_to_plot <- threshold %>%
#     filter(Stochasticity == stochasticity[1]) %>%
#     filter(Model == models[p])
# ################################################################################  
#   
# }

##### plotting abundance total##################################################

fig3 <- ggplot(data = DF_to_use, 
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
  ylab(paste('Increase in sand temperature (\u00B0C) by year ', 
             DF_to_use$Survive_to, sep = '')) +
  ggtitle(paste(name_to_use, ': Probability of population persistence \n
          (> 10% of starting total abundance) to year ', 
                DF_to_use$Survive_to[1], sep = '')) +
  facet_grid(rows = vars(DF_to_use[, var_rows]),
             cols = vars(DF_to_use[, var_columns])) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 10)) +
  theme(title = element_text(size = 13))

# save combined figure to file
ggsave(plot = fig3,
filename = paste(name_to_use, '_abundance_total.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 7)

##### plotting abundance mature ################################################

# heatmap for survival to year whatever
# base models and both stochasticities

fig4 <- ggplot(data = DF_to_use, 
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
  ylab(paste('Increase in sand temperature (\u00B0C) by year ', 
             DF_to_use$Survive_to, sep = '')) +
ggtitle(paste(name_to_use, ': Probability of population persistence \n
          (> 10% of starting mature abundance) to year ', 
              DF_to_use$Survive_to[1], sep = '')) +
  facet_grid(rows = vars(DF_to_use[, var_rows]),
             cols = vars(DF_to_use[, var_columns])) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 10)) +
  theme(title = element_text(size = 13))

# save combined figure to file
ggsave(plot = fig4,
       filename = paste(name_to_use, '_abundance_mature.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 7)
