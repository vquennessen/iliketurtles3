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

# extract parts from both
all_combos <- rbind(base_persistence, evolution_persistence)

# make scenario and osr a factor variable
all_combos$Scenario <- factor(all_combos$Scenario, 
                           levels = unique(all_combos$Scenario))
pivotal <- all_combos %>%
  filter(Stochasticity == 'temperature stochasticity') %>%
  filter(Model %in% c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
                      'GM_base', 'GM_evol_piv', 'GM_evol_piv_high_H'))
  
threshold <- all_combos %>%
  filter(Stochasticity == 'temperature stochasticity') %>%
  filter(Model %in% c('P_base', 'P_evol_threshold', 'P_evol_threshold_high_H', 
                      'GM_base', 'GM_evol_threshold', 'GM_evol_threshold_high_H'))


##### plotting abundance total##################################################

# # temperature increase scenarios
# scenarios <- unique(base_persistence$Scenario)
# stochasticity <- unique(base_persistence$Stochasticity)
# models <- unique(base_persistence$Model)
# 
# # operational sex ratios to get 100% reproductive success
# osrs <- unique(base_persistence$OSR)

# temperature increase scenarios
scenarios <- unique(threshold$Scenario)
stochasticity <- unique(threshold$Stochasticity)
models <- unique(threshold$Model)

# operational sex ratios to get 100% reproductive success
osrs <- unique(threshold$OSR)

# models - TO MODIFY
short_stochasticities <- c('TS')

# generate automatically
model_names <- rep(models, each = length(stochasticity))
short_stochasticity_names <- rep(short_stochasticities, times = length(models))
paths <- as.vector(outer(stochasticity, models, paste, sep="/"))

# dimensions
P <- length(paths)
S <- length(scenarios)
OSRs <- length(osrs)

# initialize plot lists
plot_list_total <- list()
plot_list_mature <- list()

# for each model
for (p in 1:P) {
  
  # # subset DF
  # DF <- base_persistence %>%
  #   filter(Stochasticity == stochasticity[(p + 1) %% 2 + 1]) %>%
  #   filter(Model == models[ceiling(p / 2)])
  
  # subset DF
  DF <- threshold %>%
    filter(Stochasticity == stochasticity[1]) %>%
    filter(Model == models[p])
  
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
  
  # add figs to plot_list
  plot_list_total[[p]] <- fig1
  
  # # save to file
  # ggsave(plot = fig1,
  #        filename = paste(short_stochasticity_names[p], '_', model_names[p], '_', 
  #                         '_Y100_total_persistence.png', sep = ''),
  #        path = '~/Projects/iliketurtles3/figures/',
  #        width = 8, height = 3.5)
  
  # save to file
  ggsave(plot = fig1,
         filename = paste(short_stochasticity_names[p], '_', model_names[p], '_', 
                          '_threshold_total_persistence.png', sep = ''),
         path = '~/Projects/iliketurtles3/figures/individual figs/',
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
  
  # add figs to plot_list
  plot_list_mature[[p]] <- fig2
  
  # save to file
  ggsave(plot = fig2,
         filename = paste(short_stochasticity_names[p], '_', model_names[p], '_', 
                          'Y100_mature_persistence.png', sep = ''),
         path = '~/Projects/iliketurtles3/figures/individual figs/',
         width = 8, height = 3.5)
  
}

# save individual figures as R objects
save(plot_list_total, 
     file = paste('~/Projects/iliketurtles3/figures/individual figs/individual_total_figs.Rdata', 
                  sep = ''))

# save individual figures as R objects
save(plot_list_mature, 
     file = paste('~/Projects/iliketurtles3/figures/individual figs/individual_mature_figs.Rdata', 
                  sep = ''))

# heatmap for survival to year 100
# base models and both stochasticities - abundance total
fig3 <- ggplot(data = threshold, 
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
  ggtitle('threshold: Probability of population persistence \n
          (> 10% of starting total abundance) to year 100') +
  facet_grid(rows = vars(model),
             cols = vars(Population)) +
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
       filename = 'threshold_abundance_total.png',
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 10)

##### plotting abundance mature ################################################

# heatmap for survival to year 100
# base models and both stochasticities
fig4 <- ggplot(data = threshold, 
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
  ggtitle('threshold: Probability of population persistence \n
          (> 10% of starting mature abundance) to year 100') +
  facet_grid(rows = vars(model),
             cols = vars(Population)) +
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
       filename = 'threshold_abundance_mature.png',
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 10)
