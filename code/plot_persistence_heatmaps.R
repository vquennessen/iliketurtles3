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

# load in persistence object
load("~/Projects/iliketurtles3/output/base_persistence.Rdata")

# make scenario and osr a factor variable
base_persistence$Scenario <- factor(base_persistence$Scenario, 
                                    levels = unique(base_persistence$Scenario))

##### plotting abundance total##################################################

# temperature increase scenarios
scenarios <- unique(base_persistence$Scenario)

# operational sex ratios to get 100% reproductive success
osrs <- unique(base_persistence$OSR)

# models - TO MODIFY
stochasticity <- c('no temperature stochasticity', 'temperature stochasticity')
short_stochasticities <- c('nTS', 'TS')
models <- c('P_base', 'GM_base')

# general automatically
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
  
  # subset DF
  DF <- base_persistence %>%
    filter(Stochasticity == stochasticity[(p + 1) %% 2 + 1]) %>%
    filter(Model == models[ceiling(p / 2)])
  
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
  
  # save to file
  ggsave(plot = fig1,
         filename = paste(short_stochasticity_names[p], '_', model_names[p], '_', 
                          '_Y100_total_persistence.png', sep = ''),
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
  
  # add figs to plot_list
  plot_list_mature[[p]] <- fig2
  
  # save to file
  ggsave(plot = fig2,
         filename = paste(short_stochasticity_names[p], '_', model_names[p], '_', 
                          'Y100_mature_persistence.png', sep = ''),
         path = '~/Projects/iliketurtles3/figures/',
         width = 8, height = 3.5)
  
}

# save individual figures as R objects
save(plot_list_total, 
     file = paste('~/Projects/iliketurtles3/figures/individual_total_figs.Rdata', 
                  sep = ''))

# save individual figures as R objects
save(plot_list_mature, 
     file = paste('~/Projects/iliketurtles3/figures/individual_mature_figs.Rdata', 
                  sep = ''))

# heatmap for survival to year 100
# base models and both stochasticities - abundance total
fig3 <- ggplot(data = base_persistence, 
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
  ggtitle('Probability of population persistence \n
          (> 10% of starting total abundance) to year 100') +
  facet_grid(rows = vars(Stochasticity),
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
       filename = 'abundance_total.png',
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 7)

##### plotting abundance mature ################################################

# heatmap for survival to year 100
# base models and both stochasticities
fig4 <- ggplot(data = base_persistence, 
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
  ggtitle('Probability of population persistence \n
          (> 10% of starting mature abundance) to year 100') +
  facet_grid(rows = vars(Stochasticity),
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
       filename = 'abundance_mature.png',
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 7)
