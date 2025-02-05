# make figures representing output

# TODO 
# make this script just for extracting all the probabilities of population
# persistence, then use separate scripts to make the figures

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)

# load in persistence object
load("~/Projects/iliketurtles3/output/base_persistence.Rdata")

# make scenario and osr a factor variable
SDF$Scenario <- factor(SDF$Scenario, levels = scenarios)

# subset
DF <- SDF %>%
  filter(Stochasticity == stochasticity[p]) %>%
  filter(Model == models_short[p])

# individual plot
fig <- ggplot(data = DF, aes(x = OSR, y = Scenario, fill = Probability)) +
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
  ylab('Increase in sand temperature (C) by year 100') +
  ggtitle(paste(stochasticity[p], ' - ', models_short[p], ':
    Probability of population persistence (> 10% of starting population size) to year ',
                year_to_plot, sep = '')) +
  theme(panel.background = element_blank())

# add figs to plot_list
plot_list[[p]] <- fig

# save to file
ggsave(plot = fig,
       filename = paste(stochasticity_short[p], '_', models_short[p], '_', 'Y',
                        year_to_plot, '_persistence_heatmap.png',
                        sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 3.5)


# heatmap for survival to year 100
# base models and both stochasticities
fig2 <- ggplot(data = SDF, aes(x = OSR, y = Scenario, fill = Probability)) +
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
  ggtitle(paste('Probability of population persistence \n
                (> 10% of starting population size) to year ',
                year_to_plot, sep = '')) +
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
ggsave(plot = fig2,
       filename = paste(combined_fig_filename, '.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 7)

# save individual figures as R objects
save(plot_list, file = paste('~/Projects/iliketurtles3/figures/',
                             individual_figs_filename, '.Rdata', sep = ''))
