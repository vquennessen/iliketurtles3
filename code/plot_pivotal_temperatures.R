# pivotal temperatures

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)
library(scales)

# load in pivotal temps data
load("~/Projects/iliketurtles3/output/pivotal_temperatures.Rdata")
load("~/Projects/iliketurtles3/output/evolution_persistence.Rdata")

# which computer am I using?
desktop <- TRUE

# join with persistence to remove pivotal temps for populations that have died out
pivotal_and_persistence <- evolution_persistence %>%
  select(!Stochasticity) %>%
  right_join(pivotal_temps) 

piv_persist_total <- pivotal_and_persistence %>%
  mutate(Piv_mean = replace(Piv_mean, Probability_total < 0.01, NA)) %>%
  mutate(Piv_median = replace(Piv_median, Probability_total < 0.01, NA)) %>%
  mutate(Piv_Q25 = replace(Piv_Q25, Probability_total < 0.01, NA)) %>%
  mutate(Piv_Q75 = replace(Piv_Q75, Probability_total < 0.01, NA))

change_in_mean_piv <- piv_persist_total %>%
  filter(Year %in% c(1, 100)) %>%
  select(Population, Model, model, Scenario, OSR, Year, Piv_mean) %>%
  group_by(Model, Scenario, OSR) %>%
    pivot_wider(names_from = Year, values_from = Piv_mean) %>%
  mutate(Difference = `100` - `1`)

change_in_mean_piv$Scenario <- factor(change_in_mean_piv$Scenario, 
                                      levels = unique(change_in_mean_piv$Scenario))

change_in_median_piv <- piv_persist_total %>%
  filter(Year %in% c(1, 100)) %>%
  select(Population, Model, model, Scenario, OSR, Year, Piv_median) %>%
  group_by(Model, Scenario, OSR) %>%
  pivot_wider(names_from = Year, values_from = Piv_median) %>%
  mutate(Difference = `100` - `1`)

change_in_median_piv$Scenario <- factor(change_in_median_piv$Scenario, 
                                      levels = unique(change_in_median_piv$Scenario))

##### change in pivotal temps by year 100 plots ################################
 
change_in_mean_piv$bin <- cut(change_in_mean_piv$Difference,
                                    breaks = c(-0.03, -0.01, 0, 0.01, 0.03),
                                    right = FALSE)

# heatmap for change in mean pivotal temperature for hatchlings to year 100
fig1 <- ggplot(data = change_in_mean_piv, aes(x = OSR,
                                              y = Scenario,
                                              fill = bin)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  # scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
  #                      mid = hcl.colors(5, "viridis")[3],
  #                      high = hcl.colors(5, "viridis")[5], #colors in the scale
  #                      # midpoint = 0.53,    #same midpoint for plots (mean of the range)
  #                      # breaks = c(0.47, 0.53, 0.59), #breaks in the scale bar
  #                      # limits = c(0.47, 0.59),
  #                      na.value = 'gray') +
  # guides(fill = guide_colourbar(title = "Change")) +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle('Change in mean hatchling pivotal temperature (year 100 - year 1)') +
  facet_grid(rows = vars(model),
             cols = vars(Population)) +
  theme(plot.margin = unit(c(1, 0, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 12, vjust = -3)) +
  theme(axis.title.y = element_text(size = 12, vjust = 3)) +
  theme(title = element_text(size = 13))

# save to file
ggsave(plot = fig1,
       filename = paste('change_in_mean_piv.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 7)

# heatmap for change in median pivotal temperature for hatchlings to year 100

change_in_median_piv$bin <- cut(change_in_median_piv$Difference,
                                breaks = c(-0.03, -0.01, 0, 0.01, 0.03),
                                right = FALSE)

fig2 <- ggplot(data = change_in_median_piv, aes(x = OSR,
                                              y = Scenario,
                                              fill = bin)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  # scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
  #                      mid = hcl.colors(5, "viridis")[3],
  #                      high = hcl.colors(5, "viridis")[5], #colors in the scale
  #                      # midpoint = 0.53,    #same midpoint for plots (mean of the range)
  #                      # breaks = c(0.47, 0.53, 0.59), #breaks in the scale bar
  #                      # limits = c(0.47, 0.59),
  #                      na.value = 'gray') +
  # guides(fill = guide_colourbar(title = "Change")) +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle('Change in median hatchling pivotal temperature (year 100 - year 1)') +
  facet_grid(rows = vars(model),
             cols = vars(Population)) +
  theme(plot.margin = unit(c(1, 0, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 12, vjust = -3)) +
  theme(axis.title.y = element_text(size = 12, vjust = 3)) +
  theme(title = element_text(size = 13))

# save to file
ggsave(plot = fig2,
       filename = paste('change_in_median_piv.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 7)

##### pivotal temps over time plot #############################################

population_to_plot <- c('West Africa')
model_to_plot <- c('evolution with high H')
scenario_to_plot <- c('5C')
osr_to_plot <- c(0.05, 0.5)

# plot mean and median pivotal temperatures over time for most extreme scenario
piv_to_plot <- pivotal_and_persistence %>%
  filter(Population %in% population_to_plot) %>%
  filter(model %in% model_to_plot) %>%
  filter(Scenario %in% scenario_to_plot) %>%
  filter(OSR %in% osr_to_plot)

# extract years to extinction for pivotal temperature plot

# load in files
load("~/Projects/iliketurtles3/output/pivotal plots/1_piv_plot_10000_abundance_total.Rda")
osr0.5_total <- sims_abundance_total
alive_total_0.5 <- osr0.5_total > 0.1*osr0.5_total[1, ]
prop_alive_total_0.5 <- rowMeans(alive_total_0.5)
vline_total_0.5 <- min(which(prop_alive_total_0.5 < 0.01))

# load in files
load("~/Projects/iliketurtles3/output/pivotal plots/0.05_piv_plot_10000_abundance_total.Rda")
osr0.05_total <- sims_abundance_total
alive_total_0.05 <- osr0.05_total > 0.1*osr0.05_total[1, ]
prop_alive_total_0.05 <- rowMeans(alive_total_0.05)
vline_total_0.05 <- min(which(prop_alive_total_0.05 < 0.01))

load("~/Projects/iliketurtles3/output/pivotal plots/1_piv_plot_10000_abundance_mature.Rda")
osr0.5_mature <- sims_abundance_mature
alive_mature_0.5 <- osr0.5_mature > 0.1*osr0.5_mature[1, ]
prop_alive_mature_0.5 <- rowMeans(alive_mature_0.5)
vline_mature_0.5 <- min(which(prop_alive_mature_0.5 < 0.01))

load("~/Projects/iliketurtles3/output/pivotal plots/0.05_piv_plot_10000_abundance_mature.Rda")
osr0.05_mature <- sims_abundance_mature
alive_mature_0.05 <- osr0.05_mature > 0.1*osr0.05_mature[1, ]
prop_alive_mature_0.05 <- rowMeans(alive_mature_0.05)
vline_mature_0.05 <- min(which(prop_alive_mature_0.05 < 0.01))

# TO DO
# add vlines to piv_to_plot DF to plot proper vlines in each plot

# plot figure - median
fig3 <- ggplot(data = piv_to_plot, aes(x = Year, 
                                        y = Piv_median)) + 
  # geom_vline(xintercept = vline_total, lty = 2) +
  # geom_vline(xintercept = vline_mature, lty = 3) +
  geom_hline(yintercept = 29.2) +
  geom_ribbon(aes(ymin = Piv_10yr_Q25,
                  ymax = Piv_10yr_Q75), 
                  fill = 'turquoise1',
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1, col = 'turquoise3') +
  xlab('Year') +
  ylab('Median hatchling pivotal temperature (\u00B0C)') +
  ggtitle('median hatchling pivotal temperature over time + IQR \n
          west africa population, scenario 5C, OSR 0.5, \n evolution with high H') +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13)) +
  theme(legend.key.width = unit(2.65, "line"))

# save to file
ggsave(plot = fig3,
       filename = paste('median_piv_over_time.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 7, height = 5)
