# threshold temperatures

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)
library(scales)

# load in pivotal temps data
load("~/Projects/iliketurtles3/output/threshold_temperatures.Rdata")
load("~/Projects/iliketurtles3/output/evolution_persistence.Rdata")

# which computer am I using?
desktop <- TRUE

# join with persistence to remove pivotal temps for populations that have died out
threshold_and_persistence <- evolution_persistence %>%
  select(!Stochasticity) %>%
  right_join(threshold_temps) 

threshold_persist_total <- threshold_and_persistence %>%
  mutate(Threshold_mean = replace(Threshold_mean, Probability_total < 0.01, NA)) %>%
  mutate(Threshold_median = replace(Threshold_median, Probability_total < 0.01, NA)) %>%
  mutate(Threshold_Q25 = replace(Threshold_Q25, Probability_total < 0.01, NA)) %>%
  mutate(Threshold_Q75 = replace(Threshold_Q75, Probability_total < 0.01, NA))

change_in_mean_threshold <- threshold_persist_total %>%
  filter(Year %in% c(1, 100)) %>%
  select(Population, Model, model, Scenario, OSR, Year, Threshold_mean) %>%
  group_by(Model, Scenario, OSR) %>%
  pivot_wider(names_from = Year, values_from = Threshold_mean) %>%
  mutate(Difference = `100` - `1`)

change_in_mean_threshold$Scenario <- factor(change_in_mean_threshold$Scenario, 
                                      levels = unique(change_in_mean_threshold$Scenario))

change_in_median_threshold <- threshold_persist_total %>%
  filter(Year %in% c(1, 100)) %>%
  select(Population, Model, model, Scenario, OSR, Year, Threshold_median) %>%
  group_by(Model, Scenario, OSR) %>%
  pivot_wider(names_from = Year, values_from = Threshold_median) %>%
  mutate(Difference = `100` - `1`)

change_in_median_threshold$Scenario <- factor(change_in_median_threshold$Scenario, 
                                        levels = unique(change_in_median_threshold$Scenario))

##### change in threshold temps by year 100 plots ##############################

# heatmap for change in mean threshold temperature for hatchlings to year 100

change_in_mean_threshold$bin <- cut(change_in_mean_threshold$Difference,
                                      breaks = c(-0.02, -0.01, 0, 0.01, 0.02),
                                      right = FALSE)

fig1 <- ggplot(data = change_in_mean_threshold, aes(x = OSR,
                                              y = Scenario,
                                              fill = bin)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  # scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
  #                      mid = hcl.colors(5, "viridis")[3],
  #                      high = hcl.colors(5, "viridis")[5], #colors in the scale
  #                      # midpoint = 0.53,    #same midpoint for plots (mean of the range)
  #                      # breaks = c(0.47, 0.53, 0.59), #breaks in the scale bar
  #                      # limits = c(0.47, 0.59),
  #                      na.value = 'gray') +
  # guides(fill = guide_colourbar(title = "Change")) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle('Change in mean hatchling threshold temperature (year 100 - year 1)') +
  facet_grid(rows = vars(model),
             cols = vars(Population)) +
  theme(plot.margin = unit(c(1, 0, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 12, vjust = -3)) +
  theme(axis.title.y = element_text(size = 12, vjust = 3)) +
  theme(title = element_text(size = 13))

# save to file
ggsave(plot = fig1,
       filename = paste('change_in_mean_threshold.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 9, height = 7)

# heatmap for change in median threshold temperature for hatchlings to year 100

change_in_median_threshold$bin <- cut(change_in_median_threshold$Difference,
                         breaks = c(-0.02, -0.01, 0, 0.01, 0.02),
                         right = FALSE)

fig2 <- ggplot(data = change_in_median_threshold, aes(x = OSR,
                                                y = Scenario,
                                                fill = bin)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle('Change in median hatchling threshold temperature (year 100 - year 1)') +
  facet_grid(rows = vars(model),
             cols = vars(Population)) +
  theme(plot.margin = unit(c(1, 0, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 12, vjust = -3)) +
  theme(axis.title.y = element_text(size = 12, vjust = 3)) +
  theme(title = element_text(size = 13))

# save to file
ggsave(plot = fig2,
       filename = paste('change_in_median_threshold.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 9, height = 7)

##### pivotal temps over time plot #############################################

# plot mean and median pivotal temperatures over time for most extreme scenario
threshold_to_plot <- threshold_and_persistence %>%
  filter(Population == 'West Africa') %>%
  filter(model == 'evolution with high H') %>%
  filter(Scenario == '5C') %>%
  filter(OSR == 0.05)

# extract years to extinction for pivotal temperature plot

# load in files
load("~/Projects/iliketurtles3/output/1_threshold_plot_10000_abundance_total.Rda")
load("~/Projects/iliketurtles3/output/1_threshold_plot_10000_abundance_mature.Rda")

# over extinction threshold or no?
alive_total <- sims_abundance_total > 0.1*sims_abundance_total[1, ]

# which year does persistence < 1%?
prop_alive_total <- rowMeans(alive_total)
vline_total <- min(which(prop_alive_total < 0.01))
# year 51

# over extinction threshold or no?
alive_mature <- sims_abundance_mature > 0.1*sims_abundance_mature[1, ]

# which year does persistence < 1%?
prop_alive_mature <- rowMeans(alive_mature)
vline_mature <- min(which(prop_alive_mature < 0.01))
# year 63


# plot figure - median
fig3 <- ggplot(data = threshold_to_plot, aes(x = Year, 
                                       y = Threshold_median)) + 
  geom_vline(xintercept = vline_total, lty = 2) +
  geom_vline(xintercept = vline_mature, lty = 3) +
  geom_hline(yintercept = 35) +
  geom_ribbon(aes(ymin = Threshold_Q25,
                  ymax = Threshold_Q75), 
              fill = 'turquoise1',
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1, col = 'turquoise3') +
  xlab('Year') +
  ylab('Median hatchling threshold temperature (\u00B0C)') +
  ggtitle('median hatchling threshold temperature over time + IQR \n
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
       filename = paste('median_threshold_over_time.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 7, height = 5)
