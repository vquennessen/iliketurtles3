# plot final lambda at year 100

# set working directory
setwd('~/Projects/iliketurtles3')

# source functions
source('code/mating function/OSRs_to_betas.R')

# load libraries
library(ggplot2)
library(ggpattern)
library(matrixStats)
library(dplyr)
library(tidyr)
library(scales)

# load persistence probabilities objects and left_join to SDF_lambdas, then 
# filter out any rows where the probability is < 1% (0.01)
load("~/Projects/iliketurtles3/output/lambdas.Rdata")
load("~/Projects/iliketurtles3/output/base_persistence.Rdata")

lambdas_and_persistence <- base_persistence %>%
  filter(Stochasticity == 'temperature stochasticity') %>%
  pivot_longer(cols = c(Probability_total, Probability_mature), 
               names_to = c('Abundance')) %>%
  rename('Probability' = 'value') %>%
  mutate(Abundance = replace(Abundance, 
                             Abundance == 'Probability_total', 
                             'total abundance')) %>%
  mutate(Abundance = replace(Abundance, 
                             Abundance == 'Probability_mature', 
                             'mature abundance')) %>%
  select(Population, Scenario, OSR, Abundance, Probability) %>%
  right_join(lambdas) %>%
  mutate(Lambda_mean = replace(Lambda_mean, Probability < 0.01, NA)) %>%
  mutate(Lambda_median = replace(Lambda_median, Probability < 0.01, NA)) %>%
  mutate(Lambda_10yr_mean = replace(Lambda_10yr_mean, Probability < 0.01, NA)) %>%
  mutate(Lambda_10yr_median = replace(Lambda_10yr_median, Probability < 0.01, NA)) %>%
  mutate(Lambda_Q25 = replace(Lambda_Q25, Probability < 0.01, NA)) %>%
  mutate(Lambda_Q75 = replace(Lambda_Q75, Probability < 0.01, NA))
  

# make scenarios factor variable
lambdas_and_persistence$Scenario <- factor(lambdas_and_persistence$Scenario, 
                                           levels = unique(lambdas$Scenario))

##### plot final lambdas #######################################################



##### median figure ############################################################

years_to_plot <- 100

subset_median <- subset(lambdas_and_persistence, Year == years_to_plot & 
                       Stochasticity == 'temperature stochasticity')

# subset_median$Scenario <- factor(subset_median$Scenario, 
#                                 levels = scenarios, 
#                                 labels = scenarios)

subset_median$bin <- cut(subset_median$Lambda_median,
                      breaks = c(0, 0.25, 0.9, 0.99, 1, 1.01, 1.1, 2),
                      right = FALSE)

fig5a_median <- ggplot(data = subset_median, aes(x = OSR, 
                                       y = Scenario, 
                                       fill = bin)) +
  geom_tile(color = "white",
            lwd = 1.25,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  guides(fill = guide_legend(title = "Lambda")) +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle('temperature stochasticity; final median lambda (year 100)') +
  facet_grid(rows = vars(Abundance), 
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

# save to file
ggsave(plot = fig5a_median, 
       filename = paste('TS_final_lambda_median.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)

##### mean figure ##############################################################

years_to_plot <- 100

SDF_subset_mean <- subset(lambdas_and_persistence, Year == years_to_plot & 
                              Stochasticity == 'temperature stochasticity')

SDF_subset_mean$bin <- cut(SDF_subset_mean$Lambda_mean,
                             breaks = c(0, 0.25, 0.9, 0.99, 1, 1.01, 1.1, 2),
                             right = FALSE)

fig5a_mean <- ggplot(data = SDF_subset_mean, aes(x = OSR, 
                                              y = Scenario, 
                                              fill = bin)) +
  geom_tile(color = "white",
            lwd = 1.25,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  guides(fill = guide_legend(title = "Lambda")) +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle('temperature stochasticity; final mean lambda (year 100)') +
  facet_grid(rows = vars(Abundance), 
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

# save to file
ggsave(plot = fig5a_mean, 
       filename = paste('TS_final_lambda_mean.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)

##### plot lambdas over time ###################################################

# subset to only look at some scenarios and OSRs
SDF_subset2 <- subset(lambdas_and_persistence,
                      Stochasticity == 'temperature stochasticity' &
                        Scenario %in% c('0.5C', '5C') &
                        OSR %in% c(0.05, 0.5))
# 
# # make OSR variable a factor
# SDF_subset2$OSR <- factor(SDF_subset2$OSR,
#                           levels = c(vector(unique(lambdas_and_persistence$Scenario))))

# # set any year where Lambda isn't a number to NA
# SDF_subset2$Lambda_mean[is.infinite(SDF_subset2$Lambda_mean)] <- NA
# SDF_subset2$Lambda_Q25[is.infinite(SDF_subset2$Lambda_mean)] <- NA
# SDF_subset2$Lambda_Q75[is.infinite(SDF_subset2$Lambda_mean)] <- NA

# plot figure - median
fig5b <- ggplot(data = SDF_subset2, aes(x = Year, 
                                        y = Lambda_median, 
                                        color = factor(OSR), 
                                        linetype = Scenario)) +
  geom_hline(yintercept = 1, lty = 1) +
  # geom_ribbon(aes(ymin = Lambda_Q25, 
  #                 ymax = Lambda_Q75, 
  #                 fill = OSR, 
  #                 alpha = 0.25), 
  #             color = NA, 
  #             show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('F8766D', '00BFC4')) +
  xlab('Year') +
  ylab('Lambda') +
  ggtitle('temperature stochasticity; (10yr) median lambdas over time') +
  facet_grid(rows = vars(Abundance), 
             cols = vars(Population)) +
  # theme_bw() +
  # theme(panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13)) +
  theme(legend.key.width = unit(2.65, "line"))

# save to file
ggsave(plot = fig5b, 
       filename = paste('TS_avg_lambdas.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)
