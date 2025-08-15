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
  rename("Year" = "Survive_to", 
         "Persistence" = "Probability_mean") %>%
  select(Population, Model, Scenario, OSR, Year, Abundance, Persistence) %>%
  right_join(lambdas) %>%
  mutate(Lambda_mean = replace(Lambda_mean, Persistence < 0.01, NA)) %>%
  mutate(Lambda_median = replace(Lambda_median, Persistence < 0.01, NA)) %>%
  mutate(Lambda_10yr_mean = replace(Lambda_10yr_mean, Persistence < 0.01, NA)) %>%
  mutate(Lambda_10yr_median = replace(Lambda_10yr_median, Persistence < 0.01, NA)) %>%
  mutate(Lambda_Q25 = replace(Lambda_Q25, Persistence < 0.01, NA)) %>%
  mutate(Lambda_Q75 = replace(Lambda_Q75, Persistence < 0.01, NA)) %>%
  mutate(Lambda_10yr_Q25 = replace(Lambda_10yr_Q25, Persistence < 0.01, NA)) %>%
  mutate(Lambda_10yr_Q75 = replace(Lambda_10yr_Q75, Persistence < 0.01, NA))



# make scenarios factor variable
lambdas_and_persistence$Scenario <- factor(lambdas_and_persistence$Scenario, 
                                           levels = unique(lambdas$Scenario))

# save as object
save(lambdas_and_persistence, 
     file = '~/Projects/iliketurtles3/output/lambdas_and_persistence.Rdata')

##### plot final lambdas #######################################################



##### median figure ############################################################
load("~/Projects/iliketurtles3/output/lambdas_and_persistence.Rdata")

years_to_plot <- 100

SDF_subset_median <- lambdas_and_persistence %>%
  filter(Model %in% c('GM_base', 'P_base')) %>%
  filter(Year == years_to_plot) %>%
  filter(Stochasticity == 'temperature stochasticity') %>%
  mutate(bins = cut(Lambda_10yr_median, 
                    breaks = rev(c(0, 0.9, 0.99, 1, 1.01, 1.025, 1.05)), 
                    include.lowest = TRUE,
                    right = FALSE))

fig5_median <- ggplot(data = SDF_subset_median, aes(x = OSR, 
                                                y = Scenario, 
                                                fill = bins)) +
  geom_tile(color = "white",
            lwd = 1.25,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  guides(fill = guide_legend(title = "Median \n Lambda", 
                             reverse = TRUE)) +
  xlab('Minimum operational sex ratio required for 100% female reproductive success') +
  ylab('Increase in temperature (\u00B0C) by year 100') +
  ggtitle('temperature stochasticity; final 10 yr median lambda (year 100)') +
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
ggsave(plot = fig5_median, 
       filename = paste('TS_final_lambda_median.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)

##### mean figure ##############################################################

years_to_plot <- 100

SDF_subset_mean <- lambdas_and_persistence %>%
  filter(Year == years_to_plot) %>%
  filter(Stochasticity == 'temperature stochasticity') %>%
  mutate(bins = cut(Lambda_mean, 
                    breaks = rev(c(0, 0.9, 0.99, 1, 1.01, 1.025, 1.05)), 
                    include.lowest = TRUE,
                    right = FALSE))

fig5a_mean <- ggplot(data = SDF_subset_mean, aes(x = OSR, 
                                                 y = Scenario, 
                                                 fill = bins)) +
  geom_tile(color = "white",
            lwd = 1.25,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  guides(fill = guide_legend(title = "Mean \n Lambda", 
                             reverse = TRUE)) + 
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
