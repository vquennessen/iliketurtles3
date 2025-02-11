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
  filter(Probability > 0.01)

# make scenarios factor variable
lambdas_and_persistence$Scenario <- factor(lambdas_and_persistence$Scenario, 
                                           levels = unique(lambdas$Scenario))

# save as object
save(lambdas_and_persistence, 
     file = '~/Projects/iliketurtles3/output/lambdas_and_persistence.Rdata')

##### plot final lambdas #######################################################



##### median figure ############################################################

years_to_plot <- 100

subset_median <- subset(lambdas_and_persistence, Year == years_to_plot & 
                       Stochasticity == 'temperature stochasticity')

subset_median$Scenario <- factor(subset_median$Scenario, 
                                levels = scenarios, 
                                labels = scenarios)

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

SDF_subset_mean <- subset(SDF, Year == years_to_plot & 
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

# load object
load("~/Projects/iliketurtles3/output/lambdas_and_persistence.Rdata")

# subset to only look at some scenarios and OSRs
SDF_subset2 <- subset(lambdas_and_persistence,
                      Stochasticity == 'temperature stochasticity' &
                        Scenario %in% c('1C', '2C') &
                        OSR %in% c(0.05, 0.5))

# plot figure
fig5b <- ggplot(data = SDF_subset2, aes(x = Year, 
                                        y = Lambda_10yr_median, 
                                        color = as.factor(OSR), 
                                        linetype = Scenario)) + 
  geom_hline(yintercept = 1, lty = 1) +
  geom_ribbon(aes(ymin = Lambda_Q25,
                  ymax = Lambda_Q75,
                  fill = as.factor(OSR)),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  xlab('Year') +
  ylab('Lambda') +
  ggtitle('temperature stochasticity; (10yr) median lambdas over time + IQR') +
  facet_grid(cols = vars(Population)) +
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
       filename = paste('TS_10yr_median_lambdas.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)
