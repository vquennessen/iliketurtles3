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

# at which point should we just not even calculate lambda because too many 
# populations have not persisted?
cutoff <- 0.01

lambdas_and_persistence <- base_persistence %>%
  rename("Year" = "Survive_to", 
         "Persistence" = "Probability_mean") %>%
  select(Population, Model, Scenario, OSR, Year, Abundance, Persistence) %>%
  right_join(lambdas) %>%
  mutate(TRT = ifelse(Population == 'West Africa', 
                      'Narrow TRT', 
                      'Wide TRT')) %>%
  mutate(facet_labels = ifelse(Abundance == 'Mature', 
                               'Mature abundance', 
                               'Total abundance'))

# %>%
#   mutate(Lambda_mean = replace(Lambda_mean, Persistence < cutoff, NA)) %>%
#   mutate(Lambda_median = replace(Lambda_median, Persistence < cutoff, NA)) %>%
#   mutate(Lambda_10yr_mean = replace(Lambda_10yr_mean, Persistence < cutoff, NA)) %>%
#   mutate(Lambda_10yr_median = replace(Lambda_10yr_median, Persistence < cutoff, NA)) %>%
#   mutate(Lambda_Q25 = replace(Lambda_Q25, Persistence < cutoff, NA)) %>%
#   mutate(Lambda_Q75 = replace(Lambda_Q75, Persistence < cutoff, NA)) %>%
#   mutate(Lambda_10yr_Q25 = replace(Lambda_10yr_Q25, Persistence < cutoff, NA)) %>%
#   mutate(Lambda_10yr_Q75 = replace(Lambda_10yr_Q75, Persistence < cutoff, NA))



# make scenarios factor variable
lambdas_and_persistence$Scenario <- factor(lambdas_and_persistence$Scenario, 
                                           levels = unique(lambdas$Scenario))
lambdas_and_persistence$Abundance <- factor(lambdas_and_persistence$Abundance, 
                                            levels = unique(lambdas$Abundance))
lambdas_and_persistence$OSR <- factor(lambdas_and_persistence$OSR, 
                                      levels = rev(unique(lambdas$OSR)))
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
  filter(Abundance %in% c('Mature')) %>%
  # filter(Stochasticity == 'temperature stochasticity') %>%
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
  guides(fill = guide_legend(title = "Final \n growth \n rate", 
                             reverse = TRUE)) +
  xlab('Minimum operational sex ratio required for 99% female reproductive success') +
  ylab('Increase in temperature (\u00B0C) \n by year 100') +
  # ggtitle('final 10 yr median growth rate (year 100)') +
  facet_grid(
    # rows = vars(Abundance),
    cols = vars(TRT)) +
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
       filename = paste('final_lambda_median.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 4)
# # some mature males and females in [1, 1.01) at high temps
# lambdas_and_persistence %>%
#   filter(Scenario == '4C') %>%
#   filter(OSR == '0.1') %>%
#   filter(Population == 'West Africa') %>%
#   filter(Abundance == 'Mature') %>%
#   filter(Year > 90) %>%
#   ggplot(aes(x = Year, y = Lambda_median)) +
#   geom_point()
# 
# lambdas_and_persistence %>%
#   filter(Scenario == '4C') %>%
#   filter(OSR == '0.1') %>%
#   filter(Population == 'West Africa') %>%
#   filter(Abundance == 'Mature') %>%
#   filter(Year > 85) %>%
#   ggplot(aes(x = Year, y = Persistence)) +
#   geom_path()
# 

##### troubleshooting lambda = 1 at end for mature females and males
load("C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/i like turtles/2025_08_14/P_base/4C/beta20.63/10000_N.Rda")

mature_abundances <- colSums(sims_N[3:4, , , ], dim = 2)
mature_lambdas <- mature_abundances[2:100, ]/mature_abundances[1:99, ]
# 
# to_plot <- reshape2::melt(MF_abundances) 
# to_plot2 <- reshape2::melt(MF_lambdas) 
# 
# colnames(to_plot) <- c('Year', 'Simulation', 'Abundance')
# colnames(to_plot2) <- c('Year', 'Simulation', 'Lambda')
# 
# sims <- sample(1:10000, size = 20)
# 
# to_plot %>%
#   filter(Simulation %in% sims) %>%
#   filter(Year > 50) %>%
#   ggplot(aes(x = Year, 
#              y = Abundance, 
#              col = Simulation, 
#              group = Simulation)) +
#   geom_path() +
#   guides(color = 'none') +
#   geom_hline(yintercept = MF_abundances[1, 1]/10)
# 
# to_plot2 %>%
#   filter(Simulation %in% sims) %>%
#   filter(Year > 50) %>%
#   ggplot(aes(x = Year, 
#              y = Lambda, 
#              col = Simulation, 
#              group = Simulation)) +
#   geom_path() +
#   guides(color = 'none') +
#   geom_hline(yintercept = 1)


##### mean figure ##############################################################

years_to_plot <- 100

SDF_subset_mean <- lambdas_and_persistence %>%
  filter(Year == years_to_plot) %>%
  # filter(Stochasticity == 'temperature stochasticity') %>%
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
  ggtitle('final mean lambda (year 100)') +
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
       filename = paste('final_lambda_mean.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)
