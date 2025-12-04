# pivotal temperatures

rm(list = ls())

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)
library(tidyr)
library(readr)

# load in pivotal temps data
load("~/Projects/iliketurtles3/output/evolution_trait_values.Rdata")
load("~/Projects/iliketurtles3/output/2025_11_30_evolution_n100_all_outputs.Rdata")

# which computer am I using?
desktop <- TRUE

# cutoff value
cutoff <- 0.1

# scenarios
scenarios <- c(0.5, 4.5)

# betas
betas <- c(20.63, 3.82)

# join with persistence to remove pivotal temps for populations that have died out
evolution_traits_and_persistence <- all_outputs %>%
  mutate(Scenario = as.numeric(as.character(Scenario))) %>%
  full_join(traits) %>%
  mutate(G_mean = replace(G_mean, Persist_mean < cutoff, NA)) %>%
  mutate(G_median = replace(G_median, Persist_mean < cutoff, NA)) %>%
  mutate(G_var = replace(G_var, Persist_mean < cutoff, NA)) %>%
  mutate(P_mean = replace(P_mean, Persist_mean < cutoff, NA)) %>%
  mutate(P_median = replace(P_median, Persist_mean < cutoff, NA)) %>%
  mutate(P_var = replace(P_var, Persist_mean < cutoff, NA)) %>%
  mutate(OSR = factor(OSR, levels = rev(levels(OSR)))) %>%
  mutate(G_mean_diff = G_mean - ifelse(Trait == 'T_piv', 29.4, 32.7)) %>%
  mutate(P_mean_diff = P_mean - ifelse(Trait == 'T_piv', 29.4, 32.7)) %>%
  mutate(P_lower = P_mean - 2*sqrt(P_var)) %>%
  mutate(P_upper = P_mean + 2*sqrt(P_var))


# plot final pivotal temperatures 
evolution_traits_and_persistence %>%
  filter(Trait == 'T_piv') %>%
  filter(Year == 100) %>%
  ggplot(aes(x = OSR, y = Scenario, fill = P_mean_diff)) +
  geom_tile(color = "white",
          lwd = 1.5,
          linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                       mid = hcl.colors(5, "viridis")[3],
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 2.75,    #same midpoint for plots (mean of the range)
                       # lim = c(2, 3.5),
                       na.value = 'gray') +  
  facet_grid(rows = vars(Rate), cols = vars(TRT)) +
  guides(fill = guide_colourbar(
    title = "Change in \n final pivotal \n temperature \n (\u00B0C) \n")) +
  xlab('\n Minimum OSR required for 99% female breeding success \n (proportion male)') +
  ylab('Temperature increase by year 100 (\u00B0C) \n') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  # xlab('') +
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_blank(), 
  #       axis.ticks.x = element_blank()) +
  ggtitle('change in hatchling pivotal temperature mean phenotypes by year 100')

ggsave(filename = 'evolution_n100_final_pivotal_temp_phenotypes.png', 
       path = '~/Projects/iliketurtles3/figures/')

# plot final emergence success t0 phenotypes
evolution_traits_and_persistence %>%
  filter(Trait == 'emergence_success_t0') %>%
  filter(Year == 100) %>%
  ggplot(aes(x = OSR, y = Scenario, fill = P_mean_diff)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                       mid = hcl.colors(5, "viridis")[3],
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 1,    #same midpoint for plots (mean of the range)
                       # lim = c(32.7, 34.3),
                       na.value = 'gray') +  
  facet_grid(rows = vars(Rate), cols = vars(TRT)) +
  guides(fill = guide_colourbar(
    title = paste('Change in \n final emergence \n success t_0', 
                  # expression(t[0]), 
                  ' \n (\u00B0C) \n', sep = ''))) +
  xlab('\n Minimum OSR required for 99% female breeding success \n (proportion male)') +
  ylab('Temperature increase by year 100 (\u00B0C) \n') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  # xlab('') +
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_blank(), 
  #       axis.ticks.x = element_blank()) +
  ggtitle('change in hatchling emergence success t0 phenotypes by year 100')

ggsave(filename = 'evolution_n100_final_ES_t0_temp_phenotypes.png', 
       path = '~/Projects/iliketurtles3/figures/')

##### plot phenotypes over time ################################################

phenotypes_over_time <- evolution_traits_and_persistence %>%
  filter(TRT == 'narrow') %>%
  filter(Rate == 'effective') %>%
  filter(Abundance == 'Hatchlings') %>%
  filter(Scenario %in% scenarios) %>%
  filter(Beta %in% betas) %>%
  select(Trait, Scenario, Mating_Function, Year, P_mean, 
         P_mean_diff, P_lower, P_upper)

  ggplot(aes(x = Year, 
             y = P_mean, 
             color = factor(Scenario), 
             lty = Mating_Function)) + 
  facet_grid(cols = vars(Trait)) +
  # geom_ribbon(aes(ymin = P_lower,
  #                 ymax = P_upper, 
  #                 col = NULL, 
  #                 fill = factor(Scenario)),
  #             alpha = 0.25,
  #             show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('Year') +
  ylab('Mean phenotype value') +
  # guides(col = 'none', fill = 'none', lty = 'none') +
  theme_bw()

##### changes in reaction norms ################################################

load("~/Projects/iliketurtles3/output/evolution_trait_values.Rdata")

emergence_success_A <- 0.86               # logistic by temp - A
emergence_success_k <- -1.7               # logistic by temp - beta
emergence_success_t0 <- 32.7              # logistic by temp - t0

T_threshold <- 35                         # lethal temperature threshold
k_piv <- -1.54  
T_piv <- 29.4                             # thermal reaction norm midpoint

temperatures = seq(from = 25, to = 35, by = 0.1)

beta_OSR <- all_outputs %>%
  select(Beta, OSR) %>%
  distinct()

DF2_to_plot <- traits %>%
  filter(Year == 100) %>%
  filter(TRT == 'narrow') %>%
  filter(Rate == 'effective') %>%
  full_join(beta_OSR) %>%
  select(Trait, P_mean, P_var, OSR, Scenario) %>%
  mutate(Temperature = list(temperatures)) %>%
  unnest(Temperature) %>%
  mutate(Trait_value = case_when(Trait == 'T_piv' 
                                 ~ 1 / (1 + exp(-k_piv * (Temperature - P_mean))), 
                                 TRUE ~ emergence_success_A / (
                                   1 + exp(-emergence_success_k * (
                                     Temperature - P_mean)))))

reference_ES <- data.frame(Temperature = temperatures) %>%
  mutate(Trait_value = emergence_success_A / (
    1 + exp(-emergence_success_k * (Temperature - emergence_success_t0)))) %>%
  mutate(Trait = 'emergence_success_t0')

reference <- data.frame(Temperature = temperatures) %>%
  mutate(Trait_value = 1 / (1 + exp(-k_piv * (Temperature - T_piv)))) %>%
  mutate(Trait = 'T_piv') %>%
  rbind(reference_ES) %>%
  mutate(Rate = c('effective'))

fig_norms <- ggplot(data = DF2_to_plot, 
                    aes(x = Temperature, 
                        y = Trait_value, 
                        col = factor(Scenario), 
                        lty = factor(OSR))) +
  geom_path(alpha = 0.75, lwd = 0.5) +
  geom_path(data = reference, inherit.aes = FALSE,
            aes(x = Temperature, y = Trait_value),
            col = 'black', lwd = 1) +
  facet_grid(cols = vars(Trait)) +
  xlab('Incubation temperature (\u00B0C)') +
  ylab('Trait value') +
  labs(col = 'Total \n temperature \n increase \n scenario (\u00B0C)', 
       lty = 'Minimum \n OSR \n needed \n for full \n female \n reproductive \n success') +
  theme(legend.box = 'horizontal') +
  theme(strip.text = element_blank())
