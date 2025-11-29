# pivotal temperatures

rm(list = ls())

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)
library(scales)

# load in pivotal temps data
load("~/Projects/iliketurtles3/output/evolution_trait_values.Rdata")
load("~/Projects/iliketurtles3/output/evolution_n100_all_outputs.Rdata")

# which computer am I using?
desktop <- TRUE

# cutoff value
cutoff <- 0.1

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
  mutate(OSR = factor(OSR, levels = rev(levels(OSR))))

# plot final pivotal temperatures 
evolution_traits_and_persistence %>%
  filter(Trait == 'T_piv') %>%
  filter(Year == 100) %>%
  ggplot(aes(x = OSR, y = Scenario, fill = P_mean)) +
  geom_tile(color = "white",
          lwd = 1.5,
          linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                       mid = hcl.colors(5, "viridis")[3],
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 31.5,    #same midpoint for plots (mean of the range)
                       # lim = c(29.4, 34.2),
                       na.value = 'gray') +  
  facet_grid(rows = vars(Rate), cols = vars(TRT)) +
  guides(fill = guide_colourbar(title = "Final pivotal \n temperature \n")) +
  xlab('Minimum OSR required for 99% female breeding success \n (proportion male)') +
  ylab('Temperature increase by year 100 (\u00B0C) \n') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  xlab('') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  ggtitle('final hatchling pivotal temperature phenotypes')

ggsave(filename = 'evolution_n100_final_pivotal_temp_phenotypes.png', 
       path = '~/Projects/iliketurtles3/figures/')

# plot final emergence success t0 phenotypes
evolution_traits_and_persistence %>%
  filter(Trait == 'emergence_success_t0') %>%
  filter(Year == 100) %>%
  ggplot(aes(x = OSR, y = Scenario, fill = P_mean)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                       mid = hcl.colors(5, "viridis")[3],
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 33.1,    #same midpoint for plots (mean of the range)
                       lim = c(32.7, 33.5),
                       na.value = 'gray') +  
  facet_grid(rows = vars(Rate), cols = vars(TRT)) +
  guides(fill = guide_colourbar(title = "Final emergence \n success t0 \n")) +
  xlab('Minimum OSR required for 99% female breeding success \n (proportion male)') +
  ylab('Temperature increase by year 100 (\u00B0C) \n') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  xlab('') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  ggtitle('final hatchling emergence success t0 phenotypes')

ggsave(filename = 'evolution_n100_final_ES_t0_temp_phenotypes.png', 
       path = '~/Projects/iliketurtles3/figures/')
