# plot sample outputs, take 2

# empty environment
rm(list = ls())

# load libraries
library(ggplot2)
library(matrixStats)
library(readr)
library(patchwork)
library(tidyverse)
library(ggh4x)

# red noise?
red_noise <- FALSE
noise <- ifelse(red_noise == TRUE, '_red_noise', '')

# nsims
nsims <- 100

# source functions and load data
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')
load(paste('~/Projects/iliketurtles3/output/evolution_n', nsims, noise, 
           '_all_outputs.Rdata', sep = ''))
load("~/Projects/iliketurtles3/output/ideals.Rdata")

# scenarios
scenarios <- c(0.5, 4.5)
# osrs
osrs <- c(0.1, 0.35)
betas <- OSRs_to_betas(osrs)

# filename
name <- 'evolution_ESt0_effective'

##### clean up data ############################################################

# add ideals to dataframe
clean_ideals <- ideals %>%
  mutate(Beta = as.character(Beta)) %>%
  rename('Ideal_PSR' = 'PSR') %>% 
  mutate(TRT = case_when(TRT == 'Narrow transitional range' 
                               ~ 'narrow', 
                               TRUE ~ 'wide')) 

examples_to_plot <- all_outputs %>%
  mutate(Beta = as.character(Beta))  %>%
  full_join(clean_ideals, relationship = 'many-to-many') %>%
  filter(Scenario %in% scenarios) %>%
  filter(OSR %in% factor(osrs)) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios)) %>%
  mutate(OSR = factor(OSR, levels = osrs)) %>%
  mutate(facet_label = case_when(TRT == 'narrow' 
                                 ~ '(A) Narrow transitional range', 
                                 TRUE ~ '(B) Wide transitional range')) %>% 
  mutate(TRT = factor(TRT)) %>%
  filter(Trait == 'emergence_success_t0') %>%
  filter(Rate == 'effective')

##### plot 1: hatchling sex ratios #############################################

HSR <- examples_to_plot %>%
  filter(Abundance == 'Hatchlings') %>%
  ggplot(aes(x = Year, 
             y = Sex_ratio_median)) +
  geom_ribbon(aes(ymin = Sex_ratio_Q25,
                  ymax = Sex_ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  scale_color_manual(values = c('#00BFC4', '#F8766D'),
                     name = 'Emergence \n success') +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  geom_line(aes(x = Year,
                y = Ideal_PSR,
                lty = Mating_Function),
            col = 'black',
            show.legend = FALSE) +
  geom_path(aes(col = Scenario, 
                lty = Mating_Function), 
            linewidth = 0.75, 
            show.legend = FALSE) +
  scale_linetype_manual(values = c(1, 2)) +
  geom_point(data = examples_to_plot %>% filter(Year %% 3 == 0), 
             aes(x = Year, 
                 y = Emergence_Success, 
                 col = Scenario, 
                 fill = Scenario), 
             shape = 3,
             size = 1) +
  facet_grid(cols = vars(TRT)) +
  ylab("(A) Median \n hatchling sex ratio") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

HSR

##### plot 2: hatchling abundance ##############################################

HA <- examples_to_plot %>%
  filter(Abundance == 'Hatchlings') %>%
  ggplot(aes(x = Year, 
             y = Abundance_median, 
             col = Scenario, 
             lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Abundance_Q25,
                  ymax = Abundance_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  facet_grid(cols = vars(TRT)) +
  # geom_hline(yintercept = ideal_0.1, lty = 2) +
  # geom_hline(yintercept = ideal_0.35, lty = 1) +
  geom_path(linewidth = 0.75) +
  guides(col = 'none', fill = 'none', lty = 'none') +
  # guides(y = guide_axis_truncated(trunc_lower = c(-Inf, 750000),
  #                                  trunc_upper = c(250000, Inf))) +
  ylab("(D) Median \n hatchling abundance") +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  theme(strip.text = element_blank())

HA

##### plot 2: operational sex ratios ###########################################

OSR <- examples_to_plot %>%
  filter(Abundance == 'Mature') %>%
  ggplot(aes(x = Year, 
             y = Sex_ratio_median,
             col = Scenario, 
             lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Sex_ratio_Q25,
                  ymax = Sex_ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  facet_grid(cols = vars(TRT)) +
  geom_path(linewidth = 0.75) +
  labs(lty = 'Mating \n function', col = 'Temperature \n increase') +
  # ylim(0, 10) +
  geom_hline(yintercept = 0.1, lty = 2) +
  geom_hline(yintercept = 0.35, lty = 1) +
  ylab("(B) Median \n operational sex ratio") +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  theme(strip.text = element_blank())

OSR

##### plot 4: breeding success #################################################

BS <- examples_to_plot %>%
  filter(Abundance == 'Mature') %>%
  ggplot(aes(x = Year, 
             y = Breeding_success_median, 
             col = Scenario, 
             lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Breeding_success_Q25,
                  ymax = Breeding_success_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  facet_grid(cols = vars(TRT)) +
  # geom_hline(yintercept = ideal_0.1, lty = 2) +
  # geom_hline(yintercept = ideal_0.35, lty = 1) +
  geom_path(linewidth = 0.75) +
  guides(col = 'none', fill = 'none', lty = 'none') +
  ylab("(C) Median \n breeding success") +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  theme(strip.text = element_blank())

BS

##### plot 5: mature abundance #################################################

MA <- examples_to_plot %>%
  filter(Abundance == 'Mature') %>%
  ggplot(aes(x = Year, 
             y = Abundance_median, 
             col = Scenario, 
             lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Abundance_Q25,
                  ymax = Abundance_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  facet_grid(cols = vars(TRT)) +
  geom_path(linewidth = 0.75) +
  ylab("(E) Median \n mature abundance") +
  guides(col = 'none', fill = 'none', lty = 'none') +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  theme(strip.text = element_blank())

MA

##### plot 6: median lambda ####################################################

lambda <- examples_to_plot %>%
  filter(Abundance == 'Mature') %>%
  ggplot(aes(x = Year, 
             y = Lambda_10yr_median, 
             color = Scenario, 
             linetype = Mating_Function)) + 
  # facet_grid(cols = vars(TRT), rows = vars(facet_labels)) +
  facet_grid(cols = vars(TRT)) +
  geom_hline(yintercept = 1) +
  geom_ribbon(aes(ymin = Lambda_10yr_Q25,
                  ymax = Lambda_10yr_Q75, 
                  col = NULL, 
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('Year') +
  ylab('(E) Median \n mature growth rate') +
  guides(col = 'none', fill = 'none', lty = 'none') +
  theme_bw() +
  theme(strip.text = element_blank())

lambda

##### final figure #############################################################

final_fig <- HSR / OSR / BS / HA / lambda 
final_fig

# red noise?
noise <- ifelse(red_noise == TRUE, '_red_noise', '')

# save to file
ggsave(plot = final_fig,
       filename = paste(name, nsims, noise, '_sample_outputs.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 7, height = 10)
