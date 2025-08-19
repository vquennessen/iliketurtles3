# plot sample outputs, take 2

# empty environment
rm(list = ls())

# load libraries
library(ggplot2)
library(matrixStats)
library(readr)
library(patchwork)
library(tidyverse)

# source functions and load data
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')
load("~/Projects/iliketurtles3/output/example_outputs.Rdata")
load("~/Projects/iliketurtles3/output/ideals.Rdata")

##### clean up data ############################################################

# reformat ideal temps data to merge

# make scenario and OSR a factor
example_outputs$Scenario <- factor(example_outputs$Scenario, 
                                   levels = as.factor(unique(example_outputs$Scenario)))
example_outputs$OSR <- factor(example_outputs$OSR, 
                              levels = as.factor(unique(example_outputs$OSR)))
example_outputs$TRT <- factor(example_outputs$TRT, 
                              levels = as.factor(unique(example_outputs$TRT)))

# filter scenarios and OSRs to plot
examples_to_plot <- example_outputs %>%
  filter(OSR %in% c('0.1', '0.35')) %>%
  filter(Scenario %in% c('0.5C', '4.5C')) %>%
  mutate(Mating_Function = if_else(as.numeric(as.character(OSR)) < 0.26, 
                                   'Steep', 'Shallow')) %>%
  mutate(TRT = str_replace(TRT, "Narrow", "Narrow TRT"), 
         TRT = str_replace(TRT, "Wide", "Wide TRT")) %>%
  mutate(Scenario = str_replace(Scenario, "0.5C", "0.5\u00B0C"), 
         Scenario = str_replace(Scenario, "4.5C", "4.5\u00B0C")) 

# ideal incubation temperatures
ideal_0.1 <- 0.40
ideal_0.35 <- 0.170

##### plot 1: hatchling sex ratios #############################################

A <- ggplot(data = examples_to_plot, 
            aes(x = Year, 
                y = Hatchling_Sex_Ratio_Median, 
                col = Scenario, 
                lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Hatchling_Sex_Ratio_Q25,
                  ymax = Hatchling_Sex_Ratio_Q75,
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
  ylab("Median hatchling sex ratio") +
  xlab("") +
  theme_bw()

A

##### plot 2: operational sex ratios ###########################################

B <- ggplot(data = examples_to_plot, 
            aes(x = Year, 
                y = Mature_Sex_Ratio_Median, 
                col = Scenario, 
                lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Mature_Sex_Ratio_Q25,
                  ymax = Mature_Sex_Ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  facet_grid(cols = vars(TRT)) +
  geom_hline(yintercept = ideal_0.1, lty = 1) +
  geom_hline(yintercept = ideal_0.35, lty = 2) +
  geom_path(linewidth = 0.75) +
  labs(lty = 'Mating \n Function') +
  ylab("Median operational sex ratio") +
  xlab("") +
  theme_bw() +
  theme(strip.text = element_blank())

B

##### plot 3: mature abundance #################################################

C <- ggplot(data = examples_to_plot, 
            aes(x = Year, 
                y = Mature_Abundance_Median, 
                col = Scenario, 
                lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Mature_Abundance_Q25,
                  ymax = Mature_Abundance_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  facet_grid(cols = vars(TRT)) +
  geom_path(linewidth = 0.75) +
  ylab("Median mature abundance") +
  guides(col = 'none', fill = 'none', lty = 'none') +
  theme_bw() +
  theme(strip.text = element_blank())

C

final_fig <- A / B / C
final_fig

# save to file
ggsave(plot = final_fig,
       filename = paste('sample_outputs.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 7, height = 8)
