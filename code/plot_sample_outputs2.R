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
  
  Alabs <- data.frame(TRT = c('Narrow TRT', 'Wide TRT'), 
                      Year = c(25, 25),
                      Hatchling_Sex_Ratio_Median = c(0.22, 0.22),
                      Labs = c('A', 'B'))
  
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
  geom_hline(yintercept = ideal_0.1, lty = 1) +
  geom_hline(yintercept = ideal_0.35, lty = 2) +
  geom_path(linewidth = 0.75) +
  guides(col = 'none', fill = 'none', lty = 'none') +
  ylab("(A) Median \n hatchling sex ratio") +
  xlab("") +
  theme_bw()
# 
#   geom_text(data = Alabs, 
#             aes(x = Year, y = Hatchling_Sex_Ratio_Median, label = Labs))

A

##### plot 2: hatchling abundance ##############################################

B <- ggplot(data = examples_to_plot, 
            aes(x = Year, 
                y = Hatchling_Abundance_Median, 
                col = Scenario, 
                lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Hatchling_Abundance_Q25,
                  ymax = Hatchling_Abundance_Q75,
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
  ylab("(B) Median \n hatchling abundance") +
  xlab("") +
  theme_bw() +
  theme(strip.text = element_blank())

B

##### plot 2: operational sex ratios ###########################################

C <- ggplot(data = examples_to_plot, 
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
  geom_path(linewidth = 0.75) +
  labs(lty = 'Mating \n Function') +
  ylab("(C) Median \n operational sex ratio") +
  xlab("") +
  theme_bw() +
  theme(strip.text = element_blank())

C

##### plot 4: breeding success #################################################

D <- ggplot(data = examples_to_plot, 
            aes(x = Year, 
                y = Breeding_Success_Median, 
                col = Scenario, 
                lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Breeding_Success_Q25,
                  ymax = Breeding_Success_Q75,
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
  ylab("(D) Median \n breeding success") +
  xlab("") +
  theme_bw() +
  theme(strip.text = element_blank())

D

##### plot 5: mature abundance #################################################

E <- ggplot(data = examples_to_plot, 
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
  ylab("(E) Median \n mature abundance") +
  guides(col = 'none', fill = 'none', lty = 'none') +
  theme_bw() +
  theme(strip.text = element_blank())

E

final_fig <- A / B / C / D / E
final_fig

# save to file
ggsave(plot = final_fig,
       filename = paste('sample_outputs.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 7, height = 9)
