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

##### clean up data ############################################################

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
  mutate(TRT = replace(TRT, TRT == "Narrow", "Narrow TRT"))
# plotting parameters


##### plot 1: hatchling sex ratios and emergence success #######################

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
  geom_path() +
  facet_grid(cols = vars(TRT)) +
  labs(lty = 'Mating \n Function')
  
A
