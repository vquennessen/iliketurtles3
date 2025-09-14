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

# source functions and load data
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')
load("~/Projects/iliketurtles3/output/example_outputs.Rdata")
load("~/Projects/iliketurtles3/output/ideals.Rdata")

##### clean up data ############################################################

# filter scenarios and OSRs to plot
examples_to_plot <- example_outputs %>%
  filter(OSR %in% c(0.1, 0.35)) %>%
  filter(Scenario %in% c('0.5C', '4.5C')) %>%
  mutate(Mating_Function = if_else(OSR < 0.26, 
                                   'Steep', 'Shallow')) %>%
  mutate(TRT = str_replace(TRT, "Narrow", "Narrow transitional range"), 
         TRT = str_replace(TRT, "Wide", "Wide transitional range")) %>%
  mutate(Scenario = str_replace(Scenario, "0.5C", "0.5\u00B0C"), 
         Scenario = str_replace(Scenario, "4.5C", "4.5\u00B0C")) %>%
  mutate(Hatchling_xF_Median = round(1/Hatchling_Sex_Ratio_Median - 1, 2)) %>%
  mutate(Hatchling_xF_Q25 = round(1/Hatchling_Sex_Ratio_Q25 - 1, 2)) %>%
  mutate(Hatchling_xF_Q75 = round(1/Hatchling_Sex_Ratio_Q75 - 1, 2)) %>%
  mutate(Mature_xF_Median = round(1/Mature_Sex_Ratio_Median - 1, 2)) %>%
  mutate(Mature_xF_Q25 = round(1/Mature_Sex_Ratio_Q25 - 1, 2)) %>%
  mutate(Mature_xF_Q75 = round(1/Mature_Sex_Ratio_Q75 - 1, 2)) %>%
  mutate(Hatchling_xM_Median = Hatchling_Sex_Ratio_Median/(1 - Hatchling_Sex_Ratio_Median)) %>%
  mutate(Hatchling_xM_Q25 = Hatchling_Sex_Ratio_Q25/(1 - Hatchling_Sex_Ratio_Q25)) %>%
  mutate(Hatchling_xM_Q75 = Hatchling_Sex_Ratio_Q75/(1 - Hatchling_Sex_Ratio_Q75)) %>%
  mutate(Mature_xM_Median = Mature_Sex_Ratio_Median/(1 - Mature_Sex_Ratio_Median)) %>%
  mutate(Mature_xM_Q25 = Mature_Sex_Ratio_Q25/(1 - Mature_Sex_Ratio_Q25)) %>%
  mutate(Mature_xM_Q75 = Mature_Sex_Ratio_Q75/(1 - Mature_Sex_Ratio_Q75)) %>%
  mutate(Hatchling_xF_Median = replace(Hatchling_xF_Median, 
                                       Hatchling_Sex_Ratio_Median < 0.01, 
                                       NA)) %>%
  mutate(Hatchling_xF_Q25 = replace(Hatchling_xF_Q25, 
                                       Hatchling_Sex_Ratio_Median < 0.01, 
                                       NA)) %>%
  mutate(Hatchling_xF_Q75 = replace(Hatchling_xF_Q75, 
                                    Hatchling_Sex_Ratio_Median < 0.01, 
                                    NA)) %>%
  mutate(Mature_xF_Median = replace(Mature_xF_Median, 
                                    Mature_Sex_Ratio_Median < 0.01, 
                                       NA)) %>%
  mutate(Mature_xF_Q25 = replace(Mature_xF_Q25, 
                                    Mature_Sex_Ratio_Median < 0.01, 
                                    NA)) %>%
  mutate(Mature_xF_Q75 = replace(Mature_xF_Q75, 
                                    Mature_Sex_Ratio_Median < 0.01, 
                                    NA)) %>%
  mutate(Emergence = 0.86 / (1 + exp(1.7 * (Temperature - 32.7))))

  # Alabs <- data.frame(TRT = c('Narrow TRT', 'Wide TRT'), 
  #                     Year = c(25, 25),
  #                     Hatchling_Sex_Ratio_Median = c(0.22, 0.22),
  #                     Labs = c('A', 'B'))

  # make scenario and OSR a factor
example_outputs$Scenario <- factor(example_outputs$Scenario, 
                                   levels = as.factor(unique(example_outputs$Scenario)))
example_outputs$OSR <- factor(example_outputs$OSR, 
                              levels = as.factor(unique(example_outputs$OSR)))
example_outputs$TRT <- factor(example_outputs$TRT, 
                              levels = as.factor(unique(example_outputs$TRT)))  

# ideal hatchling proportions male
ideal_0.1 <- 0.09997
ideal_0.35 <- 0.24497

ideal_0.1_xF <- round(1/ideal_0.1 - 1, 2)
ideal_0.35_xF <- round(1/ideal_0.35 - 1, 2)

ideal_0.1_xM <- round(ideal_0.1/(1 - ideal_0.1), 2)
ideal_0.35_xM <- round(ideal_0.35/(1 - ideal_0.35), 2)

##### plot 1: hatchling sex ratios #############################################

A <- ggplot(data = examples_to_plot, 
            aes(x = Year, 
                y = Hatchling_Sex_Ratio_Median,
                # y = Hatchling_xF_Median, 
                col = Scenario, 
                lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Hatchling_Sex_Ratio_Q25,
                  ymax = Hatchling_Sex_Ratio_Q75,
  # geom_ribbon(aes(ymin = Hatchling_xF_Q25,
  #                 ymax = Hatchling_xF_Q75,
                  col = Scenario,
                  fill = Scenario, 
                  lty = Mating_Function),
              alpha = 0.25,
              show.legend = FALSE) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  facet_grid(cols = vars(TRT)) +
  geom_hline(yintercept = ideal_0.1, lty = 2) +
  geom_hline(yintercept = ideal_0.35, lty = 1) +
  # geom_hline(yintercept = ideal_0.1_xF, lty = 2) +
  # geom_hline(yintercept = ideal_0.35_xF, lty = 1) +
  geom_path(linewidth = 0.75) +
  geom_line(aes(x = Year, y = Emergence, col = Scenario), 
            lty = 3, linewidth = 1) +
  # ylim(0, 100) +
  guides(col = 'none', fill = 'none', lty = 'none') +
  ylab("(A) Median hatchling \n proportion male") +
  # ylab("(A) Median hatchling \n sex ratio (xF:1M)") +
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
  # guides(y = guide_axis_truncated(trunc_lower = c(-Inf, 750000),
  #                                  trunc_upper = c(250000, Inf))) +
  ylab("(B) Median \n hatchling abundance") +
  xlab("") +
  theme_bw() +
  theme(strip.text = element_blank())

B

##### plot 2: operational sex ratios ###########################################

C <- ggplot(data = examples_to_plot, 
            aes(x = Year, 
                y = Mature_Sex_Ratio_Median,
                # y = Mature_xF_Median, 
                col = Scenario, 
                lty = Mating_Function)) +
  geom_ribbon(aes(ymin = Mature_Sex_Ratio_Q25,
                  ymax = Mature_Sex_Ratio_Q75,
  # geom_ribbon(aes(ymin = Mature_xF_Q25,
  #                 ymax = Mature_xF_Q75,
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
  ylab("(C) Median operational \n proportion male") +
  # ylab("(C) Median operational \n sex ratio (xF:1M)") +
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
