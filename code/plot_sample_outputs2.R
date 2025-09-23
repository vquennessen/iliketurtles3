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
load("~/Projects/iliketurtles3/output/joined_outputs.Rdata")

##### clean up data ############################################################

# scenarios
scenarios <- c('0.5\u00B0C', '4.5\u00B0C')

# osrs
osrs <- c(0.1, 0.35)
betas <- OSRs_to_betas(osrs)

examples_to_plot <- joined_outputs %>%
  filter(Scenario %in% scenarios) %>%
  filter(OSR %in% factor(osrs))

  # make scenario and OSR a factor
examples_to_plot$Scenario <- factor(examples_to_plot$Scenario, 
                                   levels = as.factor(unique(examples_to_plot$Scenario)))
examples_to_plot$OSR <- factor(examples_to_plot$OSR, 
                              levels = as.factor(unique(examples_to_plot$OSR)))
examples_to_plot$TRT <- factor(examples_to_plot$TRT, 
                              levels = as.factor(unique(examples_to_plot$TRT)))  

# ideal hatchling proportions male
ideal_0.1 <- 0.09997
ideal_0.35 <- 0.24497

ideal_0.1_xF <- round(1/ideal_0.1 - 1, 2)
ideal_0.35_xF <- round(1/ideal_0.35 - 1, 2)

ideal_0.1_xM <- round(ideal_0.1/(1 - ideal_0.1), 2)
ideal_0.35_xM <- round(ideal_0.35/(1 - ideal_0.35), 2)

##### plot 1: hatchling sex ratios #############################################

HSR <- ggplot(data = examples_to_plot, 
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
  geom_line(aes(x = Year, y = Emergence_Success, col = Scenario), 
            lty = 3, linewidth = 1) +
  # ylim(0, 100) +
  guides(col = 'none', fill = 'none', lty = 'none') +
  ylab("(A) Median \n hatchling sex ratio") +
  # ylab("(A) Median hatchling \n sex ratio (xF:1M)") +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
# 
#   geom_text(data = Alabs, 
#             aes(x = Year, y = Hatchling_Sex_Ratio_Median, label = Labs))

HSR

##### plot 2: hatchling abundance ##############################################

HA <- ggplot(data = examples_to_plot, 
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
  ylab("(D) Median \n hatchling abundance") +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  theme(strip.text = element_blank())

HA

##### plot 2: operational sex ratios ###########################################

OSR <- ggplot(data = examples_to_plot, 
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
  ylab("(B) Median \n operational sex ratio") +
  # ylab("(C) Median operational \n sex ratio (xF:1M)") +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  theme(strip.text = element_blank())

OSR

##### plot 4: breeding success #################################################

BS <- ggplot(data = examples_to_plot, 
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
  ylab("(C) Median \n breeding success") +
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  theme(strip.text = element_blank())

BS

##### plot 5: mature abundance #################################################

MA <- ggplot(data = examples_to_plot, 
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
  labs(x = NULL) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  theme(strip.text = element_blank())

MA

##### plot 6: median lambda ####################################################

lambda <- ggplot(data = examples_to_plot, 
                aes(x = Year, 
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
  ylab('(F) Median \n mature growth rate') +
  guides(col = 'none', fill = 'none', lty = 'none') +
  theme_bw() +
  theme(strip.text = element_blank())
  
lambda

##### final figure #############################################################

final_fig <- HSR / OSR / BS / HA / MA / lambda 
final_fig

# save to file
ggsave(plot = final_fig,
       filename = paste('sample_outputs.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 7, height = 10)
