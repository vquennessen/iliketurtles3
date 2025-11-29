# table and/or heatmap of ideal incubation temperatures

# load libraries
library(ggplot2)
library(tidyverse)
library(readr)
library(ggpattern)
library(patchwork)

##### calculate ideal temperatures (adjusted) ##################################

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

# operational proportion male
OSRs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
betas <- as.numeric(OSRs_to_betas(OSRs))

# hatchling sex ratios * emergence success
temps <- seq(from = 25, to = 35, by = 0.01)
emergence <- 0.86 / (1 + exp(1.7 * (temps - 32.7)))

# narrow TRT proportions male (patricio et al. 2017)
t_piv1 <- 29.4
k1 <- -1.54
narrow_TRT_pM <- round(1/(1 + exp(-k1*(temps - t_piv1))), 5)

# TRTs
M_remigration <- 1.82
F_remigration <- 3.87

hatchlings <- data.frame(TRT = rep(TRTs, each = length(betas) * length(temps)), 
                         OSR = rep(OSRs, 
                                   each = length(temps), times = length(TRTs)),
                         Beta = rep(rep(betas, each = length(temps)), 
                                    times = length(TRTs)), 
                         Temp = rep(temps, times = length(TRTs) * length(betas)), 
                         Emergence = rep(emergence, 
                                         times = length(TRTs) * length(betas)), 
                         PSR = c(rep(narrow_TRT_pM, times = length(betas)), 
                                 rep(wide_TRT_pM, times = length(betas)))) %>%
  # mutate(xF = round(1/OSR - 1, 2)) %>%
  mutate(nM = 100 * PSR / M_remigration) %>%
  mutate(nF = 100 * (1 - PSR) / F_remigration) %>%
  mutate(breeding_success = pbeta(q = 2*(nM / (nM + nF)), 
                                  shape1 = 1, 
                                  shape2 = Beta)) %>%
  mutate(nEggs = nF * breeding_success * 4.95 * 100.58) %>%
  mutate(nHatchlings = nEggs * emergence)

ideals <- hatchlings %>%
  group_by(TRT, Beta) %>%
  filter(nHatchlings == max(nHatchlings))
# mutate(xF = round(1/OSR - 1, 2)) 

# make values factors
ideals$OSR <- factor(ideals$OSR, levels = OSRs)
ideals$TRT <- factor(ideals$TRT, levels = TRTs)
# ideals$xF <- factor(ideals$xF, levels = unique(ideals$xF))

# save as table
save(ideals, 
     file = '~/Projects/iliketurtles3/output/ideals.Rdata')

ideals_without_emergence <- hatchlings %>%
  group_by(TRT, Beta) %>%
  filter(nEggs == max(nEggs))

# make values factors
ideals_without_emergence$OSR <- factor(ideals_without_emergence$OSR, levels = OSRs)
ideals_without_emergence$TRT <- factor(ideals_without_emergence$TRT, levels = TRTs)

# save as table
save(ideals_without_emergence, 
     file = '~/Projects/iliketurtles3/output/ideals_without_emergence.Rdata')

##### make the figure ##########################################################

# load ideals object
# ideals <- read.csv('output/ideals.csv')
load("~/Projects/iliketurtles3/output/ideals.Rdata")

# # xaxis labels
# xlabs <- paste(rep(unique(ideals_adjusted$xF), times = 2), 
#                '\n (', 
#                unique(ideals_adjusted$iPSR), 
#                ')', 
#                sep = '')

# apply to factor
# ideals_adjusted$xlabs <- xlabs

# adjust dataframe to get other useful columns
to_plot <- ideals %>%
  # mutate(Above_init_temp = as.character(Temp > 31.8)) %>%
  # mutate(temps_below = replace(Temp, Temp <= 31.8, '')) %>%
  # mutate(temps_above = replace(Temp, Temp > 31.8, '')) %>%
  mutate(labs = paste(round(Temp, 1), '\n (', round(PSR, 2), ')', sep = ''))

# actually do the heatmap thing
ideal_temps_heatmap <- ggplot(data = to_plot, 
                              aes(x = OSR, 
                                  y = TRT, 
                                  fill = Temp)) +
  geom_tile(color = 'white') +
  labs(fill = "Incubation \n temperature \n (\u00B0C)") +
  xlab("Minimum OSR required for 99% female reproductive success \n (associated hatchling sex ratio)") +
  scale_y_discrete(labels = c("Narrow \n transitional \n range", 
                              "Wide \n transitional \n range"
  )) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 11, vjust = -3)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  theme(strip.text = element_text(size = 9)) +
  theme(legend.title = element_text(size = 9)) +
  geom_text(aes(label = labs), 
            size = 3, 
            fontface = 'bold')
# geom_text(aes(label = round(as.numeric(temps_above), 2)), 
#           size = 3)

# guides(pattern = 'none')

ideal_temps_heatmap

# ggsave(ideal_temps_heatmap,
#        file = '~/Projects/iliketurtles3/figures/ideal_temps_heatmap.png',
#        width = 8, height = 3)

##### ideal temps (not adjusted by emergence success) ##########################

# load in object
load("~/Projects/iliketurtles3/output/ideals_without_emergence.Rdata")

# adjust dataframe to get other useful columns
to_plot2 <- ideals_without_emergence %>%
  mutate(labs = paste(round(Temp, 1), '\n (', round(PSR, 2), ')', sep = ''))

# actually do the heatmap thing
ideal_temps_without_emergence_heatmap <- ggplot(data = to_plot2, 
                                                aes(x = OSR, 
                                                    y = TRT, 
                                                    fill = Temp)) +
  geom_tile(color = 'white') +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', 
                       midpoint = 29.4) +
  labs(fill = "Incubation \n temperature \n (\u00B0C)") +
  xlab("Minimum OSR required for 99% female breeding success \n (associated hatchling sex ratio)") +
  scale_y_discrete(labels = c("Narrow \n transitional \n range", 
                              "Wide \n transitional \n range"
  )) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 11, vjust = -3)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  theme(strip.text = element_text(size = 9)) +
  theme(legend.title = element_text(size = 9)) +
  geom_text(aes(label = labs), 
            size = 3, 
            fontface = 'bold')

ideal_temps_without_emergence_heatmap

# ggsave(ideal_temps_without_emergence_heatmap,
#        file = '~/Projects/iliketurtles3/figures/ideal_temps_without_emergence_heatmap.png',
#        width = 8, height = 3)
