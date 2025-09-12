# table and/or heatmap of ideal incubation temperatures

# load libraries
library(ggplot2)
library(tidyverse)
library(readr)
library(ggpattern)

##### calculate ideal temperatures (adjusted) ##################################

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

# operational proportion male
OSRs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
xFs <- round(1/OSRs - 1, 2)
betas <- as.numeric(OSRs_to_betas(OSRs))

# hatchling sex ratios * emergence success
temps <- seq(from = 25, to = 35, by = 0.01)
emergence <- 0.86 / (1 + exp(1.7 * (temps - 32.7)))

# narrow TRT proportions male (patricio et al. 2017)
t_piv1 <- 29.2
k1 <- -1.34
narrow_TRT_pM <- round(1/(1 + exp(-k1*(temps - t_piv1))), 5)

# wide TRT proportions male (godfrey & mrosovsky 2006)
t_piv2 <- 29.2
k2 <- -0.561
wide_TRT_pM <- round(1/(1 + exp(-k2*(temps - t_piv2))), 5)

# TRTs
TRTs <- c('Narrow transitional range', 'Wide transitional range')
M_remigration <- 1.47
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
  mutate(xF = round(1/OSR - 1, 2)) %>%
  mutate(nM = 100 * PSR / M_remigration) %>%
  mutate(nF = 100 * (1 - PSR) / F_remigration) %>%
  mutate(breeding_success = pbeta(q = 2*(nM / (nM + nF)), 
                                    shape1 = 1, 
                                    shape2 = Beta)) %>%
  mutate(nEggs = nF * breeding_success * 4.95 * 100.58) %>%
  mutate(nHatchlings = nEggs * emergence)

ideals <- hatchlings %>%
  group_by(TRT, Beta) %>%
  filter(nHatchlings == max(nHatchlings)) %>%
  mutate(xF = round(1/OSR - 1, 2)) 

# make values factors
ideals$OSR <- factor(ideals$OSR, levels = OSRs)
ideals$TRT <- factor(ideals$TRT, levels = TRTs)
ideals$xF <- factor(ideals$xF, levels = unique(ideals$xF))

# save as table
save(ideals, file = '~/Projects/iliketurtles3/output/ideals.Rdata')


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
  mutate(Above_init_temp = as.character(Temp > 31.8)) %>%
  mutate(temps_below = replace(Temp, Temp <= 31.8, '')) %>%
  mutate(temps_above = replace(Temp, Temp > 31.8, ''))

# actually do the heatmap thing
ideal_temps_heatmap <- ggplot(data = to_plot, 
                              aes(x = xF, 
                                  y = TRT, 
                                  fill = Temp, 
                                  pattern = Above_init_temp)) +
  geom_tile(color = 'white') +
  # scale_x_discrete(labels = xlabs) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', 
                       midpoint = 29.2) +
  labs(fill = "Incubation \n temperature \n (\u00B0C)") +
  xlab("Minimum OSR required for 99% female reproductive success (xF:1M)") +
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
  geom_text(aes(label = round(as.numeric(temps_below), 2)), 
            size = 3) +
  geom_text(aes(label = round(as.numeric(temps_above), 2)), 
            size = 3, 
            fontface = 'bold')

# guides(pattern = 'none')

ideal_temps_heatmap

ggsave(ideal_temps_heatmap,
       file = '~/Projects/iliketurtles3/figures/ideal_temps_heatmap.png',
       width = 8, height = 3)

##### ideal temps (not adjusted by emergence success) ##########################
# 
# # osrs
# osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
# betas <- as.numeric(OSRs_to_betas(osrs))
# # betas <- c(43.71, 20.64, 12.92, 9.02, 6.65, 5.03, 3.83, 2.87, 2.01, 1)
# 
# # ideal hatchling sex ratios based on mating system
# IHSR <- (1.47 * osrs) / (1.47 * osrs + 3.87 * (1 - osrs))
# # [1] 0.01960000 [2] 0.04049587 [3] 0.06282051 [4] 0.08672566 [5] 0.11238532 
# # [6] 0.14000000 [7] 0.16980198 [8] 0.20206186 [9] 0.23709677 [10] 0.26737194
# 
# # temperatures that give those IHSRs based on thermal reaction norm
# # narrow TRT population; k = -1.34, pivotal temp = 29.2
# ITemps_narrow <- (log( 1 / IHSR - 1) + 1.34 * 29.2) / 1.34
# # [1] 32.11972 [2] 31.56210 [3] 31.21686 [4] 30.95693 [5] 30.74224 
# # [6] 30.55469 [7] 30.38435 [8] 30.22497 [9] 30.07214 [10] 29.95224
# 
# # wide TRT population, k = -0.561, pivotal temp = 29.2
# ITemps_wide <- (log( 1 / IHSR - 1) + 0.561 * 29.2) / 0.561
# # [1] 36.17403 [2] 34.84210 [3] 34.01746 [4] 33.39659 [5] 32.88379 
# # [6] 32.43581 [7] 32.02893 [8] 31.64823 [9] 31.28318 [10] 30.99679
# 
# 
# # ideal values
# ideals <- data.frame(
#   Min_OSR = as.numeric(osrs), 
#   Ideal_Hatchling_Sex_Ratio = round(as.numeric(IHSR), 3), 
#   Narrow_TRT = round(as.numeric(ITemps_narrow), 3),
#   Wide_TRT = round(as.numeric(ITemps_wide), 3)
# ) 
# 
# # save as table
# write_csv(ideals, file = '../output/ideals.csv')
# save(ideals, file = '../output/ideals.Rdata')
# 
# # load ideals object
# # ideals <- read.csv('output/ideals.csv')
# load("~/Projects/iliketurtles3/output/ideals.Rdata")
# 
# # xaxis labels
# xlabs <- paste(unique(ideals$Min_OSR), '\n (', 
#                unique(ideals$Ideal_Hatchling_Sex_Ratio), ')', sep = '')
# 
# # apply to factor
# ideals$xlabs <- as.factor(xlabs)
# 
# # adjust dataframe to get other useful columns
# to_plot <- ideals %>%
#   pivot_longer(cols = c(3, 4), 
#                names_to = "Population", 
#                values_to = "Temperature") %>%
#   mutate(Above_init_temp = as.character(Temperature > 31.8)) %>%
#   mutate(temps_above = replace(Temperature, Temperature <= 31.8, '')) %>%
#   mutate(temps_below = replace(Temperature, Temperature > 31.8, ''))
# 
# # actually do the heatmap thing
# ideal_temps_heatmap <- ggplot(data = to_plot, 
#          aes(x = as.factor(Min_OSR), 
#            y = Population, 
#            fill = Temperature, 
#            pattern = Above_init_temp)) +
#   geom_tile(color = 'white') +
#   scale_x_discrete(labels = xlabs) +
#   scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', 
#                       midpoint = 29.2) +
#   labs(fill = "Incubation \n temperature \n (\u00B0C)") +
#   xlab("Minimum OSR required for 99% female reproductive success \n (Associated proportion of hatchlings produced that are male)") +
#   scale_y_discrete(labels = c("Narrow TRT \n population ", 
#                               "Wide TRT \n population "
#                               )) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) +
#   theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
#   theme(axis.title.x = element_text(size = 11, vjust = -3)) +
#   theme(axis.title.y = element_blank()) +
#   theme(axis.text = element_text(size = 9)) +
#   theme(strip.text = element_text(size = 9)) +
#   theme(legend.title = element_text(size = 9)) +
#   # annotate("rect", 
#   #          xmin = c(1.5, 1.5, 1.5, 7.5), 
#   #          xmax = c(10.5, 7.5, 1.5, 7.5), 
#   #          ymin = c(0.5, 1.5, 0.5, 1.5), 
#   #          ymax = c(0.5, 1.5, 1.5, 2.5), 
#   #          colour = "black", linewidth = 1) +
#     # geom_tile_pattern(pattern_color = NA,
#     #                   pattern_fill = "black",
#     #                   pattern_alpha = 0.5,
#     #                   pattern_angle = 45,
#     #                   pattern_density = 0.125,
#     #                   pattern_spacing = 0.05, 
#     #                   pattern_key_scale_factor = 1) +
#     # scale_pattern_manual(values = c('FALSE' = "stripe", 'TRUE' = "none")) +
#   geom_text(aes(label = round(as.numeric(temps_above), 2)), 
#             size = 3) +
#   geom_text(aes(label = round(as.numeric(temps_below), 2)), 
#             size = 3, 
#             fontface = 'bold')
# 
#   # guides(pattern = 'none')
# 
# ideal_temps_heatmap
# 
# 
# 
# ggsave(ideal_temps_heatmap, 
#        file = '~/Projects/iliketurtles3/figures/ideal_temps_heatmap.png', 
#        width = 8, height = 3)
