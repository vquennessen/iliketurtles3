# make figures representing output

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)
library(tidyr)

# EDIT dataframes to load up ###################################################

# load in persistence object
load("output/TS_b800_10y_n10000_all_outputs.Rdata")

# red noise?
red_noise <- FALSE
noise <- ifelse(red_noise == TRUE, '_red_noise', '')

# what years to plot
years_to_plot <- c(55, 70, 80, 90, 100)
name_to_use <- paste('mature_base_persistence', noise, sep = '')

################################################################################

# # make scenario and osr a factor variable
# all_combos$yaxislabs <- factor(parse_number(as.character(all_combos$Scenario)))
# all_combos$Scenario <- factor(all_combos$Scenario, 
#                               levels = unique(all_combos$Scenario))
# OSRs <- unique(all_combos$OSR)
# all_combos$OSR <- factor(all_combos$OSR, levels = unique(all_combos$OSR))
# all_combos$xF <- factor(round(1/OSRs - 1, 2), 
                        # levels = rev(round(1/OSRs - 1, 2)))

# # shorter time scales
# short <- all_combos

# pivotal <- all_combos %>%
#   filter(Stochasticity == 'temperature stochasticity') %>%
#   filter(Model %in% c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
#                       'GM_base', 'GM_evol_piv', 'GM_evol_piv_high_H'))
#   
# threshold <- all_combos %>%
#   filter(Stochasticity == 'temperature stochasticity') %>%
#   filter(Model %in% c('P_base', 'P_evol_threshold', 'P_evol_threshold_high_H', 
#                       'GM_base', 'GM_evol_threshold', 'GM_evol_threshold_high_H'))

# EDIT #########################################################################
DF_to_use <- all_outputs %>% 
  filter(Year %in% years_to_plot) %>%
  filter(Abundance == 'Mature') %>%
  mutate(facet_labels = factor(paste('Year', Year, sep = ' '), 
                               levels = paste('Year', unique(DF_to_use$Year, 
                                                             sep = ''))))

  # filter(Stochasticity == 'white noise') %>%
  # select(Population, Scenario, OSR, Survive_to, Abundance, Probability_mean) %>%
  

# # set order of demographics for pretty plot
# DF_to_use$Abundance <- factor(DF_to_use$Abundance, 
#                                 levels = c(
#                                   # 'Immature Females',
#                                   # 'Mature Females',
#                                   # 'Immature Males', 
#                                   # 'Mature Males',
#                                   'total', 
#                                   'mature'), 
#                                 labels = c(
#                                   # 'Immature Females',
#                                   # 'Mature Females',
#                                   # 'Immature Males', 
#                                   # 'Mature Males',
#                                   'Total', 
#                                   'Mature'))

# short_stochasticities <- unique(DF_to_use$Stochasticity_short)

##### plotting all abundances  #################################################
fig3 <- ggplot(data = DF_to_use, 
               aes(x = OSR, 
                   y = Scenario, 
                   fill = Persist_mean)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                       mid = hcl.colors(5, "viridis")[3],
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 0.5,    #same midpoint for plots (mean of the range)
                       breaks = c(0, 0.25, 0.5, 0.75, 1), #breaks in the scale bar
                       limits = c(0, 1),
                       na.value = 'gray') +
  guides(fill = guide_colourbar(title = "Persistence \n probability \n")) +
  xlab('Minimum OSR required for 99% female reproductive success \n (proportion male)') +
  ylab('Temperature increase \n by year 100 (\u00B0C)') +
  # ggtitle(paste(name_to_use, ': Probability of population persistence \n
  #         (> 10% of starting abundance) by year', year_to_plot, 
  #               sep = '')) +
  facet_grid(
    rows = vars(facet_labels),
    cols = vars(TRT)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 18, vjust = -3)) +
  theme(axis.title.y = element_text(size = 18, vjust = 4)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 10)) +
  theme(strip.text = element_text(size = 16)) +
  # theme(title = element_text(size = 13)) +
  theme(legend.title = element_text(size = 18))

# save combined figure to file
ggsave(plot = fig3,
       filename = paste(name_to_use, '.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 10, height = 12)


# ##### plotting abundance total #################################################
# 
# fig4 <- ggplot(data = DF_to_use, 
#                aes(x = OSR, 
#                    y = Scenario, 
#                    fill = Probability_total_mean)) +
#   geom_tile(color = "white",
#             lwd = 1.5,
#             linetype = 1) +
#   scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
#                        mid = hcl.colors(5, "viridis")[3],
#                        high = hcl.colors(5, "viridis")[5], #colors in the scale
#                        midpoint = 0.5,    #same midpoint for plots (mean of the range)
#                        breaks = c(0, 0.25, 0.5, 0.75, 1), #breaks in the scale bar
#                        limits = c(0, 1),
#                        na.value = 'gray') +
#   guides(fill = guide_colourbar(title = "Probability")) +
#   xlab('Operational sex ratio required to fertilize all females') +
#   ylab(paste('Increase in sand temperature (\u00B0C) by year ', 
#              DF_to_use$Survive_to, sep = '')) +
#   ggtitle(paste(name_to_use, ': Probability of population persistence \n
#           (> 10% of starting total abundance) on shorter timescales', 
#                 sep = '')) +
#   facet_grid(
#     # rows = vars(DF_to_use[, var_rows]),
#     cols = vars(DF_to_use[, var_columns])) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) +
#   theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
#   theme(axis.title.x = element_text(size = 13, vjust = -3)) +
#   theme(axis.title.y = element_text(size = 13, vjust = 4)) +
#   theme(axis.text = element_text(size = 10)) +
#   theme(strip.text = element_text(size = 10)) +
#   theme(title = element_text(size = 13))
# 
# # save combined figure to file
# ggsave(plot = fig4,
#        filename = paste(name_to_use, '_total.png', sep = ''),
#        path = '~/Projects/iliketurtles3/figures/',
#        width = 8, height = 5)
