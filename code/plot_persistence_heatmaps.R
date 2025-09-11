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
load("~/Projects/iliketurtles3/output/base_persistence.Rdata")

# load("~/Projects/iliketurtles3/output/base_persistence_25.Rdata")
# bp25 <- base_persistence
# 
# load("~/Projects/iliketurtles3/output/base_persistence_50.Rdata")
# bp50 <- base_persistence
# 
# load("~/Projects/iliketurtles3/output/base_persistence_75.Rdata")
# bp75 <- base_persistence

# load in persistence object
# load("~/Projects/iliketurtles3/output/evolution_persistence.Rdata")

all_combos <- base_persistence
# all_combos <- rbind(base_persistence, evolution_persistence)
# all_combos <- rbind(bp25, bp50, bp75)

# what year to plot
year_to_plot <- 100
name_to_use <- paste('mature_base_persistence')

################################################################################

# make scenario and osr a factor variable
all_combos$yaxislabs <- factor(parse_number(as.character(all_combos$Scenario)))
all_combos$Scenario <- factor(all_combos$Scenario, 
                              levels = unique(all_combos$Scenario))
OSRs <- unique(all_combos$OSR)
all_combos$OSR <- factor(all_combos$OSR, levels = unique(all_combos$OSR))
all_combos$xF <- factor(round(1/OSRs - 1, 2), 
                        levels = rev(round(1/OSRs - 1, 2)))

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
DF_to_use <- all_combos %>% 
  filter(Survive_to == year_to_plot) %>%
  filter(Abundance == 'Mature') %>%
  mutate(TRT = ifelse(Population == 'West Africa', 
                      'Narrow transitional range', 
                      'Wide transitional range')) %>%
  mutate(xF = round(1/as.numeric(as.character(OSR)) - 1, 2))

DF_to_use$xF <- factor(DF_to_use$xF, 
                       levels = rev(as.factor(round(1/as.numeric(as.character(OSRs)) - 1, 2))))

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
               aes(x = xF, 
                   y = yaxislabs, 
                   fill = Probability_mean)) +
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
  guides(fill = guide_colourbar(title = "Probability")) +
  xlab('Minimum OSR required for 99% female reproductive success (xF:1M)') +
  ylab('Temperature increase \n by year 100 (\u00B0C)') +
  # ggtitle(paste(name_to_use, ': Probability of population persistence \n
  #         (> 10% of starting abundance) by year', year_to_plot, 
  #               sep = '')) +
  facet_grid(
    # rows = vars(facet_labels), 
    cols = vars(TRT)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 14, vjust = -3)) +
  theme(axis.title.y = element_text(size = 14, vjust = 4)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(strip.text = element_text(size = 10)) +
  theme(title = element_text(size = 13)) +
  theme(legend.title = element_text(size = 13))

# save combined figure to file
ggsave(plot = fig3,
       filename = paste(name_to_use, '_Y', year_to_plot, '.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       # width = 8, height = 12)
       width = 8.5, height = 4)


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
