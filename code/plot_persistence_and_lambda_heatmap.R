# make heatmaps for mature population persistence and growth rate

rm(list = ls())

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)
library(tidyr)
library(readr)

# EDIT dataframes to load up ###################################################

# nsims
nsims <- 100

# source functions and load data
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')
load("~/Projects/iliketurtles3/output/ideals.Rdata")
load("~/Projects/iliketurtles3/output/TS_b800_10y_n10000_all_outputs.Rdata")

base <- all_outputs %>%
  select(-Model) %>%
  mutate(Trait = NA) %>%
  mutate(Rate = NA) %>%
  mutate(Model = '(A) no evolution') %>%
  mutate(TRT = case_when(TRT == 'Narrow transitional range' ~ 'narrow', 
                         TRUE ~ 'wide')) 

# EDIT #########################################################################

# load data object
load(paste('~/Projects/iliketurtles3/output/2025_11_30_evolution_n', nsims, noise, 
           '_all_outputs.Rdata', sep = ''))

# evolution_effective <- all_outputs %>%
#   filter(Rate == 'effective') %>%
#   mutate(Model = '(B) evolution_effective_h2') %>%
#   select(names(base))
# 
# evolution_high <- all_outputs %>%
#   filter(Rate == 'high') %>%
#   mutate(Model = '(C) evolution_high_h2') %>%
#   select(names(base))

# all_data <- rbind(base, evolution_effective, evolution_high)

evolution_effective <- all_outputs %>%
  filter(Rate == 'effective') %>%
  mutate(Model = case_when(Trait == 'T_piv' ~ '(B) evolution in pivotal temperature', 
                           TRUE ~ '(C) evolution in emergence success t0')) %>%
  select(names(base))

all_data <- rbind(base, evolution_effective)


# what year to plot
year_to_plot <- 100
# trait <- 'T_piv'
rate <- 'effective'
name_to_use <- paste('evolution', rate, 'narrow', sep = '_')

# dataframe of data to plot
DF_to_use <- all_data %>% 
  filter(Year == year_to_plot) %>%
  # filter(Trait %in% c(NA, trait)) %>%
  # filter(Rate == rate) %>%
  filter(TRT == 'narrow') %>%
  # mutate(facet_label = case_when(TRT == 'narrow' 
  #                                  ~ '(A) Narrow transitional range', 
  #                                TRUE ~ '(B) Wide transitional range')) %>%
  mutate(yaxislabs = factor(parse_number(as.character(Scenario)))) %>%
  mutate(Scenario = factor(Scenario, levels = unique(Scenario))) %>%
  mutate(OSR = factor(OSR, levels = unique(OSR)))


##### probability of persistence ###############################################
fig4A <- ggplot(data = DF_to_use, 
               aes(x = OSR, 
                   y = yaxislabs, 
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
  xlab('Minimum OSR required for 99% female breeding success \n (proportion male)') +
  ylab('Temperature increase by year 100 (\u00B0C) \n') +
  facet_grid(
    cols = vars(Model)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  xlab('') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

fig4A

##### final lambda #############################################################

DF_to_use2 <- DF_to_use %>% 
  mutate(Lambda_10yr_median = round(Lambda_10yr_median, 3)) %>%
  mutate(bins = cut(Lambda_10yr_median,
                    breaks = rev(c(0, 0.9, 0.99, 1, 1.01, 1.025, 1.05)), 
                    include.lowest = TRUE,
                    right = FALSE)) 

fig4B <- ggplot(data = DF_to_use2, aes(x = OSR, 
                                       y = yaxislabs, 
                                       fill = bins)) +
  geom_tile(color = "white",
            lwd = 1.25,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  guides(fill = guide_legend(title = "Final \n median \n growth \n rate", 
                             reverse = TRUE)) +
  xlab('\n Minimum OSR required for 99% female breeding success (proportion male)') +
  ylab('Temperature increase by year 100 (\u00B0C) \n') +
  facet_grid(
    cols = vars(Model)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(strip.text = element_blank())

fig4B

##### final combined figure ####################################################

final_fig <- fig4A / fig4B +
  plot_layout(axis_titles = "collect_y")

final_fig  

# save to file
ggsave(plot = final_fig,
       filename = paste(name_to_use,
                        '_final_persistence_and_lambda.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 10, height = 6)


