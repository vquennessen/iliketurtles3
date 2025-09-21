# make heatmaps for mature population persistence and growth rate

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)
library(tidyr)

# EDIT dataframes to load up ###################################################

# load in persistence and lambda object
load("~/Projects/iliketurtles3/output/joined_outputs.Rdata")

# what year to plot
year_to_plot <- 100
name_to_use <- paste('mature_base_persistence')

################################################################################

# make scenario and osr a factor variable
joined_outputs$yaxislabs <- factor(parse_number(as.character(joined_outputs$Scenario)))
joined_outputs$Scenario <- factor(joined_outputs$Scenario, 
                              levels = unique(joined_outputs$Scenario))
OSRs <- unique(joined_outputs$OSR)
joined_outputs$OSR <- factor(joined_outputs$OSR, levels = unique(joined_outputs$OSR))
# joined_outputs$xF <- factor(round(1/OSRs - 1, 2), 
#                         levels = rev(round(1/OSRs - 1, 2)))

# EDIT #########################################################################
DF_to_use <- joined_outputs %>% 
  filter(Year == year_to_plot)

# DF_to_use$xF <- factor(DF_to_use$xF, 
#                        levels = rev(as.factor(round(1/as.numeric(as.character(OSRs)) - 1, 2))))

##### probability of persistence ###############################################
fig4A <- ggplot(data = DF_to_use, 
               aes(x = OSR, 
                   y = yaxislabs, 
                   fill = Persistence)) +
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
  guides(fill = guide_colourbar(title = "Probability \n of population \n persistence")) +
  xlab('Minimum OSR required for 99% female reproductive success (xF:1M)') +
  ylab('Temperature increase \n by year 100 (\u00B0C)') +
  facet_grid(
    cols = vars(TRT)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  xlab('') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

fig4A

##### final lambda #############################################################

DF_to_use2 <- joined_outputs %>% 
  filter(Year == year_to_plot) %>%
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
  xlab('Minimum OSR required for 99% female reproductive success') +
  ylab('Temperature increase \n by year 100 (\u00B0C)') +
  facet_grid(
    cols = vars(TRT)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(strip.text = element_blank()) 

fig4B

##### final combined figure ####################################################

final_fig <- fig4A / fig4B

final_fig

# save to file
ggsave(plot = final_fig,
       filename = paste('final_persistence_and_lambda.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 6)


