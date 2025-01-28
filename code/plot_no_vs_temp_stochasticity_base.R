# chapter 2 figure 4

# set working directory
setwd("~/Projects/iliketurtles3")

# load libraries
library(patchwork)
library(ggplot2)

# load figure objects

# no temp stochasticity
load("C:/Users/Vic/Documents/Projects/iliketurtles3/output/persistence_probs/no_temp_stochasticity.Rdata")
NTS <- SDF

# temp stochasticity
load("C:/Users/Vic/Documents/Projects/iliketurtles3/output/persistence_probs/temp_stochasticity.Rdata")
TS <- SDF

# add columns for stochasticity
NTS$Stochasticity <- 'no temperature stochasticity'
TS$Stochasticity <- 'temperature stochasticity'

# put them together
all_data <- rbind(NTS, TS)

# plot them!
fig4 <- ggplot(data = all_data, 
               aes(x = OSR, y = Scenario, fill = Probability)) +
  geom_tile(color = 'white', 
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
  xlab('Operational sex ratio required for 100% reproductive success') +
  ylab('Increase in average incubation temperature (\u00B0C) by year 100') +
  ggtitle('Probability of population persistence \n (> 10% of starting population size) to year 100') +
  facet_grid(rows = vars(Stochasticity), 
             cols = vars(Author)) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 10)) +
  theme(title = element_text(size = 13))


# save to file
ggsave(plot = fig4, 
       filename = paste('no_vs_temp_stochasticity.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)
