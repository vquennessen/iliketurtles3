# make figures representing output

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)

# source functions
source('code/beta_axis_labels.R')

# if year1 is 2022, calculate year indices for 2040, 2060, and 2100
start_year <- 2023
end_year <- 2100
years <- start_year:end_year
years_to_plot <- c(2040, 2060, 2080, 2100)

# pull out number of simulations where population survived to 2040, 2060, 2100
DF <- data.frame(Scenario = NULL, 
                 Beta = NULL, 
                 Survive_to = NULL, 
                 Probability = NULL)

# Scenarios
Scenarios <- paste(seq(from = 0.5, to = 4, by = 0.5), 'C', sep = '')

# Betas
Betas <- c(1, 2, 3, 5, 10, 20, 50, 100)
Beta_axis_labels <- beta_axis_labels(Betas)

# data output folder
output_folder <- '2023_05_26_base'

for (i in 1:length(years_to_plot)) {
  
  # index for year to plot
  index <- which(years == years_to_plot[i])
  
  for (s in 1:length(Scenarios)) {
  
  for (b in 1:length(Betas)) {
    
    # load in appropriate output file
    load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/', 
               output_folder, '/', Scenarios[s], '/beta', Betas[b], 
               '/10000_abundance.Rda', sep = ''))
    
    DF2 <- data.frame(Scenario = Scenarios[s],
                      Beta = Beta_axis_labels[b], 
                      Survive_to = years_to_plot, 
                      Probability = c(sum(sims_abundance[index, ] > 0) / 10000))
    
    DF <- rbind(DF, DF2)
    
  }
  
}

# heatmap for survival to all three years - horizontal
fig <- ggplot(data = DF, aes(x = as.factor(Beta), y = Scenario, fill = Probability)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  # coord_fixed() +
  # facet_wrap(vars(Survive_to)) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1], 
                       mid = hcl.colors(5, "viridis")[3], 
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 0.5,    #same midpoint for plots (mean of the range)
                       breaks = c(0, 0.25, 0.5, 0.75, 1), #breaks in the scale bar
                       limits = c(0, 1)) +
  guides(fill = guide_colourbar(title = "Probability")) +
  xlab('Percent of males lost before reproductive success is 50%') +
  ylab('Climate Scenario') +
  ggtitle(paste('Probability of population persistence to ', years_to_plot[i], 
                sep = '')) +
  theme(panel.background = element_blank()) 
  

# save to file
ggsave(plot = fig, 
       filename = paste(years_to_plot[i], '_persistence_heatmap.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 3.5)

}
