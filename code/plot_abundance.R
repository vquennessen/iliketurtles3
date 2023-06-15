# abundances over time

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)

# source functions
source('code/beta_axis_labels.R')

# data output folder
output_folder <- '2023_06_09_no_floors'

# Betas
Betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
Beta_axis_labels <- beta_axis_labels(Betas)

# if year1 is 2022, calculate year indices for 2040, 2060, and 2100
start_year <- 2023
end_year <- 2100
years <- start_year:end_year

# pull out number of simulations where population survived to 2040, 2060, 2100
DF <- data.frame(Scenario = NULL, 
                 Beta = NULL, 
                 Year = NULL, 
                 Metric = NULL, 
                 Value = NULL)

# Scenarios
temps <- paste(seq(from = 0.5, to = 4, by = 0.5), 'C', sep = '')
Scenarios <- factor(temps, levels = temps)

for (s in 1:length(Scenarios)) {
  
  for (b in 1:length(Betas)) {
    
    # load in appropriate output file
    load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/', 
               output_folder, '/', Scenarios[s], '/beta', Betas[b], 
               '/10000_abundance.Rda', sep = ''))
    
    load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/', 
               output_folder, '/', Scenarios[s], '/beta', Betas[b], 
               '/10000_mature_abundance.Rda', sep = ''))
    
    DF1 <- data.frame(Scenario = rep(Scenarios[s], length(years)),
                      Beta = rep(Beta_axis_labels[b], length(years)),
                      Year = years, 
                      Metric = "Abundance", 
                      Value = rowMeans(sims_abundance))
    
    DF2 <- data.frame(Scenario = rep(Scenarios[s], length(years)),
                      Beta = rep(Beta_axis_labels[b], length(years)),
                      Year = years, 
                      Metric = "Mature Abundance", 
                      Value = rowMeans(sims_mature_abundance))
    
    DF <- rbind(DF, DF1, DF2)
    
  }
  
}

# heatmap for survival to all three years - horizontal
fig <- ggplot(data = DF, aes(x = Year, y = Value, col = Beta)) +
  geom_path(lwd = 1) +
  guides(fill = guide_colourbar(title = "Probability")) +
  xlab('Percent of males lost before reproductive success is 50%') +
  ylab('Increase in sand temperature (C) by 2100') +
  ggtitle(paste('Probability of population persistence to ', years_to_plot[i], 
                sep = '')) +
  theme(panel.background = element_blank()) +
  facet_grid(Metric ~ Scenario, scales = 'free')


# save to file
ggsave(plot = fig, 
       filename = paste('abundance.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 3.5)
