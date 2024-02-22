# make figures representing output

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)

# source functions
source('code/mating function/beta_axis_labels.R')

# plotting model parameters
output_folder <- '2024_02_16_Godfrey_Mrosovsky_2006'
Betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
start_year <- 2024
Y <- 100
Scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
nsims <- 10000

# Betas
Beta_axis_labels <- beta_axis_labels(Betas)

# if year1 is 2022, calculate year indices for 2040, 2060, and 2100
end_year <- start_year + Y - 1
years <- start_year:end_year
years_to_plot <- c(start_year + Y - 1)

# clear DF object
rm(DF)

# pull out number of simulations where population survived to 2040, 2060, 2100
DF <- data.frame(Scenario = NULL, 
                 Beta = NULL, 
                 Survive_to = NULL, 
                 Probability = NULL)

# Scenarios
temps <- paste(Scenarios, 'C', sep = '')
Scenarios <- factor(temps, levels = temps)


for (i in 1:length(years_to_plot)) {
  
  # index for year to plot
  index <- which(years == years_to_plot[i])
  
  for (s in 1:length(Scenarios)) {
    
    for (b in 1:length(Betas)) {
      
      # load in appropriate output file
      
      # if the file exists:
      if (file.exists(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/', 
                            output_folder, '/', Scenarios[s], '/beta', Betas[b], 
                            '/', nsims, '_abundance_total.Rda', sep = ''))) {
        
        load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/', 
                   output_folder, '/', Scenarios[s], '/beta', Betas[b], '/', 
                   nsims, '_abundance_total.Rda', sep = ''))
        
        DF2 <- data.frame(Scenario = Scenarios[s],
                          Beta = Beta_axis_labels[b],
                          Survive_to = years_to_plot, 
                          Probability = c(sum(sims_abundance_total[index, ] > 
                                                0.1*sims_abundance_total[1, ]) / nsims))
        
      } else {
        
        
        DF2 <- data.frame(Scenario = Scenarios[s],
                          Beta = Beta_axis_labels[b],
                          Survive_to = years_to_plot, 
                          Probability = NA)
        
      }
      
      
      
      DF <- rbind(DF, DF2)
      
    }
    
  }
  
  DF$Beta <- factor(DF$Beta, levels = rev(Beta_axis_labels))
  
  # heatmap for survival to all three years - horizontal
  fig <- ggplot(data = DF, aes(x = Beta, y = Scenario, fill = Probability)) +
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
    xlab('Percent of males that can fertilize 50% of females') +
    ylab('Increase in sand temperature (C) by 2123') +
    ggtitle(paste('Probability of population persistence (> 10% of starting population size) to ', years_to_plot[i], 
                  sep = '')) +
    theme(panel.background = element_blank()) 
  
  
  # save to file
  ggsave(plot = fig, 
         filename = paste(years_to_plot[i], '_persistence_heatmap.png', sep = ''),
         path = '~/Projects/iliketurtles3/figures/',
         width = 8, height = 3.5)
  
}
