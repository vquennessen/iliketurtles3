# make heatmaps to representing d lambda / d time

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)

# source functions
source('code/mating function/beta_axis_labels.R')

# plotting model parameters
output_folder <- '2024_03_16_GM_evo_ptiv'
Betas_raw <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
Betas <- beta_axis_labels(Betas_raw)
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
nsims <- 10000
Years <- 1:100
temps <- paste(scenarios, 'C', sep = '')
Scenarios <- factor(temps, levels = temps)

# dimensions
B <- length(Betas)
S <- length(Scenarios)
Y <- length(Years)

# Betas


# clear DF object
rm(DF)

# pull out number of simulations where population survived to 2040, 2060, 2100
DF <- data.frame(Scenario = rep(Scenarios, each = B*Y), 
                 Beta = rep(Betas, times = S, each = Y), 
                 Year = rep(1:Y, times = S*B), 
                 Population_size = NA,
                 Population_mature = NA, 
                 Lambda = NA, 
                 dLdtemp = NA, 
                 Lambda_mature = NA, 
                 dLMdtemp = NA)

# Scenarios


# for each scenario
for (s in 1:S) {
  
  # calculate dTemp
  dTemp <- scenarios[s] / 100
  
  # for each mating function
  for (b in 1:B) {
    
    # for each year to plot
    for (y in 1:Y) {
      
      # index for year to plot
      index <- (s - 1)*B*Y + (b - 1)*Y + y
      index
      
      # load in appropriate output file
      
      # if the file exists: desktop
      # if (file.exists(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
      #                       output_folder, '/', Scenarios[s], '/beta', Betas_raw[b],
      #                       '/', nsims, '_abundance_total.Rda', sep = '')) &
      #     
      #     file.exists(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
      #                       output_folder, '/', Scenarios[s], '/beta', Betas_raw[b],
      #                       '/', nsims, '_mature_abundance.Rda', sep = ''))) {
      #   
      #   # load in total abundance object
      #   load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
      #              output_folder, '/', Scenarios[s], '/beta', Betas_raw[b], '/',
      #              nsims, '_abundance_total.Rda', sep = ''))
      #   
      #   # load in abundance mature object
      #   load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
      #              output_folder, '/', Scenarios[s], '/beta', Betas_raw[b], '/',
      #              nsims, '_mature_abundance.Rda', sep = ''))
      
      # if the file exists: laptop
      if (file.exists(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                            output_folder, '/', Scenarios[s], '/beta', Betas_raw[b],
                            '/', nsims, '_abundance_total.Rda', sep = '')) &
          
          file.exists(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                            output_folder, '/', Scenarios[s], '/beta', Betas_raw[b],
                            '/', nsims, '_mature_abundance.Rda', sep = ''))) {
        
        # load in total abundance object
        load(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                   output_folder, '/', Scenarios[s], '/beta', Betas_raw[b], '/',
                   nsims, '_abundance_total.Rda', sep = ''))
        
        # load in abundance mature object
        load(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                   output_folder, '/', Scenarios[s], '/beta', Betas_raw[b], '/',
                   nsims, '_mature_abundance.Rda', sep = ''))
        
        # mean total population size
        DF$Population_size[index] = mean(sims_abundance_total[y, ], 
                                         na.rm = TRUE)
        
        # starting year 2, calculate lambda for whole population
        if (y > 1) {
          
          # lambda value for each year
          DF$Lambda[index] = DF$Population_size[index] / DF$Population_size[index - 1]
          
          # d Lambda / d time
          DF$dLdtemp[index] <- (DF$Lambda[index] - DF$Lambda[index - 1]) / (dTemp)
          
        }
        
        # mean mature population size
        DF$Population_mature[index] = mean(sims_mature_abundance[y, ], 
                                           na.rm = TRUE)
        
        # starting year 2, calculate lambda for whole population
        if (y > 1) {
          
          # lambda value for mature individuals for each year
          DF$Lambda_mature[index] = DF$Population_mature[index] / DF$Population_mature[index - 1]
          
          # d Lambda mature / d time
          DF$dLMdtemp[index] <- (DF$Lambda_mature[index] - DF$Lambda_mature[index - 1]) / (dTemp)
          
        }
        
      } else {
        
        DF$Population_size[index] = NA 
        DF$Population_mature[index] = NA 
        DF$Lambda[index] = NA
        DF$dLdtemp[index] = NA
        DF$Lambda_mature[index] = NA
        DF$dLMdtemp[index] = NA
        
      }
      
    }
    
  }
  
}

# save object to save time  
save(DF, file = '2024_03_16_GM_evo_ptiv.Rda')

# years to plot
years_to_plot <- c(10, 20, 25)

# beta as factor
DF$Beta <- factor(DF$Beta, levels = Betas)

# for each year
# for (i in 1:length(years_to_plot)) {
  
  # subset for year
  to_plot1 <- subset(DF, Year == 20)
  
  # heatmap for change in lambda for full population size
  fig1 <- ggplot(data = to_plot1, aes(x = Beta, y = Scenario, fill = dLdtemp)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) +
    scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                         mid = hcl.colors(5, "viridis")[3],
                         high = hcl.colors(5, "viridis")[5], #colors in the scale
                         midpoint = 0.29,    #same midpoint for plots (mean of the range)
                         breaks = c(-0.02, 0.29, 0.56), #breaks in the scale bar
                         limits = c(-0.02, 0.56),
                         na.value = 'gray') +
    guides(fill = guide_colourbar(title = "d Lambda / d temp")) +
    xlab('Percent of males that can fertilize 50% of females') +
    ylab('Increase in sand temperature (C) by 2123') +
    ggtitle('Change in total population growth rate by year 20') +
    theme(panel.background = element_blank())
  
  # save to file
  ggsave(plot = fig1,
         filename = paste('Y20_', output_folder, 
                          '_persistence_heatmap.png', sep = ''),
         path = '~/Projects/iliketurtles3/figures/',
         width = 8, height = 3.5)
  
  # subset for year
  to_plot2 <- subset(DF, Year == 25)
  
  # heatmap for change in lambda for full population size
  fig2 <- ggplot(data = to_plot2, aes(x = Beta, y = Scenario, fill = dLdtemp)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) +
    scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                         mid = hcl.colors(5, "viridis")[3],
                         high = hcl.colors(5, "viridis")[5], #colors in the scale
                         midpoint = 2.5,    #same midpoint for plots (mean of the range)
                         breaks = c(0, 1.25, 2.5, 3.75, 5), #breaks in the scale bar
                         limits = c(0, 5.1),
                         na.value = 'gray') +
    guides(fill = guide_colourbar(title = "d Lambda / d temp")) +
    xlab('Percent of males that can fertilize 50% of females') +
    ylab('Increase in sand temperature (C) by 2123') +
    ggtitle('Change in total population growth rate by year 25') +
    theme(panel.background = element_blank())
  
  # save to file
  ggsave(plot = fig2,
         filename = paste('Y25_', output_folder, 
                          '_persistence_heatmap.png', sep = ''),
         path = '~/Projects/iliketurtles3/figures/',
         width = 8, height = 3.5)
  
  
  
  # # heatmap for change in lambda for mature population size
  # fig_mature <- ggplot(data = to_plot, aes(x = Beta, y = Scenario, fill = dLMdtemp)) +
  #   geom_tile(color = "white",
  #             lwd = 1.5,
  #             linetype = 1) +
  #   scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
  #                        mid = hcl.colors(5, "viridis")[3],
  #                        high = hcl.colors(5, "viridis")[5], #colors in the scale
  #                        midpoint = -10,    #same midpoint for plots (mean of the range)
  #                        breaks = c(-20, -15, -10, -5, 0), #breaks in the scale bar
  #                        limits = c(-20, 0),
  #                        na.value = 'gray') +
  #   guides(fill = guide_colourbar(title = "d Lambda / d temp")) +
  #   xlab('Percent of males that can fertilize 50% of females') +
  #   ylab('Increase in sand temperature (C) by 2123') +
  #   ggtitle(paste('Change in mature population growth rate by year ', 
  #                 years_to_plot[i], sep = '')) +    
  #   theme(panel.background = element_blank())
  # 
  # # save to file
  # ggsave(plot = fig_mature,
  #        filename = paste('Y', years_to_plot[i], '_mature_', output_folder, 
  #                         '_persistence_heatmap.png', sep = ''),
  #        path = '~/Projects/iliketurtles3/figures/',
  #        width = 8, height = 3.5)
  
# }
