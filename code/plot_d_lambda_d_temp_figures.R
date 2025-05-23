# make heatmaps to representing d lambda / d time

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)

# source functions
source('code/mating function/beta_axis_labels.R')

# plotting model parameters
models <- c('2024_02_16_Godfrey_Mrosovsky_2006', 
            '2024_03_16_GM_evo_ptiv', 
            '2024_03_17_GM_evo_ptiv_high_H')
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
nsims <- 10000
Years <- 1:100
temps <- paste(scenarios, 'C', sep = '')
Scenarios <- factor(temps, levels = temps)
Betas_raw <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
betas <- beta_axis_labels(Betas_raw)
Betas <- factor(betas, levels = betas)
years_to_plot <- c(10, 20, 30)

# dimensions
M <- length(models)
S <- length(Scenarios)
B <- length(Betas)
Y <- max(years_to_plot)

# clear DF object
rm(DF)

# pull out number of simulations where population survived to 2040, 2060, 2100
DF <- data.frame(model = rep(models, each = S*Y), 
                 Scenario = rep(Scenarios, times = M, each = Y), 
                 Year = rep(1:Y, times = M*S), 
                 Population_size = NA,
                 Population_mature = NA, 
                 Lambda = NA, 
                 Lambda_mature = NA)

# Scenarios


# for each model
for (m in 1:M) {
  
  # for each scenario
  for (s in 1:S) {
    
    # for each mating function
    for (b in 1:B) {
      
      # for each year to plot
      for (y in 1:Y) {
        
        # index for year to plot
        index <- (m-1)*S*B*Y + (s - 1)*B*Y + (b - 1)*Y + y
        print(index)
        
        # load in appropriate output file
        
        # if the file exists: desktop
        # if (file.exists(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
        #                       models[m], '/', Scenarios[s], '/beta', Betas_raw[b],
        #                       '/', nsims, '_abundance_total.Rda', sep = '')) &
        #     
        #     file.exists(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
        #                       models[m], '/', Scenarios[s], '/beta', Betas_raw[b],
        #                       '/', nsims, '_mature_abundance.Rda', sep = ''))) {
        #   
        #   # load in total abundance object
        #   load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
        #              models[m], '/', Scenarios[s], '/beta', Betas_raw[b], '/',
        #              nsims, '_abundance_total.Rda', sep = ''))
        #   
        #   # load in abundance mature object
        #   load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
        #              models[m], '/', Scenarios[s], '/beta', Betas_raw[b], '/',
        #              nsims, '_mature_abundance.Rda', sep = ''))
        
        # if the file exists: laptop
        if (file.exists(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                              models[m], '/', Scenarios[s], '/beta', Betas_raw[b],
                              '/', nsims, '_abundance_total.Rda', sep = '')) &
            
            file.exists(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                              models[m], '/', Scenarios[s], '/beta', Betas_raw[b],
                              '/', nsims, '_mature_abundance.Rda', sep = ''))) {
          
          # load in total abundance object
          load(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                     models[m], '/', Scenarios[s], '/beta', Betas_raw[b], '/',
                     nsims, '_abundance_total.Rda', sep = ''))
          
          # load in abundance mature object
          load(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                     models[m], '/', Scenarios[s], '/beta', Betas_raw[b], '/',
                     nsims, '_mature_abundance.Rda', sep = ''))
          
          # mean total population size
          DF$Population_size[index] = mean(sims_abundance_total[y, ], 
                                           na.rm = TRUE)
          
          # starting year 2, calculate lambda for whole population
          if (y > 1) {
            
            # lambda value for each year
            DF$Lambda[index] = DF$Population_size[index] / DF$Population_size[index - 1]
            
          }
          
          # mean mature population size
          DF$Population_mature[index] = mean(sims_mature_abundance[y, ], 
                                             na.rm = TRUE)
          
          # starting year 2, calculate lambda for whole population
          if (y > 1) {
            
            # lambda value for mature individuals for each year
            DF$Lambda_mature[index] = DF$Population_mature[index] / DF$Population_mature[index - 1]
            
          }
          
        } 
        
      }
      
    }
    
  }
  
}

# save object to save time  
save(DF, file = paste('GM_figure.Rda', sep = ''))

# beta as factor
DF$Beta <- factor(DF$Beta, levels = Betas)

# for each year
for (i in 1:length(years_to_plot)) {
  
  # subset for year
  to_plot1 <- subset(DF, Year == years_to_plot[i])
  
  # add new column for Scenario[s] - Scenario[s - 1]
  for (s in 2:length(Scenarios)) {
    
    subset1 <- subset(to_plot1, Scenario == Scenarios[s])
    subset2 <- subset(to_plot1, Scenario == Scenarios[s - 1])
    
    # indexes
    start <- (s - 1)*B + 1
    end <- s*B
    
    # regular lambda differences
    to_plot1$dLdTemp[start:end] <- (subset1$Lambda - subset2$Lambda)/(scenarios[s] - scenarios[s - 1])
    
    # mature lambda differences
    to_plot1$dLMdTemp[start:end] <- (subset1$Lambda_mature - subset2$Lambda_mature)/(scenarios[s] - scenarios[s - 1])
    
  }
  
  # heatmap for change in lambda for full population size
  fig1 <- ggplot(data = to_plot1, aes(x = Beta, y = Scenario, fill = dLdTemp)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) +
    scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                         mid = hcl.colors(5, "viridis")[3],
                         high = hcl.colors(5, "viridis")[5], #colors in the scale
                         midpoint = -0.01,    #same midpoint for plots (mean of the range)
                         breaks = c(-0.02, -0.01, 0), #breaks in the scale bar
                         limits = c(-0.022, 0.001),
                         na.value = 'gray') +
    guides(fill = guide_colourbar(title = "dL/dT")) +
    xlab('Breeding sex ratio that can fertilize all available females') +
    ylab('Increase in sand temperature (C) by 2123') +
    ggtitle(paste('d Lambda / d Temp in total population growth rate by year ',
                  years_to_plot[i], sep = '')) +    
    theme(panel.background = element_blank())
  
  # save to file
  ggsave(plot = fig1,
         filename = paste('Y', years_to_plot[i], '_', output_folder, 
                          '_persistence_heatmap.png', sep = ''),
         path = '~/Projects/iliketurtles3/figures/',
         width = 8, height = 3.5)
  
  # heatmap for change in lambda for mature population size
  fig_mature <- ggplot(data = to_plot1, aes(x = Beta, y = Scenario, fill = dLMdTemp)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) +
    scale_fill_gradient2(low = hcl.colors(5, "viridis")[1],
                         mid = hcl.colors(5, "viridis")[3],
                         high = hcl.colors(5, "viridis")[5], #colors in the scale
                         midpoint = -0.01,    #same midpoint for plots (mean of the range)
                         breaks = c(-0.02, -0.01, 0), #breaks in the scale bar
                         limits = c(-0.022, 0.001),
                         na.value = 'gray') +
    guides(fill = guide_colourbar(title = "dLM/dT")) +
    xlab('Breeding sex ratio that can fertilize all available females') +
    ylab('Increase in sand temperature (C) by 2123') +
    ggtitle(paste('d Lambda / d Temp in mature population growth rate by year ',
                  years_to_plot[i], sep = '')) +
    theme(panel.background = element_blank())
  
  # save to file
  ggsave(plot = fig_mature,
         filename = paste('Y', years_to_plot[i], '_mature_', output_folder,
                          '_persistence_heatmap.png', sep = ''),
         path = '~/Projects/iliketurtles3/figures/',
         width = 8, height = 3.5)
  
}
