# make figures representing output

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)

# source functions
source('code/mating function/beta_axis_labels.R')

# which computer am I using?
desktop <- FALSE

# plotting model parameters
nsims <- 10000
models <- c('2024_02_28_Patricio_2017',
            '2024_03_15_P_evo_ptiv', 
            '2024_03_18_P_evo_ptiv_high_H', 
            '2024_02_16_Godfrey_Mrosovsky_2006',
            '2024_03_16_GM_evo_ptiv',
            '2024_03_17_GM_evo_ptiv_high_H')
models_short <- c('base_P', 'P_evo', 'P_high_H', 
                  'base_GM', 'GM_evo', 'GM_high_H')
authors <- c(rep('Patricio et al. 2017', 3),
             rep('Godfrey and Mrosovsky 2006', 3))
model_types <- rep(c('base model', 'evolution', 'evolution with high H'), 
                   times = 2)

years_to_plot <- c(100)
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')
Scenarios <- factor(scenarios, levels = scenarios)
Betas_raw <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
betas <- beta_axis_labels(Betas_raw)
Betas <- factor(betas, levels = betas)

# dimensions
M <- length(models)
Y <- length(years_to_plot)
S <- length(Scenarios)
B <- length(Betas_raw)

# clear DF object
rm(DF)

# initialize empty dataframe
DF <- data.frame(Author = NULL, 
                 Model = NULL,
                 Scenario = NULL, 
                 Beta = NULL, 
                 Survive_to = NULL, 
                 Probability = NULL)

# initialize plot list
plot_list <- list()

# initialize super data frame
SDF <- data.frame(Author = NULL, 
                  Model = NULL,
                  Scenario = NULL, 
                  Beta = NULL, 
                  Survive_to = NULL, 
                  Probability = NULL)

# for each model
for (m in 1:M) {
  
  # for each year to plot
  for (y in 1:Y) {
    
    # for each scenario
    for (s in 1:S) {
      
      # for each mating function
      for (b in 1:B) {
        
        # load in appropriate output file
        
        if (desktop == TRUE) {
          
          # if the file exists: desktop
          if (file.exists(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                                models[m], '/', Scenarios[s], '/beta', Betas_raw[b],
                                '/', nsims, '_abundance_total.Rda', sep = '')) &
              
              file.exists(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                                models[m], '/', Scenarios[s], '/beta', Betas_raw[b],
                                '/', nsims, '_mature_abundance.Rda', sep = ''))) {
            
            # load in total abundance object
            load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                       models[m], '/', Scenarios[s], '/beta', Betas_raw[b], '/',
                       nsims, '_abundance_total.Rda', sep = ''))
            
            # load in abundance mature object
            load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                       models[m], '/', Scenarios[s], '/beta', Betas_raw[b], '/',
                       nsims, '_mature_abundance.Rda', sep = ''))
            
          }
          
        } else {
          
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
            
          }
          
        }
        
        # initialize dataframe
        DF2 <- data.frame(Author = authors[m], 
                          Model = model_types[m],
                          Scenario = Scenarios[s],
                          Beta = Betas[b],
                          Survive_to = years_to_plot[y], 
                          Probability = c(sum(sims_abundance_total[years_to_plot[y], ] > 
                                                0.1*sims_abundance_total[1, ]) / nsims))
        
        # add DF2 to DF
        DF <- rbind(DF, DF2)
        
      }
      
    }
    
    # make beta a factor variable
    DF$Beta <- factor(DF$Beta, levels = rev(Betas))

        # heatmap for survival to year 100
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
      xlab('Breeding sex ratio required to fertilize all females') +
      ylab('Increase in sand temperature (C) by year 100') +
      ggtitle(paste('Probability of population persistence (> 10% of starting population size) to year ', 
                    years_to_plot[y], sep = '')) +
      theme(panel.background = element_blank()) 
    
    # add figs to plot_list
    plot_list[[m]] <- fig
    
    # save to file
    ggsave(plot = fig, 
           filename = paste('Y', years_to_plot[y], '_', models_short[m], 
                            '_persistence_heatmap.png', sep = ''),
           path = '~/Projects/iliketurtles3/figures/',
           width = 8, height = 3.5)
     
  }
  
  # add model results to super data frame
  SDF <- rbind(SDF, DF)
  
}

# heatmap for survival to year 100
fig2 <- ggplot(data = SDF, aes(x = Beta, y = Scenario, fill = Probability)) +
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
  xlab('Breeding sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle(paste('Probability of population persistence \n (> 10% of starting population size) to year ', 
                years_to_plot[y], sep = '')) +
  facet_grid(rows = vars(Model), 
             cols = vars(Author)) +
  theme(plot.margin = unit(c(1, 0, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 12, vjust = -3)) +
  theme(axis.title.y = element_text(size = 12, vjust = 3)) +
  theme(title = element_text(size = 13))
  
# save to file
ggsave(plot = fig2, 
       filename = 'PvsGM_base_evol_highH.png',
       path = '~/Projects/iliketurtles3/figures/',
       width = 7, height = 7)
