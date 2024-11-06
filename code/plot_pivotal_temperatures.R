# pivotal temperatures

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)

# source functions
source('code/mating function/OSRs_to_betas.R')

# which computer am I using?
desktop <- TRUE

# plotting model parameters
nsims <- 10000

# folder
folder <- 'no_temp_stochasticity/'

# test runs - folder names
models <- paste(folder, c('P_evol', 'P_evol_high_H', 
                          'GM_evol', 'GM_evol_high_H'), sep = '')

# individual heatmap titles
models_short <- c('P_evo', 'P_high_H',
                  'GM_evo', 'GM_high_H')

# column names for combined heatmap
pops <- c(rep('West Africa', 2),
             rep('Suriname', 2))

# row names for combined heatmap
model_types <- rep(c('evolution', 'evolution with high H'),
                   times = 2)

# which temperature increase scenarios 
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')
# scenarios <- paste(c(0.5, 5), 'C', sep = '')

# which operational sex ratios to fertilize all females
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)

# associated beta values
betas <- OSRs_to_betas(osrs)

# dimensions
M <- length(models)
S <- length(scenarios)
B <- length(osrs)

# clear DF and SDF objects
rm(DF)
rm(SDF)

# initialize empty dataframe
DF <- data.frame(Pop = NULL, 
                 Model = NULL,
                 Scenario = NULL, 
                 OSR = NULL, 
                 Probability = NULL)

# initialize plot list
plot_list <- list()

# initialize super data frame
SDF <- data.frame(Pop = NULL, 
                  Model = NULL,
                  Scenario = NULL, 
                  OSR = NULL, 
                  Probability = NULL)

# for each model
for (m in 1:M) {

    # for each scenario
    for (s in 1:S) {
      
      # for each mating function
      for (b in 1:B) {
        
        # load in appropriate output file
        
        if (desktop == TRUE) {
          
          # if the file exists: desktop
          if (file.exists(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                                models[m], '/', scenarios[s], '/beta', betas[b],
                                '/', nsims, '_ptiv.Rda', sep = ''))) {
            
            # load in total abundance object
            load(paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                       models[m], '/', scenarios[s], '/beta', betas[b], '/',
                       nsims, '_ptiv.Rda', sep = ''))
            
          }
          
        } else {
          
          # if the file exists: laptop
          if (file.exists(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                                models[m], '/', scenarios[s], '/beta', betas[b],
                                '/', nsims, '_ptiv.Rda', sep = ''))) {
            
            # load in total abundance object
            load(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                       models[m], '/', scenarios[s], '/beta', betas[b], '/',
                       nsims, '_ptiv.Rda', sep = ''))
            
          }
          
        }
        
        # extract last year with pivotal temperature
        last_ptivs <- apply(sims_ptiv, 2, function(x) x[max(which(!is.na(x)))])
        
        # initialize dataframe
        DF2 <- data.frame(Pop = pops[m], 
                          Model = model_types[m],
                          Scenario = scenarios[s],
                          OSR = osrs[b],
                          Probability = mean(last_ptivs > sims_ptiv[1, ], na.rm = TRUE))

        # add DF2 to DF
        DF <- rbind(DF, DF2)
        
      }
      
    }
    
    # make scenario and osr a factor variable
    DF$Scenario <- factor(DF$Scenario, levels = scenarios)

    # heatmap for survival to year 100
    fig <- ggplot(data = DF, aes(x = OSR, y = Scenario, fill = Probability)) +
      geom_tile(color = "white",
                lwd = 1.5,
                linetype = 1) +
      scale_fill_gradient2(low = hcl.colors(5, "viridis")[1], 
                           mid = hcl.colors(5, "viridis")[3], 
                           high = hcl.colors(5, "viridis")[5], #colors in the scale
                           midpoint = 0.53,    #same midpoint for plots (mean of the range)
                           breaks = c(0.47, 0.53, 0.59), #breaks in the scale bar
                           limits = c(0.47, 0.59), 
                           na.value = 'gray') +
      guides(fill = guide_colourbar(title = "Probability")) +
      xlab('Operational sex ratio required to fertilize all females') +
      ylab('Increase in sand temperature (C) by year 100') +
      ggtitle('Probability of increase in pivotal temperature') +
      theme(panel.background = element_blank()) 
    
    # add figs to plot_list
    plot_list[[m]] <- fig
    
    # save to file
    ggsave(plot = fig, 
           filename = paste(models_short[m], '_tpiv_heatmap.png', sep = ''),
           path = '~/Projects/iliketurtles3/figures/',
           width = 8, height = 3.5)
    
  # }
  # 
  # # add model results to super data frame
  # SDF <- rbind(SDF, DF)
  
}

# heatmap for survival to year 100
fig2 <- ggplot(data = DF, aes(x = OSR, y = Scenario, fill = Probability)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, "viridis")[1], 
                       mid = hcl.colors(5, "viridis")[3], 
                       high = hcl.colors(5, "viridis")[5], #colors in the scale
                       midpoint = 0.53,    #same midpoint for plots (mean of the range)
                       breaks = c(0.47, 0.53, 0.59), #breaks in the scale bar
                       limits = c(0.47, 0.59),  
                       na.value = 'gray') +
  guides(fill = guide_colourbar(title = "Probability")) +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle('Probability of increase in pivotal temperature') +
  facet_grid(rows = vars(Model), 
             cols = vars(Pop)) +
  theme(plot.margin = unit(c(1, 0, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 12, vjust = -3)) +
  theme(axis.title.y = element_text(size = 12, vjust = 3)) +
  theme(title = element_text(size = 13))

# save to file
ggsave(plot = fig2, 
       filename = paste('PvsGM_base_evol_highH_tpiv.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 7, height = 7)

##### pivotal temperature plot over time for P evol high H 5C beta 1 ###########

# load in object
load('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/model output/no_temp_stochasticity/P_evol_high_H/5C/beta1/10000_ptiv.Rda')

# extract average pivotal temperature per year
DF3 <- data.frame(Year = 1:100, 
                  Pivotal_Temperature = rowMeans(sims_ptiv, na.rm = TRUE))

# plot average pivotal temperature for each year
fig3 <- ggplot(data = DF3, aes(x = Year, y = Pivotal_Temperature)) +
  geom_segment(x = 0, y = DF3$Pivotal_Temperature[1], 
               xend = 100, yend = DF3$Pivotal_Temperature[1], 
             lwd = 1.5, col = hcl.colors(5, "viridis")[2]) +
  geom_line(data = DF3, aes(x = Year, y = Pivotal_Temperature), 
            lwd = 2, col = hcl.colors(5, "viridis")[1]) +
  ylim(c(29.175, 29.225)) + 
  ylab('') +
  ggtitle('Average Pivotal Temperature') +
  theme(plot.margin = unit(c(1, 0.25, 1, -0.25), units = 'cm')) +
  theme(axis.title.x = element_text(size = 20, vjust = -3)) +
  theme(title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 15))

# save to file
ggsave(plot = fig3, 
       filename = paste('P_evol_high_H_5c_beta1_avg_tpiv.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 6)

# histogram of final pivotal temperatures
# extract last year with pivotal temperature
last_ptivs <- apply(sims_ptiv, 2, function(x) x[max(which(!is.na(x)))])
DF4 <- data.frame(i = 1:nsims, 
                  final_pivotal_temperature = last_ptivs)
hist(last_ptivs)

ggplot(data = DF4, aes(x = last_ptivs)) + 
  geom_histogram(fill = hcl.colors(5, "viridis")[1])
