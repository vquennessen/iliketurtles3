# extract threshold temperatures

# set working directory
# setwd('~/Projects/iliketurtles3')

# source functions
source('mating function/OSRs_to_betas.R')

# load libraries
library(ggplot2)
library(matrixStats)
library(dplyr)
library(tidyr)

##### to modify ################################################################

# which computer am I using?
desktop <- FALSE

# folder(s)
stochasticity <- c('temp_stochasticity')

# model(s)
models <- c('P_evol_threshold', 'P_evol_threshold_high_H', 
            'GM_evol_threshold', 'GM_evol_threshold_high_H')

# filepaths
paths <- as.vector(outer(stochasticity, models, paste, sep="/"))

# years to average over
average_over <- 10
years <- 1:100

# plotting model parameters
nsims <- 10000

# column names for combined heatmap
populations <- rep(c('West Africa', 'Suriname'), each = length(models)/2)

# row names for combined heatmap
stochasticity <- rep('temperature stochasticity',
                     times = length(models))

# temperature increase scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')

# operational sex ratios / betas
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
betas <- OSRs_to_betas(osrs)

# dimensions
P <- length(paths)
M <- length(models)
S <- length(scenarios)
OSR <- length(osrs)
Y <- length(years)

# clear DF and SDF objects
# rm(DF)
# rm(SDF)

# initialize plot list
plot_list <- list()

# initialize super data frame
SDF <- data.frame(Stochasticity = NULL, 
                  Population = NULL, 
                  Model = NULL,
                  Scenario = NULL, 
                  OSR = NULL, 
                  Year = NULL,
                  Threshold_mean = NULL,
                  Threshold_median = NULL,
                  Threshold_Q25 = NULL, 
                  Threshold_Q75 = NULL)

for (p in 1:P) {
  
  for (s in 1:S) {
    
    for (osr in 1:OSR) {
      
      # initialize empty dataframe, one for each filepath
      DF <- data.frame(Stochasticity = stochasticity[1], 
                       Population = populations[p], 
                       Model = models[p],
                       Scenario = scenarios[s], 
                       OSR = osrs[osr], 
                       Year = years, 
                       Threshold_mean = NA,
                       Threshold_median = NA,
                       Threshold_Q25 = NA, 
                       Threshold_Q75 = NA)
      
      # load in appropriate output file
      
      if (desktop == TRUE) { user <- 'Vic' } else { user <- 'vique' }
      
      # # if the file exists - desktop or laptop
      # if (file.exists(paste('C:/Users/', user, 
      #                       '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
      #                       paths[p], '/', scenarios[s], '/beta', 
      #                       betas[osr], '/', nsims, '_', abundances[1], '.Rda', 
      #                       sep = '')) 
      #     
      #     &
      #     
      #     file.exists(paste('C:/Users/', user, 
      #                       '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
      #                       paths[p], '/', scenarios[s], '/beta', 
      #                       betas[osr], '/', nsims, '_', abundances[2], '.Rda', 
      #                       sep = ''))
      #     
      # )  {
      #   
      #   # load in total abundance object
      #   load(paste('C:/Users/', user, 
      #              '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
      #              paths[p], '/', scenarios[s], '/beta', betas[osr], '/', nsims, 
      #              '_', abundances[1], '.Rda', sep = ''))
      #   
      #   # load in mature abundance object
      #   load(paste('C:/Users/', user, 
      #              '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
      #              paths[p], '/', scenarios[s], '/beta', betas[osr], '/', nsims, 
      #              '_', abundances[2], '.Rda', sep = ''))
      #   
      # }
      
      # if the file exists - cluster
      if (file.exists(paste('/home/quennessenv/iliketurtles3/output/',
                            paths[p], '/', scenarios[s], '/beta', 
                            betas[osr], '/', nsims, '_threshold.Rda', sep = '')) 
          
          &
          
          file.exists(paste('/home/quennessenv/iliketurtles3/output/',
                            paths[p], '/', scenarios[s], '/beta', 
                            betas[osr], '/', nsims, '_threshold.Rda', 
                            sep = ''))
          
      )  {
        
        # load in total abundance object
        load(paste('/home/quennessenv/iliketurtles3/output/',
                   paths[p], '/', scenarios[s], '/beta', betas[osr], '/', nsims, 
                   '_threshold.Rda', sep = ''))
        
        # load in mature abundance object
        load(paste('/home/quennessenv/iliketurtles3/output/',
                   paths[p], '/', scenarios[s], '/beta', betas[osr], '/', nsims, 
                   '_threshold.Rda', sep = ''))
        
      }
      
      # replace Inf and NaN with NA
      sims_threshold[!is.finite(sims_threshold)] <- NA
      
      # calculate pivotal temps for hatchlings for each year for each simulation
      
      # add average lambdas across simulations to DF
      DF$Threshold_mean[1:Y] <- rowMeans(sims_threshold, na.rm = TRUE)
      
      DF$Threshold_median[1:Y] <- rowMedians(sims_threshold, na.rm = TRUE)
      
      DF$Threshold_Q25[1:Y] <- rowQuantiles(sims_threshold, prob = c(0.25), na.rm = TRUE)
      
      DF$Threshold_Q75[1:Y] <- rowQuantiles(sims_threshold, prob = c(0.75), na.rm = TRUE)
      
      # add DF to SDF
      SDF <- rbind(SDF, DF)
      
      # print progress update
      print(paste(Sys.time(), ' - ', models[p], ' - ', scenarios[s], ' - beta ', 
                  betas[osr], ' threshold temps all done!', sep = ''))
      
    }
    
  }
  
}

threshold_temps <- SDF

# # save dataframe as R object - desktop or laptop
# save(pivotal_temps, file = paste('~/Projects/iliketurtles3/output/pivotal_temperatures.Rdata', 
#                            sep = ''))

# save dataframe as R object - cluster
save(threshold_temps, file = '/home/quennessenv/iliketurtles3/output/threshold_temperatures.Rdata')
