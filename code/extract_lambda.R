# plot final lambda at year 100

# set working directory
# setwd('~/Projects/iliketurtles3')

# source functions
source('mating function/OSRs_to_betas.R')

# load libraries
library(ggplot2)
# library(ggpattern)
library(matrixStats)
library(dplyr)
library(tidyr)

##### to modify ################################################################

# which computer am I using?
desktop <- FALSE

# folder(s)
stochasticity <- c('temp_stochasticity')

# model(s)
models <- c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
            'P_evol_threshold', 'P_evol_threshold_high_H',
            'GM_base', 'GM_evol_piv', 'GM_evol_piv_high_H', 
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

# abundances to plot
abundances <- c('abundance_total', 'abundance_mature')
abundance_names <- c('total abundance', 'mature abundance')

# dimensions
P <- length(paths)
M <- length(models)
S <- length(scenarios)
OSR <- length(osrs)
Y <- length(years)
A <- length(abundances)

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
                  Abundance = NULL,
                  Lambda_mean = NULL,
                  Lambda_median = NULL,
                  Lambda_Q25 = NULL, 
                  Lambda_Q75 = NULL,
                  Lambda_10yr_mean = NULL,
                  Lambda_10yr_median = NULL,
                  Lambda_10yr_Q25 = NULL, 
                  Lambda_10yr_Q75 = NULL)

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
                       Abundance = rep(abundance_names, each = Y), 
                       Lambda_mean = NA,
                       Lambda_median = NA,
                       Lambda_Q25 = NA, 
                       Lambda_Q75 = NA,
                       Lambda_10yr_mean = NA,
                       Lambda_10yr_median = NA,
                       Lambda_10yr_Q25 = NA, 
                       Lambda_10yr_Q75 = NA)
      
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
                            betas[osr], '/', nsims, '_', abundances[1], '.Rda', 
                            sep = '')) 
          
          &
          
          file.exists(paste('/home/quennessenv/iliketurtles3/output/',
                            paths[p], '/', scenarios[s], '/beta', 
                            betas[osr], '/', nsims, '_', abundances[2], '.Rda', 
                            sep = ''))
          
      )  {
        
        # load in total abundance object
        load(paste('/home/quennessenv/iliketurtles3/output/',
                   paths[p], '/', scenarios[s], '/beta', betas[osr], '/', nsims, 
                   '_', abundances[1], '.Rda', sep = ''))
        
        # load in mature abundance object
        load(paste('/home/quennessenv/iliketurtles3/output/',
                   paths[p], '/', scenarios[s], '/beta', betas[osr], '/', nsims, 
                   '_', abundances[2], '.Rda', sep = ''))
        
      }
      
      # calculate lambdas for each year for each simulation
      # year 1 is NA because there is no previous year to divide by
      lambdas_total <- sims_abundance_total[2:Y, ] / 
        sims_abundance_total[1:(Y - 1), ]
      lambdas_mature <- sims_abundance_mature[2:Y, ] / 
        sims_abundance_mature[1:(Y - 1), ]
      
      # replace Inf and NaN with NA
      lambdas_total[!is.finite(lambdas_total)] <- NA
      lambdas_mature[!is.finite(lambdas_mature)] <- NA
      
      # add average lambdas across simulations to DF
      DF$Lambda_mean[1:Y] <- c(NA, 
                               rowMeans(lambdas_total, 
                                        na.rm = TRUE))
      DF$Lambda_mean[(Y + 1):(2 * Y)] <- c(NA, 
                                           rowMeans(lambdas_mature, 
                                                    na.rm = TRUE))
      
      DF$Lambda_median[1:Y] <- c(NA, 
                                 rowMedians(lambdas_total, 
                                            na.rm = TRUE))
      DF$Lambda_median[(Y + 1):(2 * Y)] <- c(NA, 
                                             rowMedians(lambdas_mature, 
                                                        na.rm = TRUE))
      
      DF$Lambda_Q25[1:Y] <- c(NA, 
                              rowQuantiles(lambdas_total,
                                           prob = c(0.25),
                                           na.rm = TRUE))
      DF$Lambda_Q25[(Y + 1):(2 * Y)] <- c(NA, 
                                          rowQuantiles(lambdas_mature,
                                                       prob = c(0.25), 
                                                       na.rm = TRUE))
      
      DF$Lambda_Q75[1:Y] <- c(NA, 
                              rowQuantiles(lambdas_total,
                                           prob = c(0.75),
                                           na.rm = TRUE))
      DF$Lambda_Q75[(Y + 1):(2 * Y)] <- c(NA, 
                                          rowQuantiles(lambdas_mature,
                                                       prob = c(0.75), 
                                                       na.rm = TRUE))
      
      # initialize average, Q5, and Q95 lambdas for years 2 - 100
      mean_lambdas_total <- rep(NA, Y)
      mean_lambdas_mature <- rep(NA, Y)
      
      median_lambdas_total <- rep(NA, Y)
      median_lambdas_mature <- rep(NA, Y)
      
      Q25_lambdas_total <- rep(NA, Y)
      Q25_lambdas_mature <- rep(NA, Y)
      
      Q75_lambdas_total <- rep(NA, Y)
      Q75_lambdas_mature <- rep(NA, Y)
      
      # calculate average, Q25, and Q75 lambdas across over_average years for 
      # years 11 - 100
      for (y in (average_over):(Y - 1)) {
        
        # average lambdas per year across over_average years across all simulations
        mean_lambdas_total[y + 1] <- mean(
          lambdas_total[(y - average_over):y, ], 
          na.rm = TRUE)
        
        mean_lambdas_mature[y + 1] <- mean(
          lambdas_mature[(y - average_over):y, ], 
          na.rm = TRUE)
        
        median_lambdas_total[y + 1] <- median(
          lambdas_total[(y - average_over):y, ], 
          na.rm = TRUE)
        
        median_lambdas_mature[y + 1] <- median(
          lambdas_mature[(y - average_over):y, ], 
          na.rm = TRUE)
        
        # Q25 lambdas per year across over_average years across all simulations
        Q25_lambdas_total[y + 1] <- quantile(
          lambdas_total[(y - average_over):y, ], 
          na.rm = TRUE,
          probs = c(0.25))
        
        Q25_lambdas_mature[y + 1] <- quantile(
          lambdas_mature[(y - average_over):y, ],
          na.rm = TRUE,
          probs = c(0.25))
        
        Q75_lambdas_total[y + 1] <- quantile(
          lambdas_total[(y - average_over):y, ],
          na.rm = TRUE,
          probs = c(0.75))
        
        Q75_lambdas_mature[y + 1] <- quantile(
          lambdas_mature[(y - average_over):y, ],
          na.rm = TRUE,
          probs = c(0.75))
        
      }
      
      # add average lambdas to DF
      DF$Lambda_10yr_mean[1:Y] <- mean_lambdas_total
      DF$Lambda_10yr_mean[(Y + 1):(2 * Y)] <- mean_lambdas_mature
      
      DF$Lambda_10yr_median[1:Y] <- median_lambdas_total
      DF$Lambda_10yr_median[(Y + 1):(2 * Y)] <- median_lambdas_mature
      
      # add average lambdas to DF
      DF$Lambda_10yr_Q25[1:Y] <- Q25_lambdas_total
      DF$Lambda_10yr_Q25[(Y + 1):(2 * Y)] <- Q25_lambdas_mature
      
      # add average lambdas to DF
      DF$Lambda_10yr_Q75[1:Y] <- Q75_lambdas_total
      DF$Lambda_10yr_Q75[(Y + 1):(2 * Y)] <- Q75_lambdas_mature
      
      # add DF to SDF
      SDF <- rbind(SDF, DF)
      
      # print progress update
      print(paste(Sys.time(), ' - ', stochasticity[p], ' - ',
                  models[p], ' - ', scenarios[s], ' - beta ', 
                  betas[osr], ' lambdas all done!', sep = ''))
      
    }
    
  }
  
}

lambdas <- SDF

# set any year where persistence is 0 to NA
# SDF[which(is.infinite(SDF$Lambda_mean)), 8:13] <- NA
# SDF[which(SDF$Lambda_mean == 0), 8:13] <- NA

# # save dataframe as R object - desktop or laptop
# save(lambdas, file = paste('~/Projects/iliketurtles3/output/lambdas.Rdata', 
#                            sep = ''))

# save dataframe as R object - cluster
save(lambdas, file = '/home/quennessenv/iliketurtles3/output/lambdas.Rdata')
