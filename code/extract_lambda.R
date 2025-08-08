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
desktop <- TRUE

folder <- '2025_07_30_test'

# model(s)
# models <- c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
#             'P_evol_threshold', 'P_evol_threshold_high_H',
#             'GM_base', 'GM_evol_piv', 'GM_evol_piv_high_H', 
#             'GM_evol_threshold', 'GM_evol_threshold_high_H')
models <- c('P_base', 'GM_base')

# filepaths
paths <- as.vector(outer(folder, models, paste, sep = "/"))

# years to average over
average_over <- 10
years <- 1:100

# plotting model parameters
nsims <- 10000

# column names for combined heatmap
populations <- rep(c('West Africa', 'Suriname'), 
                   each = length(models) / 2)

# temperature increase scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')

# operational sex ratios / betas
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
betas <- OSRs_to_betas(osrs)

# abundances to plot
abundances <- c('Immature Females', 'Mature Females', 'Immature Males', 
                'Mature Males', 'Total', 'Mature')

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
SDF <- data.frame(Population = NULL, 
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
      DF <- data.frame(Population = populations[p], 
                       Model = models[p],
                       Scenario = scenarios[s], 
                       OSR = osrs[osr], 
                       Year = years, 
                       Abundance = rep(abundances, each = Y), 
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
                            betas[osr], '/', nsims, '_', 'N.Rda', sep = '')) 
          
      )  {
        
        # load in total abundance object
        load(paste('/home/quennessenv/iliketurtles3/output/',
                   paths[p], '/', scenarios[s], '/beta', 
                   betas[osr], '/', nsims, '_', 'N.Rda', sep = ''))
        
      }
      
      if (is.null(sims_N)) {
        
        DF$Lambda_mean[1:(A*Y)] <- NA
        DF$Lambda_median[1:(A*Y)] <- NA
        DF$Lambda_Q25[1:(A*Y)] <- NA
        DF$Lambda_Q75[1:(A*Y)] <- NA
        
        DF$Lambda_10yr_mean[1:(A*Y)] <- NA
        DF$Lambda_10yr_median[1:(A*Y)] <- NA
        DF$Lambda_10yr_Q25[1:(A*Y)] <- NA
        DF$Lambda_10yr_Q75[1:(A*Y)] <- NA
        
      } else {
        
        # TODO total and mature abundances
        IF <- colSums(sims_N[1, , , ], dim = 1)
        IM <- colSums(sims_N[2, , , ], dim = 1)
        MF <- colSums(sims_N[3, , , ], dim = 1)
        MM <- colSums(sims_N[4, , , ], dim = 1)
        
        total <- IF + IM + MF + MM
        mature <- MF + MM
        
        # calculate lambdas for each year for each simulation
        # year 1 is NA because there is no previous year to divide by
        lambdas_IF <- IF[2:Y, ] / IF[1:(Y - 1), ]
        lambdas_IM <- IM[2:Y, ] / IM[1:(Y - 1), ]
        lambdas_MF <- MF[2:Y, ] / MF[1:(Y - 1), ]
        lambdas_MM <- MM[2:Y, ] / MM[1:(Y - 1), ]
        
        lambdas_total <- total[2:Y, ] / total[1:(Y - 1), ]
        lambdas_mature <- mature[2:Y, ] / mature[1:(Y - 1), ]
        
        # replace Inf and NaN with NA
        lambdas_IF[!is.finite(lambdas_IF)] <- NA
        lambdas_IM[!is.finite(lambdas_IM)] <- NA
        lambdas_MF[!is.finite(lambdas_MF)] <- NA
        lambdas_MM[!is.finite(lambdas_MM)] <- NA
        
        lambdas_total[!is.finite(lambdas_total)] <- NA
        lambdas_mature[!is.finite(lambdas_mature)] <- NA
        
        # add average lambdas across simulations to DF
        # order IF, IM, MF, MM, total, mature
        DF$Lambda_mean <- c(NA, rowMeans(lambdas_IF, na.rm = TRUE), 
                            NA, rowMeans(lambdas_IM, na.rm = TRUE), 
                            NA, rowMeans(lambdas_MF, na.rm = TRUE), 
                            NA, rowMeans(lambdas_MM, na.rm = TRUE), 
                            NA, rowMeans(lambdas_total, na.rm = TRUE), 
                            NA, rowMeans(lambdas_mature, na.rm = TRUE))
        
        DF$Lambda_median <- c(NA, rowMedians(lambdas_IF, na.rm = TRUE), 
                              NA, rowMedians(lambdas_IM, na.rm = TRUE), 
                              NA, rowMedians(lambdas_MF, na.rm = TRUE), 
                              NA, rowMedians(lambdas_MM, na.rm = TRUE), 
                              NA, rowMedians(lambdas_total, na.rm = TRUE), 
                              NA, rowMedians(lambdas_mature, na.rm = TRUE))
        
        DF$Lambda_Q25 <- c(NA, rowQuantiles(lambdas_IF, prob = c(0.25), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_IM, prob = c(0.25), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_MF, prob = c(0.25), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_MM, prob = c(0.25), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_total, prob = c(0.25), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_mature, prob = c(0.25), 
                                            na.rm = TRUE))
        
        DF$Lambda_Q75 <- c(NA, rowQuantiles(lambdas_IF, prob = c(0.75), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_IM, prob = c(0.75), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_MF, prob = c(0.75), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_MM, prob = c(0.75), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_total, prob = c(0.75), 
                                            na.rm = TRUE), 
                           NA, rowQuantiles(lambdas_mature, prob = c(0.75), 
                                            na.rm = TRUE))
        
        # calculate 10 year average, Q25, and Q75 lambdas for years 11 - 100
        for (y in (average_over):(Y - 1)) {
          
          # mean lambdas per year across over_average years across all simulations
          mean_lambdas_IF <- mean(lambdas_IF[(y - average_over):y, ], 
                                  na.rm = TRUE)
          mean_lambdas_IM <- mean(lambdas_IM[(y - average_over):y, ], 
                                  na.rm = TRUE)
          mean_lambdas_MF <- mean(lambdas_MF[(y - average_over):y, ], 
                                  na.rm = TRUE)
          mean_lambdas_MM <- mean(lambdas_MM[(y - average_over):y, ], 
                                  na.rm = TRUE)
          mean_lambdas_total <- mean(lambdas_total[(y - average_over):y, ], 
                                     na.rm = TRUE)
          mean_lambdas_mature <- mean(lambdas_mature[(y - average_over):y, ], 
                                      na.rm = TRUE)
          
          # median lambdas per year across over_average years across all simulations
          median_lambdas_IF <- median(lambdas_IF[(y - average_over):y, ], 
                                      na.rm = TRUE)
          median_lambdas_IM <- median(lambdas_IM[(y - average_over):y, ], 
                                      na.rm = TRUE)
          median_lambdas_MF <- median(lambdas_MF[(y - average_over):y, ], 
                                      na.rm = TRUE)
          median_lambdas_MM <- median(lambdas_MM[(y - average_over):y, ], 
                                      na.rm = TRUE)
          median_lambdas_total <- median(lambdas_total[(y - average_over):y, ], 
                                         na.rm = TRUE)
          median_lambdas_mature <- median(lambdas_mature[(y - average_over):y, ], 
                                          na.rm = TRUE)
          
          # Q25 lambdas per year across over_average years across all simulations
          Q25_lambdas_IF <- quantile(lambdas_IF[(y - average_over):y, ], 
                                     na.rm = TRUE, probs = c(0.25))
          Q25_lambdas_IM <- quantile(lambdas_IM[(y - average_over):y, ], 
                                     na.rm = TRUE, probs = c(0.25))
          Q25_lambdas_MF <- quantile(lambdas_MF[(y - average_over):y, ], 
                                     na.rm = TRUE, probs = c(0.25))
          Q25_lambdas_MM <- quantile(lambdas_MM[(y - average_over):y, ], 
                                     na.rm = TRUE, probs = c(0.25))
          Q25_lambdas_total <- quantile(lambdas_total[(y - average_over):y, ], 
                                        na.rm = TRUE, probs = c(0.25))
          Q25_lambdas_mature <- quantile(lambdas_mature[(y - average_over):y, ],
                                         na.rm = TRUE, probs = c(0.25))
          
          # Q75 lambdas per year across over_average years across all simulations
          Q75_lambdas_IF <- quantile(lambdas_IF[(y - average_over):y, ], 
                                     na.rm = TRUE, probs = c(0.75))
          Q75_lambdas_IM <- quantile(lambdas_IM[(y - average_over):y, ], 
                                     na.rm = TRUE, probs = c(0.75))
          Q75_lambdas_MF <- quantile(lambdas_MF[(y - average_over):y, ], 
                                     na.rm = TRUE, probs = c(0.75))
          Q75_lambdas_MM <- quantile(lambdas_MM[(y - average_over):y, ], 
                                     na.rm = TRUE, probs = c(0.75))
          Q75_lambdas_total <- quantile(lambdas_total[(y - average_over):y, ], 
                                        na.rm = TRUE, probs = c(0.75))
          Q75_lambdas_mature <- quantile(lambdas_mature[(y - average_over):y, ],
                                         na.rm = TRUE, probs = c(0.75))
          
        }
        
        # add average lambdas to DF
        DF$Lambda_10yr_mean <- c(mean_lambdas_IF, mean_lambdas_IM, 
                                 mean_lambdas_MF, mean_lambdas_MM, 
                                 mean_lambdas_total, mean_lambdas_mature)
        
        DF$Lambda_10yr_median <- c(median_lambdas_IF, median_lambdas_IM, 
                                   median_lambdas_MF, median_lambdas_MM, 
                                   median_lambdas_total, median_lambdas_mature)
        
        DF$Lambda_10yr_Q25 <- c(Q25_lambdas_IF, Q25_lambdas_IM, 
                                Q25_lambdas_MF, Q25_lambdas_MM, 
                                Q25_lambdas_total, Q25_lambdas_mature)
        
        DF$Lambda_10yr_Q75 <- c(Q75_lambdas_IF, Q75_lambdas_IM, 
                                Q75_lambdas_MF, Q75_lambdas_MM, 
                                Q75_lambdas_total, Q75_lambdas_mature)
        
      }
        
        # add DF to SDF
        SDF <- rbind(SDF, DF)
        
        # print progress update
        print(paste(Sys.time(), ' - ', models[p], ' - ', scenarios[s], 
                    ' - beta ', betas[osr], ' - lambdas all done!', sep = ''))
      
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
