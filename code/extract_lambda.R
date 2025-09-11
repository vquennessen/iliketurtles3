# plot final lambda at year 100

# set working directory
# setwd('~/Projects/iliketurtles3')

# source functions
source('/home/quennessenv/iliketurtles3/code/mating function/OSRs_to_betas.R')
# source('code/mating function/OSRs_to_betas.R')

# load libraries
library(matrixStats)
library(dplyr)
library(tidyr)

##### to modify ################################################################

# which computer am I using?
desktop <- TRUE

folder <- '2025_08_14'

# model(s)
# models <- c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
#             'P_evol_threshold', 'P_evol_threshold_high_H',
#             'GM_base', 'GM_evol_piv', 'GM_evol_piv_high_H', 
#             'GM_evol_threshold', 'GM_evol_threshold_high_H')
models <- c('P_base', 'GM_base')

# filepaths
folders <- rep(folder, length(models))
paths <- paste(folders, models, sep = "/")

# years to average over
average_over <- 10
years <- 1:100

# plotting model parameters
nsims <- 10000

# column names for combined heatmap
populations <- c('West Africa', 'Suriname')

# temperature increase scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')

# operational sex ratios / betas
osrs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
betas <- OSRs_to_betas(osrs)

# abundances to plot
abundances <- c('Immature Females', 'Immature Males', 'Mature Females', 
                'Mature Males', 'Total', 'Mature')
indices <- list(1, 2, 3, 4, c(1:4), c(3:4))

# dimensions
P <- length(paths)
M <- length(models)
S <- length(scenarios)
OSR <- length(osrs)
Y <- length(years)
A <- length(abundances)
nrows <- M*S*OSR*Y*A

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
                  Abundance = NULL,
                  Year = NULL,
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
                   betas[osr], '/', nsims, '_N.Rda', sep = ''))
        
      }
      
      if (is.null(sims_N)) {
        
        DF <- data.frame(Population = populations[p], 
                         Model = models[p],
                         Scenario = scenarios[s], 
                         OSR = osrs[osr], 
                         Abundance = rep(abundances, each = Y),
                         Year = rep(years, times = A),
                         Lambda_mean = rep(NA, times = A*Y), 
                         Lambda_median = rep(NA, times = A*Y), 
                         Lambda_Q25 = rep(NA, times = A*Y), 
                         Lambda_Q75 = rep(NA, times = A*Y), 
                         Lambda_10yr_mean = rep(NA, times = A*Y), 
                         Lambda_10yr_median = rep(NA, times = A*Y), 
                         Lambda_10yr_Q25 = rep(NA, times = A*Y), 
                         Lambda_10yr_Q75 = rep(NA, times = A*Y))
        
        SDF <- rbind(SDF, DF)
        
        # print progress update
        prop <- round(nrow(SDF) / nrows * 100, 2)
        print(paste(Sys.time(), ' - ', models[p], ' - ', scenarios[s], 
                    ' - beta ', betas[osr], ' - no SAD, no lambdas - ', prop, 
                    '% of total done!', sep = ''))
        
      } else {
        
        for (a in 1:A) {             
          
          # initialize empty dataframe, one for each filepath
          DF <- data.frame(Population = populations[p], 
                           Model = models[p],
                           Scenario = scenarios[s], 
                           OSR = osrs[osr], 
                           Abundance = abundances[a], 
                           Year = years, 
                           Lambda_mean = NA,
                           Lambda_median = NA,
                           Lambda_Q25 = NA, 
                           Lambda_Q75 = NA,
                           Lambda_10yr_mean = NA,
                           Lambda_10yr_median = NA,
                           Lambda_10yr_Q25 = NA, 
                           Lambda_10yr_Q75 = NA)
          
          # extract abundances
          if (a == 1) { N_abundances <- colSums(sims_N[1, , , ], dim = 1) }
          if (a == 2) { N_abundances <- colSums(sims_N[2, , , ], dim = 1) }  
          if (a == 3) { N_abundances <- colSums(sims_N[3, , , ], dim = 1) }  
          if (a == 4) { N_abundances <- colSums(sims_N[4, , , ], dim = 1) }  
          if (a == 5) { N_abundances <- colSums(sims_N[1:4, , , ], dim = 2) }  
          if (a == 6) { N_abundances <- colSums(sims_N[3:4, , , ], dim = 2) }       
          
          # calculate lambdas
          lambdas <- N_abundances[2:Y, ] / N_abundances[1:(Y - 1), ]
          
          # replace infinite / NaN with NA
          lambdas[!is.finite(lambdas)] <- NA
          
          # add lambdas to DF, with NA in front for year 1
          DF$Lambda_mean <- c(NA, rowMeans(lambdas, na.rm = TRUE))
          DF$Lambda_median <- c(NA, rowMedians(lambdas, na.rm = TRUE))
          DF$Lambda_Q25 <- c(NA, rowQuantiles(lambdas, prob = c(0.25), 
                                              na.rm = TRUE))
          DF$Lambda_Q75 <- c(NA, rowQuantiles(lambdas, prob = c(0.75), 
                                              na.rm = TRUE))
          
          mean_lambdas <- rep(NA, Y)
          median_lambdas <- rep(NA, Y)
          Q25_lambdas <- rep(NA, Y)
          Q75_lambdas <- rep(NA, Y)
          
          for (y in (average_over + 1):Y) {
            
            # mean lambdas per year across over_average years across all simulations
            mean_lambdas[y] <- mean(lambdas[(y - average_over):(y - 1), ], 
                                    na.rm = TRUE)
            
            median_lambdas[y] <- median(lambdas[(y - average_over):(y - 1), ], 
                                        na.rm = TRUE)
            
            Q25_lambdas[y] <- quantile(lambdas[(y - average_over):(y - 1), ], 
                                       na.rm = TRUE, probs = c(0.25))
            
            Q75_lambdas[y] <- quantile(lambdas[(y - average_over):(y - 1), ], 
                                       na.rm = TRUE, probs = c(0.75))
            
          }
          
          # add average lambdas to DF
          DF$Lambda_10yr_mean <- mean_lambdas
          DF$Lambda_10yr_median <- median_lambdas
          DF$Lambda_10yr_Q25 <- Q25_lambdas
          DF$Lambda_10yr_Q75 <- Q75_lambdas
          
          # add DF to SDF
          SDF <- rbind(SDF, DF)        
          
          # print progress update
          prop <- nrow(SDF) / nrows * 100
          print(paste(Sys.time(), ' - ', models[p], ' - ', scenarios[s], 
                      ' - beta ', betas[osr], ' - ', abundances[a], 
                      ' - lambdas done - ', prop, '% of total done!', sep = ''))
          
        }
        
      }
      
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
