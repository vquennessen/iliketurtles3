# extract hatchling phenotype values

rm(list = ls())

# set working directory
# setwd('~/Projects/iliketurtles3/code')

# source functions
source('mating function/OSRs_to_betas.R')

# load libraries
library(ggplot2)
library(matrixStats)
library(dplyr)
library(tidyr)

##### to modify ################################################################

# which computer we using
computer <- 'cluster'

# path based on computer being used
user <- ifelse(computer == 'cluster', '/home/quennessenv/iliketurtles3/output/',
               ifelse(computer == 'desktop',
                      'C:/Users/Vic/Box/Quennessen_Thesis/PhD Thesis/model output/iliketurtles3/',
                      'C:/Users/vique/Box/Quennessen_Thesis/PhD Thesis/model output/iliketurtles3/'))

# name of folder for current runs
input_folders <- c('2025_11_30_evolution')

# number of sims
nsims <- 100

# name to save to
name <- paste(input_folders, '_n', nsims, sep = '')

# model(s)
traits <- c('T_piv', 'emergence_success_t0')
rates <- c('effective', 'high')
TRTs <- c('narrow', 'wide')

paths <- expand.grid(input_folders, TRTs, traits, rates) %>%
  # mutate(path = paste(Var1, Var2, sep = '/')) 
  mutate(path = paste(Var1, Var3, Var4, Var2, sep = '/')) 

# years to average over
average_over <- 10
years <- 1:100

# temperature increase scenarios
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)

# operational sex ratios / betas
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
betas <- OSRs_to_betas(osrs)

# dimensions
P <- nrow(paths)
S <- length(scenarios)
B <- length(betas)
Y <- length(years)
numrows <- P*S*B*Y

# initialize super data frame
SDF <- data.frame(Folder = NULL,
                  TRT = NULL,
                  Trait = NULL, 
                  Rate = NULL, 
                  Scenario = NULL, 
                  OSR = NULL, 
                  Year = NULL,
                  G_mean = NULL,
                  G_median = NULL,
                  G_var = NULL, 
                  P_mean = NULL,
                  P_median = NULL,
                  P_var = NULL)

for (p in 1:P) {
  
  for (s in 1:S) {
    
    for (b in 1:B) {
      
      # initialize empty dataframe, one for each filepath
      sub_DF <- data.frame(Folder = rep(paths[p, ]$Var1, Y),
                           TRT = rep(paths[p, ]$Var2, Y),
                           Trait = rep(paths[p, ]$Var3, Y),
                           Rate = rep(paths[p, ]$Var4, Y),
                           Scenario = rep(scenarios[s], Y),
                           Beta = rep(betas[b], Y),
                           Year = years, 
                           G_mean = rep(NA, Y),
                           G_median = rep(NA, Y),
                           G_var = rep(NA, Y), 
                           P_mean = rep(NA, Y),
                           P_median = rep(NA, Y),
                           P_var = rep(NA, Y))
      
      # load in appropriate output file
      
      G_stats <- paste(user, paths[p, ]$path, '/', scenarios[s], 'C/beta', 
                            betas[b], '/', nsims, '_G_stats.Rda', sep = '')
      
      P_stats <- paste(user, paths[p, ]$path, '/', scenarios[s], 'C/beta', 
                            betas[b], '/', nsims, '_P_stats.Rda', sep = '')
      
      # if the file exists - cluster
      if (file.exists(G_stats) & file.exists(P_stats))  {
        
        # load in genetics stats
        load(G_stats)
        
        # load in phenotypes stats
        load(P_stats)
        
      }
      
      G_stats <- rowMeans(sims_G_stats, dim = c(4))
      P_stats <- rowMeans(sims_P_stats, dim = c(4))
      
      # extract mean g stats for hatchlings by year, across simulations
      sub_DF$G_mean <- colMeans(G_stats[1:2, 1, , 1], dim = c(1), na.rm = TRUE)
      sub_DF$G_median <- colMeans(G_stats[1:2, 1, , 2], dim = c(1), na.rm = TRUE)
      sub_DF$G_var <- colMeans(G_stats[1:2, 1, , 3], dim = c(1), na.rm = TRUE)
      
      # extract mean p stats for hatchlings by year, across simulations
      sub_DF$P_mean <- colMeans(P_stats[1:2, 1, , 1], dim = c(1), na.rm = TRUE)
      sub_DF$P_median <- colMeans(P_stats[1:2, 1, , 2], dim = c(1), na.rm = TRUE)
      sub_DF$P_var <- colMeans(P_stats[1:2, 1, , 3], dim = c(1), na.rm = TRUE)
      
      # add DF to SDF
      SDF <- rbind(SDF, sub_DF)
      
      prop <- round(nrow(SDF) / (numrows) * 100, 2)
      boop <- format(lubridate::now())
      print(paste(boop, ' - ', paths[p, ]$Var2, ' - ', paths[p, ]$Var3, 
                  ' - ', paths[p, ]$Var4, ' - ', scenarios[s],
                  'C - beta ', betas[b], ' - done - ', prop, 
                  '% of total done!', sep = ''))
      
    }
    
  }
  
}

traits <- SDF

# save dataframe as R object
save(traits, 
     file = '../output/evolution_trait_values.Rdata')
