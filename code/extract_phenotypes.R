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
library(abind)
library(zoo)

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
      
      # extract mean g stats for hatchlings by year, across simulations
      
      GF_means <- rowMeans(sims_G_stats[1, 1, , 1, ], na.rm = TRUE)
      GM_means <- rowMeans(sims_G_stats[2, 1, , 1, ], na.rm = TRUE)
      sub_DF$G_mean <- rowMeans(data.frame(X = GF_means, Y = GM_means), 
                                na.rm = TRUE)
      
      GF_medians <- rowMedians(sims_G_stats[1, 1, , 2, ], na.rm = TRUE)
      GM_medians <- rowMedians(sims_G_stats[2, 1, , 2, ], na.rm = TRUE)
      sub_DF$G_median <- rowMeans(data.frame(X = GF_medians, Y = GM_medians), 
                                    na.rm = TRUE)
      
      GF_vars <- rowMeans(sims_G_stats[1, 1, , 3, ], na.rm = TRUE)
      GM_vars <- rowMeans(sims_G_stats[2, 1, , 3, ], na.rm = TRUE)
      sub_DF$G_var <- rowMeans(data.frame(X = GF_vars, GY = GM_vars), 
                               na.rm = TRUE)
      
      PF_means <- rowMeans(sims_P_stats[1, 1, , 1, ], na.rm = TRUE)
      PM_means <- rowMeans(sims_P_stats[2, 1, , 1, ], na.rm = TRUE)
      sub_DF$P_mean <- rowMeans(data.frame(X = PF_means, Y = PM_means), 
                                na.rm = TRUE)
      
      PF_medians <- rowMedians(sims_P_stats[1, 1, , 2, ], na.rm = TRUE)
      PM_medians <- rowMedians(sims_P_stats[2, 1, , 2, ], na.rm = TRUE)
      sub_DF$P_median <- rowMeans(data.frame(X = PF_medians, Y = PM_medians), 
                                    na.rm = TRUE)
      
      PF_vars <- rowMeans(sims_P_stats[1, 1, , 3, ], na.rm = TRUE)
      PM_vars <- rowMeans(sims_P_stats[2, 1, , 3, ], na.rm = TRUE)
      sub_DF$P_var <- rowMeans(data.frame(X = PF_vars, Y = PM_vars), 
                               na.rm = TRUE)
      
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

traits <- SDF %>%
  mutate(G_mean_10yr = rollapply(G_mean, width = average_over, mean, 
                                 na.rm = TRUE, fill = NA, align = 'right')) %>%
  mutate(G_median_10yr = rollapply(G_median, width = average_over, median, 
                                   na.rm = TRUE, fill = NA, align = 'right')) %>%
  mutate(G_var_10yr = rollapply(G_var, width = average_over, mean, 
                                 na.rm = TRUE, fill = NA, align = 'right')) %>%
  mutate(P_mean_10yr = rollapply(P_mean, width = average_over, mean, 
                                 na.rm = TRUE, fill = NA, align = 'right')) %>%
  mutate(P_median_10yr = rollapply(P_median, width = average_over, median, 
                                   na.rm = TRUE, fill = NA, align = 'right')) %>% 
  mutate(P_var_10yr = rollapply(P_var, width = average_over, mean, 
                                na.rm = TRUE, fill = NA, align = 'right'))

# save dataframe as R object
save(traits, 
     file = '../output/evolution_trait_values.Rdata')
