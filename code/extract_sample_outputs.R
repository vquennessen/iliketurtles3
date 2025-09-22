# get vlines for plotting 10 yr median lambdas over time + IQR

# empty environment
rm(list = ls())

# load libraries
library(ggplot2)
library(matrixStats)
library(readr)
library(patchwork)
library(tidyverse)

# source functions and objects
load("~/Projects/iliketurtles3/output/red_noise_lambdas_and_persistence.Rdata")
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

##### abundances and sex ratios ################################################

# category titles
TRTs <- c('Narrow transitional range', 'Wide transitional range')
ages <- c('Hatchling', 'Mature')
folder <- '2025_09_19_red_noise'
years <- 100
nsims <- 10000
temp_mu <- 31.8
desktop <- TRUE
user <- ifelse(desktop == TRUE, 'Vic', 'vique')

# scenarios
temp_increases <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
scenarios <- paste(temp_increases, 'C', sep = '')


# osrs
osrs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
betas <- OSRs_to_betas(osrs)

##### create objects ###########################################################

# initialize super data frame (SDF)
SDF <- data.frame(TRT = NULL,
                  Scenario = NULL, 
                  OSR = NULL,
                  Beta = NULL,
                  Year = NULL,
                  Temperature = NULL,
                  Hatchling_Abundance_Median = NULL, 
                  Hatchling_Abundance_Q25 = NULL, 
                  Hatchling_Abundance_Q75 = NULL, 
                  Hatchling_Sex_Ratio_Median = NULL, 
                  Hatchling_Sex_Ratio_Q25 = NULL, 
                  Hatchling_Sex_Ratio_Q75 = NULL, 
                  Mature_Abundance_Median = NULL, 
                  Mature_Abundance_Q25 = NULL, 
                  Mature_Abundance_Q75 = NULL, 
                  Mature_Sex_Ratio_Median = NULL, 
                  Mature_Sex_Ratio_Q25 = NULL, 
                  Mature_Sex_Ratio_Q75 = NULL, 
                  Breeding_Success_Median = NULL, 
                  Breeding_Success_Q25 = NULL,
                  Breeding_success_Q75 = NULL
)

# for each population / TRT
for (t in 1:length(TRTs)) {
  
  if (TRTs[t] == 'Narrow transitional range') { model <- 'P_base' }
  if (TRTs[t] == 'Wide transitional range') { model <- 'GM_base' }
  
  # for each scenario
  for (s in 1:length(scenarios)) {
    
    
    temps <- seq(from = temp_mu, 
                 to = temp_mu + temp_increases[s], 
                 length = years)
    
    # for each osr
    for (b in 1:length(betas)) {
      
      # load in sims N
      load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/', 
                 'model output/i like turtles/', folder, '/', model, '/', 
                 scenarios[s], '/beta', betas[b], '/', nsims, '_N.Rda', 
                 sep = ''))
      
      # load in sims OSR
      load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/', 
                 'model output/i like turtles/', folder, '/', model, '/', 
                 scenarios[s], '/beta', betas[b], '/', nsims, '_OSR.Rda', 
                 sep = ''))
      
      # initialize
      subset <- data.frame(TRT = TRTs[t],
                           Scenario = scenarios[s], 
                           OSR = osrs[b], 
                           Beta = betas[b],
                           Year = 1:100,
                           Temperature = temps,
                           Hatchling_Abundance_Median = NA, 
                           Hatchling_Abundance_Q25 = NA, 
                           Hatchling_Abundance_Q75 = NA, 
                           Hatchling_Sex_Ratio_Median = NA, 
                           Hatchling_Sex_Ratio_Q25 = NA, 
                           Hatchling_Sex_Ratio_Q75 = NA,
                           Mature_Abundance_Median = NA, 
                           Mature_Abundance_Q25 = NA, 
                           Mature_Abundance_Q75 = NA, 
                           Mature_Sex_Ratio_Median = NA, 
                           Mature_Sex_Ratio_Q25 = NA, 
                           Mature_Sex_Ratio_Q75 = NA)
      
      if (is.numeric(sims_N)) {
        
        # extract hatchling F
        hatchlings_F <- sims_N[1, 1, , ]
        
        # extract hatchling M
        hatchlings_M <- sims_N[2, 1, , ]
        
        # hatchlings total
        hatchlings_total <- hatchlings_F + hatchlings_M
        
        # extract hatchling sex ratios, remove NaNs
        hatchling_sex_ratio <- hatchlings_M / (hatchlings_F + hatchlings_M)
        hatchling_sex_ratio[!is.finite(hatchling_sex_ratio)] <- NA
        
        # extract mature F
        mature_F <- colSums(sims_N[3, , , ], dims = 1)
        
        # extract mature M
        mature_M <- colSums(sims_N[4, , , ], dims = 1)
        
        # mature total
        mature_total <- mature_F + mature_M
        
        # mature sex ratio, remove infinite values
        OSR <- sims_OSR
        OSR[!is.finite(OSR)] <- NA
        
        # add values to data frame
        subset$Hatchling_Abundance_Median <- rowMedians(hatchlings_total, na.rm = TRUE)
        subset$Hatchling_Abundance_Q25 <- rowQuantiles(hatchlings_total, prob = 0.25, na.rm = TRUE)
        subset$Hatchling_Abundance_Q75 <- rowQuantiles(hatchlings_total, prob = 0.75, na.rm = TRUE)
        subset$Hatchling_Sex_Ratio_Median <- rowMedians(hatchling_sex_ratio, na.rm = TRUE)
        subset$Hatchling_Sex_Ratio_Q25 <- rowQuantiles(hatchling_sex_ratio, prob = 0.25, na.rm = TRUE)
        subset$Hatchling_Sex_Ratio_Q75 <- rowQuantiles(hatchling_sex_ratio, prob = 0.75, na.rm = TRUE)
        subset$Mature_Abundance_Median <- rowMedians(mature_total, na.rm = TRUE)
        subset$Mature_Abundance_Q25 <- rowQuantiles(mature_total, prob = 0.25, na.rm = TRUE)
        subset$Mature_Abundance_Q75 <- rowQuantiles(mature_total, prob = 0.75, na.rm = TRUE)
        subset$Mature_Sex_Ratio_Median <- rowMedians(OSR, na.rm = TRUE)
        subset$Mature_Sex_Ratio_Q25 <- rowQuantiles(OSR, prob = 0.25, na.rm = TRUE)
        subset$Mature_Sex_Ratio_Q75 <- rowQuantiles(OSR, prob = 0.75, na.rm = TRUE)
        
        # update tracker
        print(paste(model, ' - ', scenarios[s], ' - beta ', betas[b], 
                    ' - done!', sep = ''))
        print(paste('length SDF = ', nrow(SDF), sep = ''))
        
      } else {
        
        # add values to data frame
        subset$Hatchling_Abundance_Median <- NA
        subset$Hatchling_Abundance_Q25 <- NA
        subset$Hatchling_Abundance_Q75 <- NA
        subset$Hatchling_Sex_Ratio_Median <- NA
        subset$Hatchling_Sex_Ratio_Q25 <- NA
        subset$Hatchling_Sex_Ratio_Q75 <- NA
        subset$Mature_Abundance_Median <- NA
        subset$Mature_Abundance_Q25 <- NA
        subset$Mature_Abundance_Q75 <- NA
        subset$Mature_Sex_Ratio_Median <- NA
        subset$Mature_Sex_Ratio_Q25 <- NA
        subset$Mature_Sex_Ratio_Q75 <- NA
        
        # update tracker
        print(paste(model, ' - ', scenarios[s], ' - beta ', betas[b], 
                    ' - was all NA (and done)', sep = ''))
        print(paste('length SDF = ', nrow(SDF), sep = ''))
        
      }

        # tack subset onto SDF
        SDF <- rbind(SDF, subset)
        
      }
      
    }
    
}

abundance_sex_ratio_outputs <- SDF %>%
  mutate(Breeding_Success_Median = pbeta(2 * Mature_Sex_Ratio_Median, 
                                        shape1 = 1, 
                                        shape2 = as.numeric(Beta))) %>%
  mutate(Breeding_Success_Q25 = pbeta(2 * Mature_Sex_Ratio_Q25, 
                                         shape1 = 1, 
                                         shape2 = as.numeric(Beta))) %>%
  mutate(Breeding_Success_Q75 = pbeta(2 * Mature_Sex_Ratio_Q75, 
                                      shape1 = 1, 
                                      shape2 = as.numeric(Beta))) %>%
  mutate(Emergence_Success = 0.86 / (1 + exp(1.7 * (Temperature - 32.7)))) %>%
  mutate(Mating_Function = if_else(as.numeric(as.character(OSR)) < 0.26, 
                                   'Steep', 'Shallow')) %>%
  mutate(OSR = factor(OSR))

# save abundance and sex ratios object as its own thing
save(abundance_sex_ratio_outputs, 
     file = '~/Projects/iliketurtles3/output/red_noise_abundance_sex_ratio_outputs.Rdata')

##### plotting parameters ######################################################

# filter out stuff we don't want to plot
median_lambdas_adjusted <- lambdas_and_persistence %>%
  filter(Abundance == 'Mature') %>%
  mutate(Lambda_10yr_median = replace(Lambda_10yr_median, Persistence < 0.1, 
                                      NA)) %>%
  mutate(Lambda_10yr_Q25 = replace(Lambda_10yr_Q25, Persistence < 0.1, NA)) %>%
  mutate(Lambda_10yr_Q75 = replace(Lambda_10yr_Q75, Persistence < 0.1, NA)) %>%
  mutate(Mating_Function = if_else(as.numeric(as.character(OSR)) < 0.26, 
                                   'Steep', 'Shallow')) %>%
  select(Scenario, OSR, Year, TRT, Mating_Function, Persistence,
         Lambda_mean, Lambda_median, Lambda_Q25, Lambda_Q75, 
         Lambda_10yr_mean, Lambda_10yr_median, Lambda_10yr_Q25, Lambda_10yr_Q75) %>%
mutate(OSR = factor(OSR))

joined_outputs <- abundance_sex_ratio_outputs %>%
  full_join(median_lambdas_adjusted) %>%
  mutate(Scenario = paste(parse_number(Scenario), '\u00B0C', sep = '')) 

save(joined_outputs, 
     file = '~/Projects/iliketurtles3/output/red_noise_joined_outputs.Rdata')
