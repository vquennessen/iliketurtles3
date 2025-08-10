# get vlines for plotting 10 yr median lambdas over time + IQR

# empty environment
rm(list = ls())

# load libraries
library(ggplot2)
library(matrixStats)
library(readr)
library(patchwork)
library(tidyverse)

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

##### plotting parameters ######################################################

# category titles
TRTs <- c('Narrow', 'Wide')
ages <- c('Hatchling', 'Mature')
folder <- '2025_07_30_test'
years <- 100
nsims <- 10000
temp_mu <- 31.8
desktop <- TRUE
user <- ifelse(desktop == TRUE, 'Vic', 'vique')

# scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')
final_temps <- paste(c(temp_mu + parse_number(scenarios)), ' \u00B0C', sep = '')

# osrs
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
betas <- as.numeric(OSRs_to_betas(osrs))
# betas <- c(43.71, 20.64, 12.92, 9.02, 6.65, 5.03, 3.83, 2.87, 2.01, 1)

# # ideal hatchling sex ratios based on mating system
# IHSR <- (1.47 * osrs) / (1.47 * osrs + 3.87 * (1 - osrs))
# # [1] 0.01960000 [2] 0.04049587 [3] 0.06282051 [4] 0.08672566 [5] 0.11238532 
# # [6] 0.14000000 [7] 0.16980198 [8] 0.20206186 [9] 0.23709677 [10] 0.26737194
# 
# # temperatures that give those IHSRs based on thermal reaction norm
# # narrow TRT population; k = -1.34, pivotal temp = 29.2
# ITemps_narrow <- (log( 1 / IHSR - 1) + 1.34 * 29.2) / 1.34
# # [1] 32.11972 [2] 31.56210 [3] 31.21686 [4] 30.95693 [5] 30.74224 
# # [6] 30.55469 [7] 30.38435 [8] 30.22497 [9] 30.07214 [10] 29.95224
# 
# # wide TRT population, k = -0.561, pivotal temp = 29.2
# ITemps_wide <- (log( 1 / IHSR - 1) + 0.561 * 29.2) / 0.561
# # [1] 36.17403 [2] 34.84210 [3] 34.01746 [4] 33.39659 [5] 32.88379 
# # [6] 32.43581 [7] 32.02893 [8] 31.64823 [9] 31.28318 [10] 30.99679
# 
# # ideal values
# ideals <- data.frame(
#   Min_OSR = osrs, 
#   Ideal_Hatchling_Sex_Ratio = IHSR, 
#   Ideal_temps_narrow = ITemps_narrow, 
#   Ideal_temps_wide = ITemps_wide
# )
# 
# write_csv(ideals, file = '../output/ideals.csv')

beta_names <- paste('beta', betas, sep = '')

# # maturity ogive
# max_age <- 85
# age_maturity_mu <- 25
# age_maturity_sd <- 2.5
# M <- pnorm(q = 1:max_age, mean = age_maturity_mu, sd = age_maturity_sd)

##### create objects ###########################################################

# initialize super data frame (SDF)
SDF <- data.frame(TRT = NULL,
                  Scenario = NULL, 
                  # Final_Temp = NULL,
                  OSR = NULL,
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
                  Breeding_success_Q75 = NULL, 
                  Eggs_to_hatch_Median = NULL,
                  Eggs_to_hatch_Q25 = NULL, 
                  Eggs_to_hatch_Q75 = NULL
)

# for each population / TRT
for (t in 1:length(TRTs)) {
  
  if (TRTs[t] == 'Narrow') { model <- 'P_base' }
  if (TRTs[t] == 'Wide') { model <- 'GM_base' }
  
  # for each scenario
  for (s in 1:length(scenarios)) {
    
    temp_increases <- parse_number(scenarios)
    
    temps <- seq(from = temp_mu, 
                 to = temp_mu + temp_increases[s], 
                 length = years)
    
    # for each osr
    for (b in 1:length(betas)) {
      
      # load in sims N
      load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/', 
                 'model output/i like turtles/', folder, '/', model, '/', 
                 scenarios[s], '/', beta_names[b], '/', nsims, '_N.Rda', 
                 sep = ''))
      
      # load in sims OSR
      load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/', 
                 'model output/i like turtles/', folder, '/', model, '/', 
                 scenarios[s], '/', beta_names[b], '/', nsims, '_OSR.Rda', 
                 sep = ''))
      
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
        subset <- data.frame(TRT = TRTs[t],
                             Scenario = scenarios[s], 
                             # Final_Temp = final_temps[s],
                             OSR = osrs[b], 
                             Year = 1:100,
                             Temperature = temps,
                             Hatchling_Abundance_Median = rowMedians(hatchlings_total, 
                                                                     na.rm = TRUE), 
                             Hatchling_Abundance_Q25 = rowQuantiles(hatchlings_total, 
                                                                    prob = 0.25, 
                                                                    na.rm = TRUE), 
                             Hatchling_Abundance_Q75 = rowQuantiles(hatchlings_total, 
                                                                    prob = 0.75, 
                                                                    na.rm = TRUE), 
                             Hatchling_Sex_Ratio_Median = rowMedians(hatchling_sex_ratio, 
                                                                     na.rm = TRUE), 
                             Hatchling_Sex_Ratio_Q25 = rowQuantiles(hatchling_sex_ratio, 
                                                                    prob = 0.25, 
                                                                    na.rm = TRUE), 
                             Hatchling_Sex_Ratio_Q75 = rowQuantiles(hatchling_sex_ratio, 
                                                                    prob = 0.75, 
                                                                    na.rm = TRUE),
                             Mature_Abundance_Median = rowMedians(mature_total, 
                                                                  na.rm = TRUE), 
                             Mature_Abundance_Q25 = rowQuantiles(mature_total, 
                                                                 prob = 0.25, 
                                                                 na.rm = TRUE), 
                             Mature_Abundance_Q75 = rowQuantiles(mature_total, 
                                                                 prob = 0.75, 
                                                                 na.rm = TRUE), 
                             Mature_Sex_Ratio_Median = rowMedians(OSR, 
                                                                  na.rm = TRUE), 
                             Mature_Sex_Ratio_Q25 = rowQuantiles(OSR, 
                                                                 prob = 0.25, 
                                                                 na.rm = TRUE), 
                             Mature_Sex_Ratio_Q75 = rowQuantiles(OSR, 
                                                                 prob = 0.75, 
                                                                 na.rm = TRUE)
        )
        
        # add in breeding success
        subset$Breeding_Success_Median <- pbeta(2 * subset$Mature_Sex_Ratio_Median, 
                                                shape1 = 1, 
                                                shape2 = betas[b])
        
        subset$Breeding_Success_Q25 <- pbeta(2 * subset$Mature_Sex_Ratio_Q25, 
                                             shape1 = 1, 
                                             shape2 = betas[b])
        
        subset$Breeding_Success_Q75 <- pbeta(2 * subset$Mature_Sex_Ratio_Q75, 
                                             shape1 = 1, 
                                             shape2 = betas[b])
        
        # tack subset onto SDF
        SDF <- rbind(SDF, subset)
        
        # update tracker
        print(paste(model, ' - ', scenarios[s], ' - beta ', betas[b], 
                    ' - all done!', sep = ''))
        print(paste('length SDF = ', nrow(SDF), sep = ''))
        
      } else {
        
        # add values to data frame
        subset <- data.frame(TRT = TRTs[t],
                             Scenario = scenarios[s], 
                             # Final_Temp = final_temps[s],
                             OSR = osrs[b], 
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
                             Mature_Sex_Ratio_Q75 = NA
        )
        
        # add in breeding success
        subset$Breeding_Success_Median <- NA
        subset$Breeding_Success_Q25 <- NA
        subset$Breeding_Success_Q75 <- NA
        
        # tack subset onto SDF
        SDF <- rbind(SDF, subset)
        
        # update tracker
        print(paste(model, ' - ', scenarios[s], ' - beta ', betas[b], 
                    ' - was all NA (and done)', sep = ''))
        print(paste('length SDF = ', nrow(SDF), sep = ''))
        
      }
      
    }
    
  }
  
}


# add in emergence success
SDF$Emergence_Success <- 0.86 / (1 + exp(1.7 * (SDF$Temperature - 32.7)))

# add in proportion of eggs that successfully hatch
SDF$Eggs_to_hatch_Median <- SDF$Emergence_Success * SDF$Breeding_Success_Median
SDF$Eggs_to_hatch_Q25 <- SDF$Emergence_Success * SDF$Breeding_Success_Q25
SDF$Eggs_to_hatch_Q75 <- SDF$Emergence_Success * SDF$Breeding_Success_Q75

example_outputs <- SDF

save(example_outputs, 
     file = '~/Projects/iliketurtles3/output/example_outputs.Rdata')

################################################################################

# load object
load("~/Projects/iliketurtles3/output/example_outputs.Rdata")

# make scenario and OSR a factor
example_outputs$Scenario <- factor(example_outputs$Scenario, 
                                   levels = as.factor(unique(example_outputs$Scenario)))
# example_outputs$Final_Temp <- factor(example_outputs$Final_Temp, 
#                                      levels = as.factor(unique(example_outputs$Final_Temp)))
example_outputs$OSR <- factor(example_outputs$OSR, 
                              levels = as.factor(unique(example_outputs$OSR)))
example_outputs$TRT <- factor(example_outputs$TRT, 
                              levels = as.factor(unique(example_outputs$TRT)))

# filter scenarios and OSRs to plot
examples_to_plot <- example_outputs %>%
  filter(OSR %in% c('0.1', '0.45')) %>%
  filter(Scenario %in% c('0.5C', '4.5C'))

##### helpful for plots
osr0.1_title <- 'minimum OSR 0.1 (steep mating function)'
osr0.45_title <- 'minimum OSR 0.45 (shallow mating function)'
annotation_x <- 90
annotation_size <- 4
annotation_label <- 0

###### temperature plot - min OSR = 0.1
temps_osr_0.1 <- ggplot(examples_to_plot, 
                        aes(x = Year, 
                            y = Temperature, 
                            col = Scenario, 
                            lty = TRT)) +
  ylab('Temperature (\u00B0C)') +
  ggtitle(osr0.1_title) +
  scale_linetype_discrete(name = 'Transitional Range \n of Temperatures \n Upper Limit', 
                          c(1, 2), 
                          guide = 'none') +
  scale_color_manual(values = c('#00BFC4', '#F8766D'), 
                     guide = 'none') +
  
  # pivotal temperature
  geom_hline(yintercept = 29.2, lwd = 1, lty = 3) +
  
  # upper TRT limit, wide TRT population
  geom_hline(yintercept = 34.4, lwd = 1, lty = 2) +
  
  # upper TRT limit, narrow TRT population
  geom_hline(yintercept = 31.4, lwd = 1, lty = 1) +
  
  # ideal temperature, wide TRT population
  geom_hline(yintercept = 34.84, lwd = 1, lty = 2, alpha = 0.4) +  
  
  # ideal temperature, narrow TRT population
  geom_hline(yintercept = 31.56, lwd = 1, lty = 1, alpha = 0.4) +
  
  # actual temperatures
  geom_path(lwd = 1)  +
  
  annotate("label", x = annotation_x, y = 32.5, label = "TI: 0.5 \u00B0C", 
           size = annotation_size, label.size = annotation_label) +
  
  annotate("label", x = annotation_x, y = 36.3, label = "TI: 4.5 \u00B0C", 
           size = annotation_size, label.size = annotation_label) +
  
  annotate("label", x = annotation_x, y = 34.4, label = "UL: Wide TRT", 
           size = annotation_size, label.size = annotation_label) +
  
  annotate("label", x = annotation_x, y = 31.2, label = "UL: Narrow TRT", 
           size = annotation_size, label.size = annotation_label) +
  
  annotate("label", x = annotation_x, y = 34.84, label = "IIT: Wide TRT", 
           size = annotation_size, label.size = annotation_label) +
  
  annotate("label", x = annotation_x, y = 31.76, label = "IIT: Narrow TRT", 
           size = annotation_size, label.size = annotation_label)

temps_osr_0.1

# temperature plot - min OSR = 0.45
temps_osr_0.45 <- ggplot(examples_to_plot, 
                         aes(x = Year, y = Temperature, 
                             col = Scenario, lty = TRT)) +
  ylab('Temperature (\u00B0C)') +
  ggtitle(osr0.45_title) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT')) +
  scale_color_manual(values = c('#00BFC4', '#F8766D'), 
                     labels = c('0.5 \u00B0C', '4.5 \u00B0C'))  +
  # guides(
  #   lty = guide_legend(
  #     title = 'Ideal \n Incubation \n Temperature (\u00B0C)', 
  #     override.aes = list(color = 'gray60'))) + 
  
  # pivotal temperature
  geom_hline(yintercept = 29.2, lwd = 1, lty = 3) +
  
  # upper TRT limit, wide TRT population
  geom_hline(yintercept = 34.4, lwd = 1, lty = 2) +
  
  # upper TRT limit, narrow TRT population
  geom_hline(yintercept = 31.4, lwd = 1, lty = 1) +
  
  # ideal temperature, wide TRT population
  geom_hline(yintercept = 31.28, lwd = 1, lty = 2, col = 'gray60') +  
  
  # ideal temperature, narrow TRT population
  geom_hline(yintercept = 30.07, lwd = 1, lty = 1, col = 'gray60') +
  
  # actual temperatures
  geom_path(lwd = 1)

temps_osr_0.45

##### emergence success (same between mating functions, only affected by temp)
emergence <- ggplot(examples_to_plot, 
                    aes(x = Year, 
                        y = Emergence_Success, 
                        col = Scenario)) +
  geom_line(lwd = 1) +
  ylab('Emergence Success') +
  scale_color_manual(values = c('#00BFC4', '#F8766D'))

emergence

# hatchling sex ratios, min OSR = 0.1, 
# only horizontal line affected by mating function
hatchling_sex_ratio_osr_0.1 <- examples_to_plot %>%
  filter(OSR == '0.1') %>%
  ggplot(aes(x = Year, 
             y = Hatchling_Sex_Ratio_Median, 
             col = Scenario, 
             lty = TRT)) +
  geom_hline(yintercept = 0.04, lwd = 1, lty = 4) +
  geom_ribbon(aes(ymin = Hatchling_Sex_Ratio_Q25,
                  ymax = Hatchling_Sex_Ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 1) +
  ggtitle(osr0.1_title) +
  ylab('Median Hatchling \n Sex Ratio') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

hatchling_sex_ratio_osr_0.1

# hatchling sex ratios, min OSR = 0.45 
# only horizontal line affected by mating function
hatchling_sex_ratio_osr_0.45 <- examples_to_plot %>%
  filter(OSR == '0.45') %>%
  ggplot(aes(x = Year, 
             y = Hatchling_Sex_Ratio_Median, 
             col = Scenario, 
             lty = TRT)) +
  geom_hline(yintercept = 0.237, lwd = 1, lty = 4) +
  geom_ribbon(aes(ymin = Hatchling_Sex_Ratio_Q25,
                  ymax = Hatchling_Sex_Ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 1) +
  ggtitle(osr0.45_title) +
  ylab('Median Hatchling \n Sex Ratio') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT')) +
  guides(color = guide_legend(title = "Final \n Incubation \n Temperature"))

hatchling_sex_ratio_osr_0.45

##### operational sex ratio, does depend on mating function, min = 0.1
OSR_min_osr_0.1 <- examples_to_plot %>%
  filter(OSR == '0.1') %>%
  ggplot(aes(x = Year, 
             y = Mature_Sex_Ratio_Median, 
             col = Final_Temp, 
             lty = TRT)) +
  geom_hline(yintercept = 0.1, lwd = 1, lty = 4) +
  geom_ribbon(aes(ymin = Mature_Sex_Ratio_Q25,
                  ymax = Mature_Sex_Ratio_Q75,
                  col = NULL,
                  fill = Final_Temp),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 1) +
  ggtitle(osr0.1_title) +
  ylab('Median Operational \n Sex Ratio') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

OSR_min_osr_0.1

##### operational sex ratio, does depend on mating function, min = 0.45
OSR_min_osr_0.45 <- examples_to_plot %>%
  filter(OSR == '0.45') %>%
  ggplot(aes(x = Year, 
             y = Mature_Sex_Ratio_Median, 
             col = Final_Temp, 
             lty = TRT)) +
  geom_hline(yintercept = 0.45, lwd = 1, lty = 4) +
  geom_ribbon(aes(ymin = Mature_Sex_Ratio_Q25,
                  ymax = Mature_Sex_Ratio_Q75,
                  col = NULL,
                  fill = Final_Temp),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 1) +
  ggtitle(osr0.45_title) +
  ylab('Median Operational \n Sex Ratio') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

OSR_min_osr_0.45

##### breeding success, does depend on mating function, min = 0.1
breeding_success_osr_0.1 <- examples_to_plot %>%
  filter(OSR == '0.1') %>%
  ggplot(aes(x = Year, 
             y = Breeding_Success_Median, 
             col = Final_Temp, 
             lty = TRT)) +
  geom_line(aes(x = Year, 
                y = Emergence_Success), 
            lwd = 1,
            alpha = 0.5, 
            lty = 4) +
  geom_ribbon(aes(ymin = Breeding_Success_Q25,
                  ymax = Breeding_Success_Q75,
                  col = NULL,
                  fill = Final_Temp),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 1) +
  ggtitle(osr0.1_title) +
  ylab('Median \n Breeding Success') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

breeding_success_osr_0.1

##### breeding success, does depend on mating function, min = 0.45
breeding_success_osr_0.45 <- examples_to_plot %>%
  filter(OSR == '0.45') %>%
  ggplot(aes(x = Year, 
             y = Breeding_Success_Median, 
             col = Final_Temp, 
             lty = TRT)) +
  geom_line(aes(x = Year, 
                y = Emergence_Success), 
            lwd = 1,
            alpha = 0.5, 
            lty = 4) +
  geom_ribbon(aes(ymin = Breeding_Success_Q25,
                  ymax = Breeding_Success_Q75,
                  col = NULL,
                  fill = Final_Temp),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 1) +
  ggtitle(osr0.45_title) +
  ylab('Median \n Breeding Success') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

breeding_success_osr_0.45

##### mature abundance, min OSR = 0.1
mature_abundance_osr_0.1 <- examples_to_plot %>%
  filter(OSR == '0.1') %>%
  ggplot(aes(x = Year, 
             y = Mature_Abundance_Median, 
             col = Final_Temp, 
             lty = TRT)) +
  geom_ribbon(aes(ymin = Mature_Abundance_Q25,
                  ymax = Mature_Abundance_Q75,
                  col = NULL,
                  fill = Final_Temp),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 1) +
  ggtitle(osr0.1_title) +
  ylab('Median \n Mature Abundance') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

mature_abundance_osr_0.1

##### mature abundance, min OSR = 0.45
mature_abundance_osr_0.45 <- examples_to_plot %>%
  filter(OSR == '0.45') %>%
  ggplot(aes(x = Year, 
             y = Mature_Abundance_Median, 
             col = Final_Temp, 
             lty = TRT)) +
  geom_ribbon(aes(ymin = Mature_Abundance_Q25,
                  ymax = Mature_Abundance_Q75,
                  col = NULL,
                  fill = Final_Temp),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 1) +
  ggtitle(osr0.45_title) +
  ylab('Median \n Mature Abundance') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

mature_abundance_osr_0.45

##### proportion of eggs that successfully hatch, min OSR = 0.1
# emergence success * breeding success, does depend on mating function
eggs_to_hatch_osr_0.1 <- examples_to_plot %>%
  filter(OSR == '0.1') %>%
  ggplot(aes(x = Year, 
             y = Eggs_to_hatch_Median, 
             col = Final_Temp, 
             lty = TRT)) +
  geom_line(aes(x = Year, 
                y = Emergence_Success), 
            lwd = 1,
            lty = 5) +
  geom_line(aes(x = Year, 
                y = Breeding_Success_Median), 
            lwd = 1, 
            alpha = 0.5) +
  geom_ribbon(aes(ymin = Eggs_to_hatch_Q25,
                  ymax = Eggs_to_hatch_Q75,
                  col = NULL,
                  fill = Final_Temp),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 3, alpha = 0.25) +
  ggtitle(osr0.1_title) +
  ylab('Median \n Eggs to Hatch') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

eggs_to_hatch_osr_0.1

##### proportion of eggs that successfully hatch, min OSR = 0.45
# emergence success * breeding success, does depend on mating function
eggs_to_hatch_osr_0.45 <- examples_to_plot %>%
  filter(OSR == '0.45') %>%
  ggplot(aes(x = Year, 
             y = Eggs_to_hatch_Median, 
             col = Final_Temp, 
             lty = TRT)) +
  geom_line(lwd = 1.5) +
  geom_ribbon(aes(ymin = Eggs_to_hatch_Q25,
                  ymax = Eggs_to_hatch_Q75,
                  col = NULL,
                  fill = Final_Temp),
              alpha = 0.25,
              show.legend = FALSE) +  
  geom_line(aes(x = Year,
                y = Emergence_Success),
            lwd = 0.75,
            lty = 5,
            alpha = 0.5) +
  geom_line(aes(x = Year, 
                y = Breeding_Success_Median), 
            lwd = 0.75,
            alpha = 0.5) +
  ggtitle(osr0.1_title) +
  ylab('Median \n Eggs to Hatch') +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  scale_linetype_discrete(name = 'Population', 
                          c(1, 2), 
                          labels = c('Wide TRT', 'Narrow TRT'))

eggs_to_hatch_osr_0.45

##### put it all together in one figure ########################################
A1 <- temps_osr_0.1 + 
  xlab('') +
  guides(color = 'none', lty = 'none')

A2 <- temps_osr_0.45 + 
  xlab('') +
  ylab('') +
  guides(color = 'none', lty = 'none')

B1 <- emergence +
  xlab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none')

B2 <- emergence +
  xlab('') +
  ylab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none')

C1 <- hatchling_sex_ratio_osr_0.1 +
  xlab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none')

C2 <- hatchling_sex_ratio_osr_0.45 +
  xlab('') +
  ylab('') +
  ggtitle('')

D1 <- OSR_min_osr_0.1 +
  xlab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none')

D2 <- OSR_min_osr_0.45 +
  xlab('') +
  ylab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none')

E1 <- breeding_success_osr_0.1 +
  xlab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none')

E2 <- breeding_success_osr_0.45 +
  xlab('') +
  ylab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none')

F1 <- mature_abundance_osr_0.1 +
  xlab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none') +
  ylim(0, 10000)

F2 <- mature_abundance_osr_0.45 +
  xlab('') +
  ylab('') +
  ggtitle('') +
  guides(color = 'none', lty = 'none')  +
  ylim(0, 10000)

# final_fig <- (A1 + A2) / (B1 + B2) / (C1 + C2) / (D1 + D2) / (E1 + E2) / (F1 + F2)
final_fig <- (A1 + A2) / (C1 + C2) / (D1 + D2) / (E1 + E2) / (F1 + F2) +
  plot_annotation(tag_levels = "A")
# plot_layout(heights = c(0, 1, 1, 1, 1)) +
# plot_layout(ncol = 1, nrow = 5, widths = c(7), heights = rep(3, 5))

final_fig

ggsave(final_fig, 
       file = '~/Projects/iliketurtles3/figures/sample_outputs.png', 
       height = 12, width = 10)

##### old figures ##############################################################

# # second axis calculations
# ylim_prim <- c(30.5, 35.5)
# ylim_sec <- c(0, 1)
# 
# b <- diff(ylim_prim) / diff(ylim_sec)
# a <- ylim_prim[1] - b*ylim_sec[1]
# 
# # plot figure - temperatures
# figAB1 <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%
#   ggplot(aes(x = Year, 
#              y = Temperature, 
#              color = Scenario 
#                    # lty = OSR
#                    )) + 
#   geom_line(lwd = 1) + 
#   geom_line(aes(x = Year, 
#                 y = a + Emergence_Success*b, 
#                 col = Scenario), 
#             lwd = 1, lty = 4) +
#   scale_y_continuous("Temperature and \n Emergence Success", 
#                      sec.axis = sec_axis(~ (. - a)/b, 
#                                          name = "")) +
#   theme_bw() +
#   theme(panel.border = element_blank()) +
#   theme(axis.line.y.right = element_line(linetype = 4, linewidth = 1), 
#         axis.line.y.left = element_line(linetype = 1, linewidth = 1), 
#         axis.line.x.bottom = element_line(linetype = 1, linewidth = 0.5, color = 'black'), 
#         axis.line.x = element_line(linetype = 1, linewidth = 0.5, color = 'black')) +
#   annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf) +
#   xlab('') +
#   ylab('temperature (\u00B0C)') +
#   guides(color = 'none')
# 
# figAB1
# 
# # plot figure - temperatures
# figAB2 <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%
#   ggplot(aes(x = Year, 
#              y = Temperature, 
#              color = Scenario 
#              # lty = OSR
#   )) + 
#   geom_line(lwd = 1) + 
#   geom_line(aes(x = Year, 
#                 y = a + Emergence_Success*b, 
#                 col = Scenario), 
#             lwd = 1, lty = 4) +
#   scale_y_continuous("", 
#                      sec.axis = sec_axis(~ (. - a)/b, 
#                                          name = "")) +
#   theme_bw() +
#   theme(panel.border = element_blank()) +
#   theme(axis.line.y.right = element_line(linetype = 4, linewidth = 1), 
#         axis.line.y.left = element_line(linetype = 1, linewidth = 1), 
#         axis.line.x.bottom = element_line(linetype = 1, linewidth = 0.5, color = 'black'), 
#         axis.line.x = element_line(linetype = 1, linewidth = 0.5, color = 'black')) +
#   annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf) +
#   xlab('') +
#   ylab('') +
#   theme(plot.margin = unit(c(0, 0, 0, -2), 'cm'))
# 
# figAB2

# # plot figure - just temperatures
# figA1 <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%
#   ggplot(aes(x = Year, 
#              y = Temperature, 
#              color = Scenario 
#              # lty = OSR
#   )) + 
#   geom_line(lwd = 1) + 
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   theme_bw() +
#   xlab('') +
#   ylab('\n temperature (\u00B0C)') +
#   guides(color = 'none')
# 
# figA1

# # plot figure - just temperatures
# figA2 <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%
#   ggplot(aes(x = Year, 
#              y = Temperature, 
#              color = Scenario 
#              # lty = OSR
#   )) + 
#   geom_line(lwd = 1) + 
#   theme_bw() +
#   xlab('') +
#   ylab('')
# 
# figA2

# # plot figure - just emergence success
# figB1 <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%
#   ggplot(aes(x = Year, 
#              y = Emergence_Success, 
#              color = Scenario 
#              # lty = OSR
#   )) + 
#   geom_line(lwd = 1) + 
#   theme_bw() +
#   xlab('') +
#   ylab('Emergence \n Success') +
#   guides(color = 'none')
# 
# figB1

# # plot figure - just emergence success
# figB2 <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%
#   ggplot(aes(x = Year, 
#              y = Emergence_Success, 
#              color = Scenario 
#              # lty = OSR
#   )) + 
#   geom_line(lwd = 1) + 
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   theme_bw() +
#   xlab('') +
#   ylab('emergence \n success') +
#   theme(plot.margin = unit(c(0, 0, 0, -1.5), 'cm'))
# 
# figB2
# 
# # plot figure - hatchling sex ratios, P base
# figC <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%
#   filter(Age == 'Hatchling') %>%
#   filter(TRT == 'Narrow') %>%
#   ggplot(aes(x = Year, 
#              y = Sex_Ratio_Median, 
#              color = Scenario, 
#              lty = OSR
#   )) +
#   # geom_hline(yintercept = 0.01) +
#   scale_linewidth_manual(values = c(1, 2)) +
#   geom_ribbon(aes(ymin = Sex_Ratio_Q25,
#                   ymax = Sex_Ratio_Q75,
#                   col = NULL,
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_path(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
#   xlab('') +
#   theme_bw() +
#   ylim(c(0, 0.41)) +
#   guides(color = "none", 
#          lty = 'none') +
#   ylab('median \n hatchling sex ratio')
# 
# figC
# 
# # plot figure - hatchling sex ratios, GM base
# figD <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%  
#   filter(Age == 'Hatchling') %>%
#   filter(TRT == 'Wide') %>%
#   filter(Year > 1) %>%
#   ggplot(aes(x = Year, 
#              y = Sex_Ratio_Median, 
#              color = Scenario, 
#              lty = OSR
#   )) +
#   # geom_hline(yintercept = 0.01) +
#   scale_linewidth_manual(values = c(1, 2)) +
#   geom_ribbon(aes(ymin = Sex_Ratio_Q25,
#                   ymax = Sex_Ratio_Q75,
#                   col = NULL,
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_path(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
#   xlab('') +
#   theme_bw() +
#   ylim(c(0, 0.41)) +
#   guides(color = "none") +
#   ylab('')
# 
# figD
# 
# # plot figure - mature sex ratios, P base
# figE <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%  
#   filter(Age == 'Mature') %>%
#   filter(TRT == 'Narrow') %>%
#   ggplot(aes(x = Year, 
#              y = Sex_Ratio_Median, 
#              color = Scenario, 
#              lty = OSR
#   )) +
#   # geom_hline(yintercept = 0.01) +
#   scale_linewidth_manual(values = c(1, 2)) +
#   geom_ribbon(aes(ymin = Sex_Ratio_Q25,
#                   ymax = Sex_Ratio_Q75,
#                   col = NULL,
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_path(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
#   xlab('') +
#   theme_bw() +
#   ylim(c(0, 0.7)) +
#   guides(color = "none", lty = 'none') +
#   ylab('median \n operational sex ratio') 
# 
# figE
# 
# # plot figure - mature sex ratios, GM base
# figF <- examples_to_plot %>%
#   filter(Abundance_Median > 0) %>%  
#   filter(Age == 'Mature') %>%
#   filter(TRT == 'Wide') %>%
#   ggplot(aes(x = Year, 
#              y = Sex_Ratio_Median, 
#              color = Scenario, 
#              lty = OSR
#   )) +
#   # geom_hline(yintercept = 0.01) +
#   scale_linewidth_manual(values = c(1, 2)) +
#   geom_ribbon(aes(ymin = Sex_Ratio_Q25,
#                   ymax = Sex_Ratio_Q75,
#                   col = NULL,
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_path(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
#   xlab('') +
#   theme_bw() +
#   ylim(c(0, 0.7)) +
#   guides(color = "none", lty = 'none') +
#   ylab('') 
# 
# figF
# 
# # plot figure - mature abundances, P base
# figG <- examples_to_plot %>%
#   filter(Age == 'Mature') %>%
#   filter(Abundance_Median > 0) %>%
#   filter(TRT == 'Narrow') %>%
#   ggplot(aes(x = Year, 
#              y = Abundance_Median, 
#              color = Scenario, 
#              lty = OSR)) + 
#   geom_ribbon(aes(ymin = Abundance_Q25,
#                   ymax = Abundance_Q75, 
#                   col = NULL, 
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_line(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
#   xlab('Year') +
#   ylab('median \n mature abundance') +
#   ylim(c(0, 10500)) +
#   theme_bw() +
#   guides(color = "none", 
#          lty = "none")
# 
# figG
# 
# # plot figure - mature abundances, GM base
# figH <- examples_to_plot %>%
#   filter(Age == 'Mature') %>%
#   filter(Abundance_Median > 0) %>%
#   filter(TRT == 'Wide') %>%
#   ggplot(aes(x = Year, 
#              y = Abundance_Median, 
#              color = Scenario, 
#              lty = OSR)) + 
#   geom_ribbon(aes(ymin = Abundance_Q25,
#                   ymax = Abundance_Q75, 
#                   col = NULL, 
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_line(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
#   xlab('Year') +
#   ylab('') +
#   ylim(c(0, 10500)) +
#   theme_bw() +
#   guides(color = "none", 
#          lty = "none")
# 
# # plot figure - hatchling abundances, P base
# figI <- examples_to_plot %>%
#   filter(Age == 'Hatchling') %>%
#   filter(Abundance_Median > 0) %>%
#   filter(TRT == 'Narrow') %>%
#   ggplot(aes(x = Year, 
#              y = Abundance_Median, 
#              color = Scenario, 
#              lty = OSR)) + 
#   geom_ribbon(aes(ymin = Abundance_Q25,
#                   ymax = Abundance_Q75, 
#                   col = NULL, 
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_line(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
#   xlab('Year') +
#   ylab('median \n mature abundance') +
#   # ylim(c(0, 10500)) +
#   theme_bw() +
#   guides(color = "none", 
#          lty = "none")
# 
# figI
# 
# # plot figure - hatchling abundances, GM base
# figJ <- examples_to_plot %>%
#   filter(Age == 'Hatchling') %>%
#   filter(Abundance_Median > 0) %>%
#   filter(TRT == 'Wide') %>%
#   ggplot(aes(x = Year, 
#              y = Abundance_Median, 
#              color = Scenario, 
#              lty = OSR)) + 
#   geom_ribbon(aes(ymin = Abundance_Q25,
#                   ymax = Abundance_Q75, 
#                   col = NULL, 
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_line(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
#   xlab('Year') +
#   ylab('') +
#   # ylim(c(0, 10500)) +
#   theme_bw() +
#   guides(color = "none", 
#          lty = "none")
# 
# figJ
# 
# # figA/B: temperatures and emergence success
# # figB: emergence success
# # figC: hatchling sex ratios (P base)
# # figD: hatchling sex ratios (GM base)
# # figI: hatchling abundance (P base)
# # figJ: hatchling abundance (GM base)
# # figE: mature sex ratios (P base)
# # figF: mature sex ratios (GM base)
# # figG: mature abundance (P base)
# # figH: mature abundance (GM base)
# 
# 
# 
# option1 <- (figA1 + figB2) / (figC + figD) / (figI + figJ) / (figE + figF) / (figG + figH) +
#   plot_layout(heights = c(1, 1, 1, 1, 1)) +
#   plot_annotation(tag_levels = "A")
# 
# option1

# option2 <- (figA1 + figA2) / (figB1 + figB2) / (figC + figD) / (figE + figF) / (figG + figH) +
#   plot_layout(heights = c(-1, -1, -1, -1, -1)) +
#   plot_annotation(tag_levels = "A")
# 
# option2
# 
# option3 <- (figAB1 + figAB2) / (figC + figD) / (figE + figF) / (figG + figH) +
#   # plot_layout(heights = c(0, -1, -1, -1, -1)) +
#   plot_annotation(tag_levels = "A") +
#   plot_layout(ncol = 1, nrow = 4, widths = c(7), heights = c(2, 2, 2, 2))
# 
# option3

# # plot figure - hatchling abundances
# figB <- examples_to_plot %>%
#   filter(Age == 'Hatchling') %>%
#   filter(Abundance_Median > 0) %>%  
#   ggplot(aes(x = Year, 
#              y = Abundance_Median, 
#              color = Scenario, 
#              lty = OSR)) + 
#   geom_ribbon(aes(ymin = Abundance_Q25,
#                   ymax = Abundance_Q75, 
#                   col = NULL, 
#                   fill = Scenario),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_path(lwd = 1) +
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +  
#   xlab('') +
#   ylab('median \n hatchling abundance') +
#   theme_bw()

# # plot figure - emergence success
# figE <- ggplot(data = subset(examples_to_plot, 
#                              OSR == '0.1' & Abundance_Median > 0), 
#                aes(x = Year, 
#                    y = Emergence, 
#                    color = Scenario, 
#                    lty = OSR)) + 
#   geom_line(lwd = 1) + 
#   geom_line(data = subset(samples_to_plot, 
#                           OSR == 0.45 & Abundance_Median > 0), 
#             lwd = 1, 
#             position = position_nudge(y = -0.015)) +  
#   scale_color_manual(values = c('#00BFC4', '#F8766D')) +
#   scale_fill_manual(values = c('#00BFC4', '#F8766D')) +  
#   xlab('Year') +
#   theme_bw() +
#   guides(color = "none", 
#          lty = "none") +
#   ylab('hatchling \n emergence success')



# final_fig <- option3
# 
# # save to file
# ggsave(plot = final_fig,
#        filename = paste('sample_outputs.png', sep = ''),
#        path = '~/Projects/iliketurtles3/figures/',
#        width = 8.5, height = 11)
# 
# # plot figure - hatchling sex ratios across all temps and OSRs
# figF <- sample_plot_values %>%
#   filter(Abundance_Median > 0) %>%  
#   filter(Age == 'Hatchling') %>%
#   ggplot(aes(x = Year, 
#              y = Sex_Ratio_Median, 
#              color = Scenario, 
#              lty = OSR 
#   )) + 
#   geom_hline(yintercept = 0.01) +
#   scale_linewidth_manual(values = c(1, 2)) +
#   geom_path() +
#   xlab('Year') +
#   theme_bw() +
#   ylim(c(0, 0.03)) +
#   ylab('Median Hatchling Sex Ratio') +
#   theme(legend.box = "horizontal")
# 
# 
# # save to file
# ggsave(plot = figF,
#        filename = paste('hatchling_sex_ratios.png', sep = ''),
#        path = '~/Projects/iliketurtles3/figures/',
#        width = 8, height = 4)
