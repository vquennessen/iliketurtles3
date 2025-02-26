# get vlines for plotting 10 yr median lambdas over time + IQR

# empty environment
rm(list = ls())

# load libraries
library(ggplot2)
library(matrixStats)
library(readr)

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

##### plotting parameters ######################################################

# category titles
ages <- c('Mature', 'Hatchling', 'Mature', 'Hatchling')
values <- c('Abundance', 'Abundance', 'Sex Ratio', 'Sex Ratio')

# scenarios
scenarios <- c('0.5C', '3.5C')

# osrs
osrs <- c(0.1, 0.45)
betas <- OSRs_to_betas(osrs)
beta_names <- paste('beta', betas, sep = '')

# maturity ogive
max_age <- 85
age_maturity_mu <- 25
age_maturity_sd <- 2.5
M <- pnorm(q = 1:max_age, mean = age_maturity_mu, sd = age_maturity_sd)

##### create objects ###########################################################

# initialize super data frame (SDF)
SDF <- data.frame(Scenario = NULL, 
                  OSR = NULL,
                  Combo = NULL,
                  Year = NULL,
                  Temperature = NULL,
                  Age = NULL,
                  Value = NULL,
                  Median = NULL, 
                  Q25 = NULL, 
                  Q75 = NULL
)

# for each scenario
for (s in 1:length(scenarios)) {
  
  temp_increases <- parse_number(scenarios)
  
  temps <- seq(from = 31.8, to = 31.8 + temp_increases[s], length = 100)
  
  # for each osr
  for (b in 1:length(betas)) {
    
    # load in sims N
    load(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/', 
               'model output/temp_stochasticity/P_base/', 
               scenarios[s], '/', beta_names[b], '/10000_N.Rda', 
               sep = ''))
    
    # load in sims OSR
    load(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/', 
               'model output/temp_stochasticity/P_base/', 
               scenarios[s], '/', beta_names[b], '/10000_OSR.Rda', 
               sep = ''))
    
    # extract hatchling F
    hatchlings_F <- sims_N[1, 1, , ]
    
    # extract hatchling M
    hatchlings_M <- sims_N[2, 1, , ]
    
    # hathclings total
    hatchlings_total <- hatchlings_F + hatchlings_M
    
    # extract hatchling sex ratios, remove NaNs
    hatchling_sex_ratio <- hatchlings_M / (hatchlings_F + hatchlings_M)
    hatchling_sex_ratio[!is.finite(hatchling_sex_ratio)] <- NA
    
    # extract mature F
    mature_F <- colSums(sims_N[1, , , ]*M, dims = 1)
    
    # extract mature M
    mature_M <- colSums(sims_N[2, , , ]*M, dims = 1)
    
    # mature total
    mature_total <- mature_F + mature_M
    
    # mature sex ratio, remove infinite values
    OSR <- sims_OSR
    OSR[!is.finite(OSR)] <- NA
    
    # remove NaNs

    # objects
    things <- list(mature_total, hatchlings_total, 
                       OSR, hatchling_sex_ratio)
    
    for (t in 1:length(things)) {
      
      subset <- data.frame(Scenario = scenarios[s], 
                           OSR = osrs[b], 
                           Combo = paste(scenarios[s], ' - OSR ', osrs[b], 
                                         sep = ''),
                           Year = 1:100,
                           Temperature = temps,
                           Age = ages[t],
                           Value = values[t],
                           Median = rowMedians(things[[t]], na.rm = TRUE), 
                           Q25 = rowQuantiles(things[[t]], 
                                              prob = 0.25, 
                                              na.rm = TRUE), 
                           Q75 = rowQuantiles(things[[t]], 
                                              prob = 0.75, 
                                              na.rm = TRUE)
      )
      
      # tack subset onto SDF
      SDF <- rbind(SDF, subset)
      
      # update tracker
      print(paste(scenarios[s], beta_names[b], ages[t], values[t], 
                  'all done!', sep = ' - '))
      print(paste('length SDF = ', nrow(SDF), sep = ''))
      
    }
    
  }
  
}

median_abundances_to_plot_over_time <- SDF

save(median_abundances_to_plot_over_time, 
     file = '~/Projects/iliketurtles3/output/median_abundances_to_plot_over_time.Rdata')

# load object
load("~/Projects/iliketurtles3/output/median_abundances_to_plot_over_time.Rdata")

# make scenario a factor
median_abundances_to_plot_over_time$Scenario <- 
  factor(median_abundances_to_plot_over_time$Scenario)

mature <- subset(median_abundances_to_plot_over_time, 
                 Age == 'Mature')

hatchling <- subset(median_abundances_to_plot_over_time, 
                 Age == 'Hatchling')

sex_ratios <- subset(median_abundances_to_plot_over_time, 
                     Value == 'Sex Ratio')

# plot figure - mature abundances
figA <- ggplot(data = mature, 
                aes(x = Year, 
                    y = Median, 
                    color = Combo)) + 
  geom_ribbon(aes(ymin = Q25,
                  ymax = Q75, 
                  col = NULL, 
                  fill = Combo),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  # scale_color_manual(values = c('#F8766D', '#00BFC4')) + 
  xlab('Year') +
  ylab('Abundance') +
  ggtitle('median mature abundances over time + IQR') +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13))

# plot figure - hatchling abundances
figB <- ggplot(data = hatchling, 
               aes(x = Year, 
                   y = Median, 
                   color = Combo)) + 
  geom_ribbon(aes(ymin = Q25,
                  ymax = Q75, 
                  col = NULL, 
                  fill = Combo),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  # scale_color_manual(values = c('#F8766D', '#00BFC4')) + 
  xlab('Year') +
  ylab('Abundance') +
  ggtitle('median hatchling abundances over time + IQR') +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13))

# plot figure - sex ratios
figC <- ggplot(data = sex_ratios, 
               aes(x = Year, 
                   y = Median, 
                   color = Combo, 
                   lty = Age)) + 
  geom_ribbon(aes(ymin = Q25,
                  ymax = Q75, 
                  col = NULL, 
                  fill = Combo),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  # scale_color_manual(values = c('#F8766D', '#00BFC4')) + 
  xlab('Year') +
  ylab('Abundance') +
  ggtitle('median sex ratios over time + IQR') +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13))

# plot figure - temperatures
figD <- ggplot(data = median_abundances_to_plot_over_time, 
               aes(x = Year, 
                   y = Temperature, 
                   color = Combo)) + 
  geom_ribbon(aes(ymin = Q25,
                  ymax = Q75, 
                  col = NULL, 
                  fill = Combo),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  # scale_color_manual(values = c('#F8766D', '#00BFC4')) + 
  xlab('Year') +
  ylab('Abundance') +
  ggtitle('temperatures over time + IQR') +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13))

# # save to file
# ggsave(plot = figA, 
#        filename = paste('median_mature_abundances.png', sep = ''),
#        path = '~/Projects/iliketurtles3/figures/',
#        width = 9, height = 8)

# # plot figure - hatchling abundances
# figB <- ggplot(data = hatchling, 
#                aes(x = Year, 
#                    y = Median, 
#                    color = Abundance)) + 
#   facet_grid(cols = vars(OSR), rows = vars(Scenario)) +
#   geom_ribbon(aes(ymin = Q25,
#                   ymax = Q75, 
#                   col = NULL, 
#                   fill = Abundance),
#               alpha = 0.25,
#               show.legend = FALSE) +
#   geom_path(lwd = 1) +
#   # scale_color_manual(values = c('#F8766D', '#00BFC4')) + 
#   xlab('Year') +
#   ylab('Lambda') +
#   ggtitle('temperature stochasticity; median hatchling abundances over time + IQR') +
#   theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
#   theme(axis.title.x = element_text(size = 13, vjust = -3)) +
#   theme(axis.title.y = element_text(size = 13, vjust = 4)) +
#   theme(axis.text = element_text(size = 10)) +
#   theme(strip.text = element_text(size = 12)) +
#   theme(title = element_text(size = 13))
# 
# # save to file
# ggsave(plot = figB, 
#        filename = paste('median_hatchling_abundances.png', sep = ''),
#        path = '~/Projects/iliketurtles3/figures/',
#        width = 9, height = 8)
