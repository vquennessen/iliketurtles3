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
ages <- c('Mature', 'Hatchling', 'Mature', 'Hatchling')
values <- c('Abundance', 'Abundance', 'Sex Ratio', 'Sex Ratio')

# scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')

# osrs
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
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
                  Abundance_Median = NULL, 
                  Abundance_Q25 = NULL, 
                  Abundance_Q75 = NULL, 
                  Sex_Ratio_Median = NULL, 
                  Sex_Ratio_Q25 = NULL, 
                  Sex_Ratio_Q75 = NULL
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
    
    for (a in 1:2) {
      
      if (a == 1) {
        
        abundances <- hatchlings_total
        sex_ratio <- hatchling_sex_ratio
        name <- 'Hatchling'
        
      } else {
        
        abundances <- mature_total
        sex_ratio <- OSR
        name <- 'Mature'
        
      }
      
      subset <- data.frame(Scenario = scenarios[s], 
                           OSR = osrs[b], 
                           Combo = paste(scenarios[s], ' - OSR ', osrs[b], 
                                         sep = ''),
                           Year = 1:100,
                           Temperature = temps,
                           Age = name,
                           Abundance_Median = rowMedians(abundances, 
                                                         na.rm = TRUE), 
                           Abundance_Q25 = rowQuantiles(abundances, 
                                              prob = 0.25, 
                                              na.rm = TRUE), 
                           Abundance_Q75 = rowQuantiles(abundances, 
                                              prob = 0.75, 
                                              na.rm = TRUE), 
                           Sex_Ratio_Median = rowMedians(sex_ratio, 
                                                         na.rm = TRUE), 
                           Sex_Ratio_Q25 = rowQuantiles(sex_ratio, 
                                                        prob = 0.25, 
                                                        na.rm = TRUE), 
                           Sex_Ratio_Q75 = rowQuantiles(sex_ratio, 
                                                        prob = 0.75, 
                                                        na.rm = TRUE)
      )
      
      # tack subset onto SDF
      SDF <- rbind(SDF, subset)
      
      # update tracker
      print(paste(scenarios[s], beta_names[b], name, 'all done!', sep = ' - '))
      print(paste('length SDF = ', nrow(SDF), sep = ''))
      
    }
    
  }
  
}

sample_plot_values <- SDF

# add in emergence success
sample_plot_values$Emergence_Success <- 
  0.86 / (1 + exp(1.7 * (sample_plot_values$Temperature - 32.7)))

save(sample_plot_values, 
     file = '~/Projects/iliketurtles3/output/sample_plot_values.Rdata')

################################################################################

# load object
load("~/Projects/iliketurtles3/output/sample_plot_values.Rdata")

# make scenario and OSR a factor
sample_plot_values$Scenario <- factor(sample_plot_values$Scenario, 
                                      levels = as.factor(scenarios))
sample_plot_values$OSR <- factor(sample_plot_values$OSR, 
                                 levels = as.factor(osrs))

# filter scenarios and OSRs to plot
samples_to_plot <- sample_plot_values %>%
  filter(OSR %in% c('0.1', '0.45')) %>%
  filter(Scenario %in% c('0.5C', '3.5C'))

# plot figure - mature abundances
figA <- samples_to_plot %>%
  filter(Age == 'Mature') %>%
  filter(Abundance_Median > 0) %>%
  ggplot(aes(x = Year, 
             y = Abundance_Median, 
             color = Scenario, 
             lty = OSR)) + 
  geom_ribbon(aes(ymin = Abundance_Q25,
                  ymax = Abundance_Q75, 
                  col = NULL, 
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_line(lwd = 2) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('') +
  ylab('median \n mature abundance') +
  theme_bw() +
  guides(color = "none", 
         lty = "none")

# plot figure - hatchling abundances
figB <- samples_to_plot %>%
  filter(Age == 'Hatchling') %>%
  filter(Abundance_Median > 0) %>%  
  ggplot(aes(x = Year, 
             y = Abundance_Median, 
             color = Scenario, 
             lty = OSR)) + 
  geom_ribbon(aes(ymin = Abundance_Q25,
                  ymax = Abundance_Q75, 
                  col = NULL, 
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +  
  xlab('') +
  ylab('median \n hatchling abundance') +
  theme_bw()

# plot figure - sex ratios
figC <- samples_to_plot %>%
  filter(Abundance_Median > 0) %>%  
  filter(Age == 'Hatchling') %>%
  ggplot(aes(x = Year, 
             y = Sex_Ratio_Median, 
             color = Scenario, 
             lty = OSR, 
             lwd = Age
             )) + 
  geom_hline(yintercept = 0.01) +
  scale_linewidth_manual(values = c(1, 2)) +
  geom_ribbon(aes(ymin = Sex_Ratio_Q25,
                  ymax = Sex_Ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path() +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('Year') +
  theme_bw() +
  ylim(c(0, 0.05)) +
  guides(color = "none",
         lty = "none") +
  ylab('median sex ratio')

# plot figure - temperatures
figD <- ggplot(data = subset(samples_to_plot, 
                             OSR == '0.1' & Abundance_Median > 0), 
               aes(x = Year, 
                   y = Temperature, 
                   color = Scenario, 
                   lty = OSR)) + 
  geom_line(lwd = 1) + 
  geom_line(data = subset(samples_to_plot, 
                          OSR == 0.45 & Abundance_Median > 0), 
            lwd = 1, 
            position = position_nudge(y = -0.05)) +  
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +  
  xlab('Year') +
  theme_bw() +
  guides(color = "none", 
         lty = "none") +
  ylab('temperature (\u00B0C)')

# plot figure - emergence success
figE <- ggplot(data = subset(samples_to_plot, 
                             OSR == '0.1' & Abundance_Median > 0), 
               aes(x = Year, 
                   y = Emergence, 
                   color = Scenario, 
                   lty = OSR)) + 
  geom_line(lwd = 1) + 
  geom_line(data = subset(samples_to_plot, 
                          OSR == 0.45 & Abundance_Median > 0), 
            lwd = 1, 
            position = position_nudge(y = -0.015)) +  
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +  
  xlab('Year') +
  theme_bw() +
  guides(color = "none", 
         lty = "none") +
  ylab('hatchling \n emergence success')

# figA: median mature abundance
# figB: median hatchling abundance
# figC: sex ratios
# figD: temperatures
# figE: emergence success

final_fig <- figD/figE/figB/figC/figA +
  plot_layout(heights = c(-1, -1, -1, -1, -1)) +
  plot_annotation(tag_levels = "A")
  
final_fig

# save to file
ggsave(plot = final_fig,
       filename = paste('abundance_sexratios_temps.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8.5, height = 12)

# plot figure - hatchling sex ratios across all temps and OSRs
figF <- sample_plot_values %>%
  filter(Abundance_Median > 0) %>%  
  filter(Age == 'Hatchling') %>%
  ggplot(aes(x = Year, 
             y = Sex_Ratio_Median, 
             color = Scenario, 
             lty = OSR 
  )) + 
  geom_hline(yintercept = 0.01) +
  scale_linewidth_manual(values = c(1, 2)) +
  geom_path() +
  xlab('Year') +
  theme_bw() +
  ylim(c(0, 0.03)) +
  ylab('Median Hatchling Sex Ratio') +
  theme(legend.box = "horizontal")
  

# save to file
ggsave(plot = figF,
       filename = paste('hatchling_sex_ratios.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 4)
