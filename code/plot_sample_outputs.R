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
folder <- '2025_05_20_temp_var_clutch_level'
nsims <- 1000
desktop <- FALSE
user <- ifelse(desktop == TRUE, 'Vic', 'vique')

# scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')

# osrs
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
betas <- OSRs_to_betas(osrs)
# betas <- c(43.71, 20.64, 12.92, 9.02, 6.65, 5.03, 3.83, 2.87, 2.01, 1)

beta_names <- paste('beta', betas, sep = '')

# maturity ogive
max_age <- 85
age_maturity_mu <- 25
age_maturity_sd <- 2.5
M <- pnorm(q = 1:max_age, mean = age_maturity_mu, sd = age_maturity_sd)

# temperatures
temp_mu <- 30.5

##### create objects ###########################################################

# initialize super data frame (SDF)
SDF <- data.frame(TRT = NULL,
                  Scenario = NULL, 
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

# for each population / TRT
for (t in 1:length(TRTs)) {
  
  if (TRTs[t] == 'Narrow') { model <- 'P_base' }
  if (TRTs[t] == 'Wide') { model <- 'GM_base' }
  
  # for each scenario
  for (s in 1:length(scenarios)) {
    
    temp_increases <- parse_number(scenarios)
    
    temps <- seq(from = temp_mu, to = temp_mu + temp_increases[s], length = 100)
    
    # for each osr
    for (b in 1:length(betas)) {
      
      # load in sims N
      load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/', 
                 'model output/', folder, '/', model, '/', 
                 scenarios[s], '/', beta_names[b], '/', nsims, '_N.Rda', 
                 sep = ''))
      
      # load in sims OSR
      load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/', 
                 'model output/', folder, '/', model, '/', 
                 scenarios[s], '/', beta_names[b], '/', nsims, '_OSR.Rda', 
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
      
      for (a in 1:length(ages)) {
        
        if (ages[a] == 'Hatchling') {
          
          abundances <- hatchlings_total
          sex_ratio <- hatchling_sex_ratio
          
        } else {
          
          abundances <- mature_total
          sex_ratio <- OSR
          
        }
        
        subset <- data.frame(TRT = TRTs[t],
                             Scenario = scenarios[s], 
                             OSR = osrs[b], 
                             Combo = paste(scenarios[s], ' - OSR ', osrs[b], 
                                           sep = ''),
                             Year = 1:100,
                             Temperature = temps,
                             Age = ages[a],
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
        print(paste(model, ' - ', scenarios[s], 'beta ', betas[b], ages[a], 'all done!', 
                    sep = ' - '))
        print(paste('length SDF = ', nrow(SDF), sep = ''))
        
      }
      
    }
    
  }
  
}

example_outputs <- SDF

# add in emergence success
example_outputs$Emergence_Success <-
  0.86 / (1 + exp(1.7 * (example_outputs$Temperature - 32.7)))

save(example_outputs, 
     file = '~/Projects/iliketurtles3/output/example_outputs.Rdata')

################################################################################

# load object
load("~/Projects/iliketurtles3/output/example_outputs.Rdata")

# make scenario and OSR a factor
example_outputs$Scenario <- factor(example_outputs$Scenario, 
                                   levels = as.factor(scenarios))
example_outputs$OSR <- factor(example_outputs$OSR, 
                              levels = as.factor(osrs))

# filter scenarios and OSRs to plot
examples_to_plot <- example_outputs %>%
  filter(OSR %in% c('0.1', '0.45')) %>%
  filter(Scenario %in% c('0.5C', '3.5C'))

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

# plot figure - just temperatures
figA1 <- examples_to_plot %>%
  filter(Abundance_Median > 0) %>%
  ggplot(aes(x = Year, 
             y = Temperature, 
             color = Scenario 
             # lty = OSR
  )) + 
  geom_line(lwd = 1) + 
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  theme_bw() +
  xlab('') +
  ylab('\n temperature (\u00B0C)') +
  guides(color = 'none')
  
figA1

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

# plot figure - just emergence success
figB2 <- examples_to_plot %>%
  filter(Abundance_Median > 0) %>%
  ggplot(aes(x = Year, 
             y = Emergence_Success, 
             color = Scenario 
             # lty = OSR
  )) + 
  geom_line(lwd = 1) + 
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  theme_bw() +
  xlab('') +
  ylab('emergence \n success') +
  theme(plot.margin = unit(c(0, 0, 0, -1.5), 'cm'))

figB2

# plot figure - hatchling sex ratios, P base
figC <- examples_to_plot %>%
  filter(Abundance_Median > 0) %>%
  filter(Age == 'Hatchling') %>%
  filter(TRT == 'Narrow') %>%
  ggplot(aes(x = Year, 
             y = Sex_Ratio_Median, 
             color = Scenario, 
             lty = OSR
  )) +
  # geom_hline(yintercept = 0.01) +
  scale_linewidth_manual(values = c(1, 2)) +
  geom_ribbon(aes(ymin = Sex_Ratio_Q25,
                  ymax = Sex_Ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('') +
  theme_bw() +
  ylim(c(0, 0.41)) +
  guides(color = "none", 
         lty = 'none') +
  ylab('median \n hatchling sex ratio')

figC

# plot figure - hatchling sex ratios, GM base
figD <- examples_to_plot %>%
  filter(Abundance_Median > 0) %>%  
  filter(Age == 'Hatchling') %>%
  filter(TRT == 'Wide') %>%
  filter(Year > 1) %>%
  ggplot(aes(x = Year, 
             y = Sex_Ratio_Median, 
             color = Scenario, 
             lty = OSR
  )) +
  # geom_hline(yintercept = 0.01) +
  scale_linewidth_manual(values = c(1, 2)) +
  geom_ribbon(aes(ymin = Sex_Ratio_Q25,
                  ymax = Sex_Ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('') +
  theme_bw() +
  ylim(c(0, 0.41)) +
  guides(color = "none") +
  ylab('')

figD

# plot figure - mature sex ratios, P base
figE <- examples_to_plot %>%
  filter(Abundance_Median > 0) %>%  
  filter(Age == 'Mature') %>%
  filter(TRT == 'Narrow') %>%
  ggplot(aes(x = Year, 
             y = Sex_Ratio_Median, 
             color = Scenario, 
             lty = OSR
  )) +
  # geom_hline(yintercept = 0.01) +
  scale_linewidth_manual(values = c(1, 2)) +
  geom_ribbon(aes(ymin = Sex_Ratio_Q25,
                  ymax = Sex_Ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('') +
  theme_bw() +
  ylim(c(0, 0.7)) +
  guides(color = "none", lty = 'none') +
  ylab('median \n operational sex ratio') 

figE

# plot figure - mature sex ratios, GM base
figF <- examples_to_plot %>%
  filter(Abundance_Median > 0) %>%  
  filter(Age == 'Mature') %>%
  filter(TRT == 'Wide') %>%
  ggplot(aes(x = Year, 
             y = Sex_Ratio_Median, 
             color = Scenario, 
             lty = OSR
  )) +
  # geom_hline(yintercept = 0.01) +
  scale_linewidth_manual(values = c(1, 2)) +
  geom_ribbon(aes(ymin = Sex_Ratio_Q25,
                  ymax = Sex_Ratio_Q75,
                  col = NULL,
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('') +
  theme_bw() +
  ylim(c(0, 0.7)) +
  guides(color = "none", lty = 'none') +
  ylab('') 

figF

# plot figure - mature abundances, P base
figG <- examples_to_plot %>%
  filter(Age == 'Mature') %>%
  filter(Abundance_Median > 0) %>%
  filter(TRT == 'Narrow') %>%
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
  geom_line(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('Year') +
  ylab('median \n mature abundance') +
  ylim(c(0, 10500)) +
  theme_bw() +
  guides(color = "none", 
         lty = "none")

figG

# plot figure - mature abundances, GM base
figH <- examples_to_plot %>%
  filter(Age == 'Mature') %>%
  filter(Abundance_Median > 0) %>%
  filter(TRT == 'Wide') %>%
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
  geom_line(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('Year') +
  ylab('') +
  ylim(c(0, 10500)) +
  theme_bw() +
  guides(color = "none", 
         lty = "none")

figH

# figA/B: temperatures and emergence success
# figB: emergence success
# figC: hatchling sex ratios (P base)
# figD: hatchling sex ratios (GM base)
# figE: mature sex ratios (P base)
# figF: mature sex ratios (GM base)
# figG: mature abundance (P base)
# figH: mature abundance (GM base)


option1 <- (figA1 + figB2) / (figC + figD) / (figE + figF) / (figG + figH) +
  plot_layout(heights = c(1, 1, 1, 1)) +
  plot_annotation(tag_levels = "A")

option1

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



final_fig <- option3

# save to file
ggsave(plot = final_fig,
       filename = paste('sample_outputs.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8.5, height = 11)

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
