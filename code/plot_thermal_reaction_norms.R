# set working directory
setwd("~/Projects/iliketurtles3")

# load libraries
library(ggplot2)
library(viridis)
library(magrittr) 
library(dplyr)
library(patchwork)

# source code
source('code/mating function/OSRs_to_betas.R')

##### thermal reaction norms (old) ###################################################

# # emergence success variables
# hatch_success_A <- 0.86                   # logistic by temp - A
# hatch_success_k <- -1.7                   # logistic by temp - beta
# hatch_success_t0 <- 32.7                  # logistic by temp - t0
# 
# # temperatures
# x <- seq(from = 20, to = 40, by = 0.01)
# 
# # patricio et al.
# t_piv1 <- 29.4
# # k1 <- -1.4 # Patricio et al 2017, TRT 
# k1 <- -1.27
# y1 <- round(1/(1 + exp(-k1*(x - t_piv1))), 5)
# 
# # double the TRT
# t_piv2 <- 29.4
# k2 <- -0.77
# y2 <- round(1/(1 + exp(-k2*(x - t_piv2))), 5)
# 
# # emergence success
# # ES <- hatch_success_A / (1 + exp(-hatch_success_k * (x - hatch_success_t0)))
# 
# 
# # make dataframe
# TRN <- data.frame(Temperature = rep(x, times = 2),
#                   Population = rep(c('Narrow TRT', 
#                                      'Wide TRT'
#                                      # 'Emergence Success'
#                   ), each = length(x)),
#                   Proportion_Male = c(y1, 
#                                       y2 
#                                       # ES
#                   ))
# 
# # WA <- subset(TRN, Model == 'West Africa')
# # SN <- subset(TRN, Model == 'Suriname')
# 
# # TRT_lower_wide <- 23.8
# # TRT_lower_narrow <- 27
# # TRT_upper_narrow <- 31.4
# # TRT_upper_wide <- 34.4
# 
# TRT_lower_wide <- 25.584
# TRT_lower_narrow <- 27.492
# TRT_upper_narrow <- 31.304
# TRT_upper_wide <- 33.216
# 
# # points
# points <- data.frame(Temperature = c(TRT_lower_wide, TRT_upper_wide, 
#                                      TRT_lower_narrow, TRT_upper_narrow, 
#                                      TRT_lower_wide, TRT_upper_wide, 
#                                      TRT_lower_narrow, TRT_upper_narrow), 
#                      Proportion_Male = c(0.95, 0.05, 
#                                          0.95, 0.05, 
#                                          0.95, 0.05, 
#                                          0.95, 0.05), 
#                      Population = c('Wide TRT', 'Wide TRT', 
#                                     'Narrow TRT', 'Narrow TRT', 
#                                     'Wide TRT', 'Wide TRT', 
#                                     'Narrow TRT', 'Narrow TRT'))
# 
# 
# colors = c('#FF3300', '#FF3300', 
#            '#6600FF', '#6600FF', 
#            '#FF3300', '#FF3300', 
#            '#6600FF', '#6600FF')
# 
# 
# # plot
# A <- ggplot(data = TRN, aes(x = Temperature, y = Proportion_Male, 
#                             col = Population, lty = Population)) +
#   geom_hline(yintercept = c(0.05, 0.50, 0.95), lwd = 1, lty = 1) +
#   geom_vline(xintercept = 29.4, col = 'black', lwd = 1.5, lty = 1) +
#   geom_vline(xintercept = 31.8, col = 'gray60', lwd = 1, lty = 1) +
#   geom_line(lwd = 2) +
#   scale_color_manual(values = c('#FF3300', '#6600FF')) +
#   scale_linetype_manual(values = c(4, 1)) +
#   ylab('Hatchling sex ratio \n (proportion male)') +
#   xlab('Incubation temperature (\u00B0C)') +
#   xlim(c(22.5, 37.5)) +
#   theme_bw() +
#   theme(axis.text = element_text(size = 10), 
#         axis.title = element_text(size = 12),
#         legend.position = 'none') +
#   annotate("label", 
#            x = c(34.4, 34.4, 34.4, 28, 32.5),
#            y = c(0.1, 0.55, 1, 0.2, 0.7), 
#            label = c('0.05', '0.50', '0.95', '29.4 \u00B0C', '31.8 \u00B0C'), 
#            size = 3.5, 
#            label.size = 0) +
#   geom_point(data = points, col = colors, size = 5)
# 
# 
# A

# # save individual figure
# ggsave("figures/thermal_reaction_norms.png",
#        plot = A,
#        height = 5,
#        width = 8)

##### hypothetical mating functions ############################################

# values of x to plot
x <- seq(from = 0, to = 0.5, by = 0.001)

# OSR values
OSRs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
xFs <- round(1/OSRs - 1, 2)

# beta values to cycle through
betas <- as.numeric(OSRs_to_betas(OSRs))

# colors
colors <- viridis(length(betas) + 1)

# initialise DF
DF <- data.frame(Operational_Sex_Ratio = rep(x, times = length(betas)), 
                 Beta = rep(betas, each = length(x)), 
                 OSR = rep(OSRs, each = length(x)),
                 xF = rep(xFs, each = length(x)),
                 Reproductive_Success = NA) 

# CDF function
for (b in 1:length(betas)) {
  
  start <- (b - 1)*length(x) + 1
  stop <- start + length(x) - 1
  DF$Reproductive_Success[start:stop] <- pbeta(2 * x, 
                                               shape1 = 1, 
                                               shape2 = betas[b])
  
}

# make Beta factor
DF$Beta <- as.factor(DF$Beta)
DF$xF <- as.factor(DF$xF)

# points dataframe
points <- data.frame(Operational_Sex_Ratio = OSRs, 
                     Reproductive_Success = 1, 
                     Beta = betas)


# plot
B <- ggplot(data = DF, aes(x = Operational_Sex_Ratio, 
                           y = Reproductive_Success, 
                           color = Beta)) +
  # geom_hline(yintercept = 0.5, linetype = 2, alpha = 0.5, lwd = 1.5) +
  geom_line(lwd = 1.25) +
  scale_color_manual(values = rev(colors)[-1], 
                     labels = OSRs) +
  # for replacement legend - delete for legend with beta values
  labs(color = 'Minimum \n OSR needed \n for 99% \n reproductive \n success') +
  ylab('Reproductive success \n (probability a female mates)') +
  xlab('Operational sex ratio (OSR)') +
  geom_point(data = points, 
             col = rev(colors)[-1], 
             size = 3) +
  # ggtitle('Hypothetical mating functions') +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12))
# theme(axis.title.y = element_text(margin = margin(r = 15, l = 10)), 
#       axis.title.x = element_text(margin = margin(t = 15, b = 10)), 
#       plot.title = element_text(margin = margin(b = 10, t = 10), 
#                                 size = 30), 
#       axis.text = element_text(size = 20), 
#       axis.title = element_text(size = 25), 
#       legend.title = element_text(size = 20), 
#       legend.text = element_text(size = 20))

B

# # save individual figure to chapter 2 stuff
# ggsave(filename = '~/Projects/iliketurtles3/figures/betaCDF.png', 
#        plot = B, 
#        width = 10, 
#        height = 6
# )

##### final figure, thermal reaction norms and mating functions combined #######

# # put figures together
# final_fig <- B + A +
#   plot_annotation(tag_levels = 'A', 
#                   tag_prefix = '(', 
#                   tag_suffix = ')')
# 
# final_fig
# 
# # save
# ggsave(filename = 'figures/population_parameters.png', 
#        plot = final_fig, 
#        width = 12, height = 4)

##### new k values 10/6/2025 ###################################################
# temperatures
x <- seq(from = 22.5, to = 37.5, by = 0.01)

# narrow TRT
t_piv1 <- 29.4
k1.2 <- -1.54
y1.2 <- round(1/(1 + exp(-k1.2*(x - t_piv1))), 5)

# embryogrowth (just Godfrey and Mrosovsky 2006)
t_piv2 <- 29.4
k2.2 <- -0.77
y2.2 <- round(1/(1 + exp(-k2.2*(x - t_piv2))), 5)

# emergence success
# ES <- hatch_success_A / (1 + exp(-hatch_success_k * (x - hatch_success_t0)))


# make dataframe
TRN2 <- data.frame(Temperature = rep(x, times = 2),
                  Population = rep(c('Narrow TRT', 
                                     'Wide TRT'
                                     # 'Emergence Success'
                  ), each = length(x)),
                  Proportion_Male = c(y1.2, 
                                      y2.2 
                                      # ES
                  ))

# WA <- subset(TRN, Model == 'West Africa')
# SN <- subset(TRN, Model == 'Suriname')

TRT_lower_wide <- TRN2 %>% filter(Population == 'Wide TRT') %>% 
  filter(Proportion_Male <= 0.95) %>% arrange(Temperature) %>% head(1) %>% 
  select(Temperature) %>% as.numeric
# 25.58
TRT_upper_wide <- TRN2 %>% filter(Population == 'Wide TRT') %>% 
  filter(Proportion_Male >= 0.05) %>% arrange(desc(Temperature)) %>% head(1) %>% 
  select(Temperature) %>% as.numeric
# 33.22
TRT_lower_narrow <- TRN2 %>% filter(Population == 'Narrow TRT') %>% 
  filter(Proportion_Male <= 0.95) %>% arrange(Temperature) %>% head(1) %>% 
  select(Temperature) %>% as.numeric
# 27.3
TRT_upper_narrow <- TRN2 %>% filter(Population == 'Narrow TRT') %>% 
  filter(Proportion_Male >= 0.05) %>% arrange(desc(Temperature)) %>% head(1) %>% 
  select(Temperature) %>% as.numeric
# 31.1

TRT_wide <- TRT_upper_wide - TRT_lower_wide
# 7.64
TRT_narrow <- TRT_upper_narrow - TRT_lower_narrow
# 3.82

# points
points <- data.frame(Temperature = c(TRT_lower_wide, TRT_upper_wide, 
                                     TRT_lower_narrow, TRT_upper_narrow, 
                                     TRT_lower_wide, TRT_upper_wide, 
                                     TRT_lower_narrow, TRT_upper_narrow), 
                     Proportion_Male = c(0.05, 0.05, 
                                         0.05, 0.05, 
                                         0.95, 0.95, 
                                         0.95, 0.95), 
                     Population = c('Wide TRT', 'Wide TRT', 
                                    'Narrow TRT', 'Narrow TRT', 
                                    'Wide TRT', 'Wide TRT', 
                                    'Narrow TRT', 'Narrow TRT'))


colors = c('#6600FF', '#6600FF', '#FF3300', '#FF3300', 
           '#6600FF', '#6600FF', '#FF3300', '#FF3300')


# plot
A <- ggplot(data = TRN2, aes(x = Temperature, y = Proportion_Male, 
                            col = Population, lty = Population)) +
  geom_hline(yintercept = c(0.05, 0.50, 0.95), lwd = 1, lty = 1) +
  geom_vline(xintercept = 29.2, col = 'black', lwd = 1.5, lty = 1) +
  geom_vline(xintercept = 31.8, col = 'gray60', lwd = 1, lty = 1) +
  geom_line(lwd = 1.25) +
  scale_color_manual(values = c('#FF3300', '#6600FF')) +
  scale_linetype_manual(values = c(2, 1)) +
  ylab('Hatchling sex ratio \n (proportion male)') +
  xlab('Incubation temperature (\u00B0C)') +
  xlim(c(22.5, 37.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12),
        legend.position = 'none') +
  annotate("label", 
           x = c(37, 37, 37, 28, 33),
           y = c(0.1, 0.55, 1, 0.32, 0.68), 
           label = c('0.05', '0.50', '0.95', '29.4 \u00B0C', '31.8 \u00B0C'), 
           size = 3.5, 
           label.size = 0) +
  geom_point(data = points, col = colors, size = 3) +
  theme(margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt'))


A

# put figures together
final_fig <- B + A +
  plot_annotation(tag_levels = 'A', 
                  tag_prefix = '(', 
                  tag_suffix = ')') +
  theme(plot.margin = unit(c(top = 0, right = 25, bottom = 0, left = 0), 
                           unit = 'pt'))

final_fig

# save
ggsave(filename = 'figures/population_parameters.png', 
       plot = final_fig, 
       width = 12, height = 4)
