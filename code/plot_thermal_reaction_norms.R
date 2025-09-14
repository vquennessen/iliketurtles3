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

##### thermal reaction norms ###################################################

# emergence success variables
hatch_success_A <- 0.86                   # logistic by temp - A
hatch_success_k <- -1.7                   # logistic by temp - beta
hatch_success_t0 <- 32.7                  # logistic by temp - t0

# temperatures
x <- seq(from = 20, to = 40, by = 0.01)

# patricio et al.
t_piv1 <- 29.2
# k1 <- -1.4 # Patricio et al 2017, TRT 
k1 <- -1.34
y1 <- round(1/(1 + exp(-k1*(x - t_piv1))), 5)

# embryogrowth (all greens)
t_piv2 <- 29.2
k2 <- -0.561
y2 <- round(1/(1 + exp(-k2*(x - t_piv2))), 5)

# emergence success
# ES <- hatch_success_A / (1 + exp(-hatch_success_k * (x - hatch_success_t0)))


# make dataframe
TRN <- data.frame(Temperature = rep(x, times = 2),
                  Population = rep(c('P2017', 
                                     'GM2006'
                                     # 'Emergence Success'
                  ), each = length(x)),
                  Proportion_Male = c(y1, 
                                      y2 
                                      # ES
                  ))

# WA <- subset(TRN, Model == 'West Africa')
# SN <- subset(TRN, Model == 'Suriname')

TRT_lower_wide <- 23.8
TRT_lower_narrow <- 27
TRT_upper_narrow <- 31.4
TRT_upper_wide <- 34.4

# points
points <- data.frame(Temperature = c(23.8, 34.4, 27, 31.4, 23.8, 34.4, 27, 31.4), 
                     Proportion_Male = c(0.05, 0.05, 0.05, 0.05, 0.95, 0.95, 
                                         0.95, 0.95), 
                     Population = c('GM2006', 'GM2006', 'P2017', 'P2017', 
                                    'GM2006', 'GM2006', 'P2017', 'P2017'))


colors = c('#FF3300', '#FF3300', '#6600FF', '#6600FF', 
           '#FF3300', '#FF3300', '#6600FF', '#6600FF')


# plot
A <- ggplot(data = TRN, aes(x = Temperature, y = Proportion_Male, 
                            col = Population, lty = Population)) +
  geom_hline(yintercept = c(0.05, 0.50, 0.95), lwd = 1, lty = 1) +
  geom_vline(xintercept = 29.2, col = 'black', lwd = 1.5, lty = 1) +
  geom_vline(xintercept = 31.8, col = 'gray60', lwd = 1, lty = 1) +
  geom_line(lwd = 2) +
  scale_color_manual(values = c('#FF3300', '#6600FF')) +
  scale_linetype_manual(values = c(4, 1)) +
  ylab('Hatchling sex ratio \n (proportion male)') +
  xlab('Incubation temperature (\u00B0C)') +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12),
        legend.position = 'none') +
  annotate("label", 
           x = c(39.4, 39.4, 39.4, 27.5, 33.5),
           y = c(0.1, 0.55, 1, 0.2, 0.7), 
           label = c("0.05", '0.50', '0.95', '29.2 \u00B0C', '31.8 \u00B0C'), 
           size = 3.5, 
           label.size = 0) +
  geom_point(data = points, col = colors, size = 5)


A

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
  geom_hline(yintercept = 0.5, linetype = 2, alpha = 0.5, lwd = 2) +
  geom_line(lwd = 2) +
  scale_color_manual(values = rev(colors)[-1], 
                     labels = OSRs) +
  # for replacement legend - delete for legend with beta values
  labs(color = 'Minimum \n OSR needed \n for 99% \n reproductive \n success') +
  ylab('Reproductive success \n (probability a female mates)') +
  xlab('Operational sex ratio (OSR)') +
  geom_point(data = points, 
             col = rev(colors)[-1], 
             size = 5) +
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

# put figures together
final_fig <- A + B +
  plot_annotation(tag_levels = 'A', 
                  tag_prefix = '(', 
                  tag_suffix = ')')

final_fig

# save
ggsave(filename = 'figures/population_parameters.png', 
       plot = final_fig, 
       width = 12, height = 4)
