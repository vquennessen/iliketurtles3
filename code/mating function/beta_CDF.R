# cumulative density function of a beta distribution

# set working directory
setwd('~/Projects/iliketurtles3/code/')

# load libraries
library(ggplot2)
library(viridis)

# source code
source('mating function/OPMs_to_betas.R')

# values of x to plot
x <- seq(from = 0, to = 0.5, by = 0.001)

# OSR values
OPMs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)

# beta values to cycle through
betas <- as.numeric(OPMs_to_betas(OPMs))

# colors
colors <- viridis(length(betas) + 1)

# initialise DF
DF <- data.frame(Operational_Proportion_Male = rep(x, times = length(betas)), 
                 Beta = rep(betas, each = length(x)), 
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

# plot
fig <- ggplot(data = DF, aes(x = Operational_Proportion_Male, 
                             y = Reproductive_Success, 
                             color = Beta)) +
  geom_hline(yintercept = 0.5, linetype = 2, alpha = 0.5, lwd = 2) +
  geom_line(lwd = 2) +
  scale_color_manual(values = rev(colors)[-1], 
                     labels = rev(OPMs)) +
  # for replacement legend - delete for legend with beta values
  labs(color = 'Minimum \n OPM needed \n for 99% \n reproductive \n success') +
  ylab('Reproductive success') +
  xlab('Operational proportion male') +
  # ggtitle('Hypothetical mating functions') +
  theme_bw() +
  theme(axis.title.y = element_text(margin = margin(r = 15, l = 10)), 
        axis.title.x = element_text(margin = margin(t = 15, b = 10)), 
        plot.title = element_text(margin = margin(b = 10, t = 10), 
                                  size = 30), 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 25), 
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20))

# save to chapter 2 stuff
ggsave(filename = '~/Projects/iliketurtles3/figures/betaCDF.png', 
       plot = fig, 
       width = 10, 
       height = 6
)
