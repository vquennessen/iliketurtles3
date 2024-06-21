# Q2 heatmaps

# set working directory
setwd("~/Projects/iliketurtles3/code/power analysis")

# load libraries
library(dplyr)
library(ggplot2)
library(viridisLite)

# load data
load("~/Projects/iliketurtles3/code/power analysis/32_nests_to_sample_1e+05.Rdata")
DF32 <- output

load("~/Projects/iliketurtles3/code/power analysis/96_nests_to_sample_1e+05.Rdata")
DF96 <- output
################################################################################

# sample size 32 heatmap
fig32 <- ggplot(data = DF32, aes(x = PropNests, y = BSR, fill = Proportion)) +
  geom_tile(color = 'white',
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(n = 5)[1],
                       mid = hcl.colors(n = 5)[3],
                       high = hcl.colors(n = 5)[5],
                       midpoint = 0.5,
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       limits = c(0, 1),
                       na.value = 'gray') +
  xlab('Proportion of Nests Sampled') +
  ylab('Breeding Sex Ratio') +
  labs(fill = 'Proportion \n') +
  # geom_text(aes(label = round(Proportion, 2))) +
  ggtitle('a. Sample size 32') +
  theme(text = element_text(size = 15))

# save heatmap
ggsave(fig32, 
       file = 'C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/figure4.png', 
       height = 4, width = 6)
################################################################################

# sample size 96 heatmap
fig96 <- ggplot(data = DF96, aes(x = PropNests, y = BSR, fill = Proportion)) +
  geom_tile(color = 'white',
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(n = 5)[1],
                       mid = hcl.colors(n = 5)[3],
                       high = hcl.colors(n = 5)[5],
                       midpoint = 0.5,
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       limits = c(0, 1),
                       na.value = 'gray') +
  xlab('Proportion of Nests Sampled') +
  ylab('Breeding Sex Ratio') +
  labs(fill = 'Proportion \n') +
  # geom_text(aes(label = round(Proportion, 2))) +
  ggtitle('a. Sample size 96') +
  theme(text = element_text(size = 15))

# save heatmap
ggsave(fig96, 
       file = 'C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/figure5.pdf', 
       height = 4, width = 6)
