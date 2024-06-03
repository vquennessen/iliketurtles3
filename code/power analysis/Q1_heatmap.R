#Q1 heatmap

# set working directory
setwd('~/Projects/iliketurtles3/code/')

# load libraries
library(viridis)
library(tidyverse)

# load probability dataframe
load("~/Projects/iliketurtles3/output/power analysis/probabilities1e+06.Rdata")
# output called 'probs'

# undo spread from hatchlings_to_sample.R
# DF <- probs %>%
#   gather(key = 'Sample_Size', value = 'Probability', 3:4)

# make factor variables
probs$Males <- as.factor(probs$Males)
probs$Fertilization_mode <- factor(probs$Fertilization_mode, 
                                   levels = c('mixed_dominant', 'dominant90', 'dominant70',
                                              'dominant50', 'exponential', 'random'),
                                   labels = c('Mixed Dominant', 'Dominant (90%)', 'Dominant (70%)', 
                                              'Dominant (50%)', 'Exponential', 'Random'))
probs$Sample_Size <- as.factor(probs$Sample_size)
probs$Sample_Size <- as.factor(probs$Sample_size)

# subset by sample size
DF32 <- subset(probs, Sample_size == 32)
DF96 <- subset(probs, Sample_size == 96)

# start heatmap for sample size 32
figure2 <- ggplot(data = DF32, aes(x = Males, 
                                   y = Fertilization_mode, 
                                   fill = Proportion_correct)) +
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
  guides(fill = guide_colourbar(title = 'Proportion \n correct')) +
  xlab('Number of males') +
  ylab('Fertilization mode') +
  ggtitle('Probability of detecting all contributing males \n and marginal contributions') +
  theme(panel.background = element_blank()) +
  
  # random
  annotate(geom = 'text', x = 1, y = 6.25, label = '1.000', colour = 'black') +
  annotate(geom = 'text', x = 2, y = 6.25, label = '1.000', colour = 'black') +
  annotate(geom = 'text', x = 3, y = 6.25, label = '1.000', colour = 'black') +
  annotate(geom = 'text', x = 4, y = 6.25, label = '0.996', colour = 'black') +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.85, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.85, xend = 2.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.77, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.77, xend = 3.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.72, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.72, xend = 4.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  
  # exponential
  annotate(geom = 'text', x = 1, y = 5.25, label = '1.000', colour = 'black') +
  annotate(geom = 'text', x = 2, y = 5.25, label = '1.000', colour = 'black') +
  annotate(geom = 'text', x = 3, y = 5.25, label = '0.972', colour = 'black') +
  annotate(geom = 'text', x = 4, y = 5.25, label = '0.748', colour = 'black') +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.77, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.635, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.635, xend = 3.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.5825, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.5825, xend = 4.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  
  # dominant 50%
  annotate(geom = 'text', x = 1, y = 4.25, label = '1.000', colour = 'black') +
  annotate(geom = 'text', x = 2, y = 4.25, label = '1.000', colour = 'black') +
  annotate(geom = 'text', x = 3, y = 4.25, label = '0.991', colour = 'black') +
  annotate(geom = 'text', x = 4, y = 4.25, label = '0.945', colour = 'black') +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.77, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.69, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.69, xend = 3.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.645, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.645, xend = 4.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  
  # dominant 70%
  annotate(geom = 'text', x = 1, y = 3.25, label = '1.000', colour = 'black') +
  annotate(geom = 'text', x = 2, y = 3.25, label = '0.989', colour = 'black') +
  annotate(geom = 'text', x = 3, y = 3.25, label = '0.900', colour = 'black') +
  annotate(geom = 'text', x = 4, y = 3.25, label = '0.702', colour = 'black') +
  
  annotate("segment", x = 0.52, xend = 0.82, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.82, xend = 1.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.67, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.67, xend = 2.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.62, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.62, xend = 3.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.595, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.595, xend = 4.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  
  # dominant 90
  annotate(geom = 'text', x = 1, y = 2.25, label = '0.965', colour = 'black') +
  annotate(geom = 'text', x = 2, y = 2.25, label = '0.647', colour = 'black') +
  annotate(geom = 'text', x = 3, y = 2.25, label = '0.282', colour = 'white') +
  annotate(geom = 'text', x = 4, y = 2.25, label = '0.088', colour = 'white') +
  
  annotate("segment", x = 0.52, xend = 0.62, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.62, xend = 1.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.57, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.57, xend = 2.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.553, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.553, xend = 3.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.545, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.545, xend = 4.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  
  # mixed dominant
  annotate(geom = 'text', x = 1, y = 1.25, label = '0.988', colour = 'black') +
  annotate(geom = 'text', x = 2, y = 1.25, label = '0.878', colour = 'black') +
  annotate(geom = 'text', x = 3, y = 1.25, label = '0.723', colour = 'black') +
  annotate(geom = 'text', x = 4, y = 1.25, label = '0.578', colour = 'black') + 
  
  annotate("segment", x = 0.52, xend = 0.62, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.62, xend = 0.82, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 0.82, xend = 1.02, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 1.02, xend = 1.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.57, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.57, xend = 1.67, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 1.67, xend = 1.77, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.553, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.553, xend = 2.62, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 2.62, xend = 2.687, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 2.687, xend = 3.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) + 
  annotate("segment", x = 3.52, xend = 3.545, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.545, xend = 3.595, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 3.595, xend = 3.645, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 3.645, xend = 4.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) +
  
  theme(text = element_text(size = 15))

ggsave(figure2, 
     file = "C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/figure2.png", 
     height = 6, width = 8)
