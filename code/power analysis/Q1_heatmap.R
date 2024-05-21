#Q1 heatmap

# set working directory
setwd('~/Projects/iliketurtles3/code/')

# load libraries
library(viridis)
library(ggplot2)

# load probability dataframe
load("~/Projects/iliketurtles3/output/power analysis/probabilities50000.Rdata")
# output called 'probs'

# make Males factor variable
probs$Males <- as.factor(probs$Males)

# undo spread from hatchlings_to_sample.R
DF <- probs %>%
  gather(key = 'Sample_Size', value = 'Probability', 3:4)

# subset by sample size
DF32 <- subset(DF, Sample_Size == 32)
DF96 <- subset(DF, Sample_Size == 96)

# start heatmap for sample size 32
ggplot(data = DF32, aes(x = Males, y = Fertilization_mode, fill = 'Probability')) +
  geom_tile(color = 'white',
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(5, 'viridis')[1], 
                       mid = hcl.colors(5, 'viridis')[3], 
                       high = hcl.colors(5, 'viridis')[5], 
                       midpoint = 0.5, 
                       breaks = c(0, 0.25, 0.5, 0.75, 1), 
                       limits = c(0, 1), 
                       na.value = 'gray') +
  guides(fill = guide_colourbar(title = 'Probability')) +
  xlab('Number of males') +
  ylab('Fertilization mode') +
  ggtitle('Probability of detecting all contributing males \n and marginal contributions') +
  theme(panel.background = element_blank())


