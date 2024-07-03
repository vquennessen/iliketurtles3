# Q2 heatmaps

# set working directory
setwd("~/Projects/iliketurtles3/code/power analysis")

# load libraries
library(dplyr)
library(ggplot2)
library(viridisLite)
library(patchwork)

# model parameters
fmodes <- c('random', 
            'exponential',
            # 'dominant50',
            # 'dominant70',
            # 'dominant90',
            'mixed_dominant')

fmode_titles <- c('Random', 'Exponential', 
                  # 'Dominant 50', 'Dominant 70', 'Dominant 90', 
                  'Mixed Dominant')

sample_sizes <- c(32, 96)

# dimensions
nS <- length(sample_sizes)

# initialize list of dataframes
DF <- data.frame()

# for each fertilization mode
for (f in 1:length(fmodes)) {
  
  fmode <- fmodes[f]
  fmode_title <- fmode_titles[f]
  
  for (s in 1:length(sample_sizes)) {
    
    sample_size <- sample_sizes[s]
    
    # load data
    load(paste('~/Projects/iliketurtles3/code/power analysis/', sample_size, 
               '_nests_to_sample_', fmode, '_100.Rdata', sep = ''))
    DF1 <- output
    
    # # dimensions
    # nBSR <- n_distinct(DF1$BSR)
    # nPN <- n_distinct(DF1$PropNests)
    
    ################################################################################
    
    # # save heatmap
    # ggsave(fig32, 
    #        file = 'C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/32_mixed_dominant.png', 
    #        height = 4, width = 6)
    ################################################################################
    
    # # sample size 96 heatmap
    # fig96 <- ggplot(data = DF96, aes(x = PropNests, y = BSR, fill = Proportion)) +
    #   geom_tile(color = 'white',
    #             lwd = 1.5,
    #             linetype = 1) +
    #   scale_fill_gradient2(low = hcl.colors(n = 5)[1],
    #                        mid = hcl.colors(n = 5)[3],
    #                        high = hcl.colors(n = 5)[5],
    #                        midpoint = 0.5,
    #                        breaks = c(0, 0.25, 0.5, 0.75, 1),
    #                        limits = c(0, 1),
    #                        na.value = 'gray') +
    #   xlab('Proportion of Nests Sampled') +
    #   ylab('Breeding Sex Ratio') +
    #   labs(fill = 'Proportion \n') +
    #   # geom_text(aes(label = round(Proportion, 2))) +
    #   ggtitle('b. Sample size 96') +
    #   theme(text = element_text(size = 15))
    
    # # save heatmap
    # ggsave(fig96, 
    #        file = 'C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/96_mixed_dominant.pdf', 
    #        height = 4, width = 6)
    
    # # put heatmaps together
    # A <- fig32 + theme(legend.position = 'none')
    # B <- fig96 + ylab('') + theme(axis.text.y = element_blank())
    # 
    # patchwork <- A + B + 
    #   xlab('Proportion of Nests Sampled') +
    #   ggtitle(fmode)
    # patchwork
    
    # fertilization mode and sample size vectors
    Fertilization_Mode <- factor(rep(fmode_title, nrow(DF1)))
    Sample_Size <- factor(rep(sample_size, nrow(DF1)))
    
    # add fertilization mode and sample size to output dataframe
    DF2 <- cbind(Fertilization_Mode, Sample_Size, DF1)
    
    # add new scenario to DF
    DF <- rbind(DF, DF2)
    
  }
  
}

# DF$Fertilization_Mode <- factor(DF$Fertilization_Mode)

# make fertilization mode a factor
# DF$Fertilization_Mode <- factor(DF$Fertilization_Mode, 
#                                 levels = fmodes, 
#                                 labels = fmode_titles)

# heatmaps
fig4 <- ggplot(data = DF, aes(x = PropNests, y = OSR, fill = Proportion)) +
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
  theme(text = element_text(size = 15)) +
  facet_grid(rows = vars(Fertilization_Mode), cols = vars(Sample_Size))

fig4

