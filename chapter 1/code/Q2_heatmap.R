# Q2 heatmaps

# set working directory
setwd("~/Projects/multiple_paternity_power_analyses")

# load libraries
library(dplyr)
library(ggplot2)
library(viridisLite)
library(patchwork)

##### model parameters #########################################################

## fertilization modes
fmodes <- c('random',
            # 'exponential',
            # 'dominant50',
            # 'dominant70',
            # 'mixed_dominant', 
            'dominant90')
# fmodes <- c('random')
# fmodes <- c('exponential')
# fmodes <- c('mixed_dominant')


# fertilization mode titles
fmode_titles <- c('Random', # 'Exponential', 
                  # 'Dominant 50', 'Dominant 70', 'Dominant 90',
                  # 'Mixed Dominant')
                  'Dominant 90')

## figure number
# fig <- 'fig4'
# fig <- 'figS2'
fig <- 'figS3'

## data title
# data_title <- 'no_polygyny'
# data_title <- 'uniform_Mprob_no_polygyny'
data_title <- 'uniform_Mprob_and_Fprob'

# sample sizes
sample_sizes <- c(32, 96)

# number of simulations
nsims <- 1e+05

# dimensions
nS <- length(sample_sizes)

################################################################################

# initialize dataframe
DF <- data.frame()

# for each fertilization mode
for (f in 1:length(fmodes)) {
  
  fmode <- fmodes[f]
  fmode_title <- fmode_titles[f]
  
  for (s in 1:length(sample_sizes)) {
    
    sample_size <- sample_sizes[s]
    
    # load data
    load(paste('~/Projects/multiple_paternity_power_analyses/output/', 
               data_title, '/', sample_size, '_nests_to_sample_', fmode, '_', 
               nsims, '.Rdata', sep = ''))
    DF1 <- output
    
    # fertilization mode and sample size vectors
    Fertilization_Mode <- factor(rep(fmode_title, nrow(DF1)))
    Sample_Size <- factor(rep(sample_size, nrow(DF1)))
    
    # add fertilization mode and sample size to output dataframe
    DF2 <- cbind(Fertilization_Mode, Sample_Size, DF1)
    
    # add new scenario to DF
    DF <- rbind(DF, DF2)
    
  }
  
}

# change 32 and 96 in DF to sample size 32 and sample size 96
DF$Sample_Label[DF$Sample_Size == 32] <- 'Sample size 32'
DF$Sample_Label[DF$Sample_Size == 96] <- 'Sample size 96'

##### heatmap #################################################################
# fig4 <- ggplot(data = DF, aes(x = PropNests, y = OSR, fill = Proportion)) +
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
#   ylab('Operational Sex Ratio') +
#   labs(fill = 'Proportion \n') +
#   # geom_text(aes(label = round(Proportion, 2))) +
#   theme(text = element_text(size = 25)) +
#   facet_grid(rows = vars(Fertilization_Mode), cols = vars(Sample_Size)) +
#   theme(panel.spacing.x=unit(1.5, "lines")) +
#   theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
#   theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
#   theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm"))
# 
# 
# 
# fig4
# 
# # save heatmap
# ggsave(fig4,
#        file = 'C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/fig4_even_Mprobs_Fprobs.png',
#        height = 6, width = 12)

##### contour plot #############################################################

# add facet labels as 'label' in DF
DF$label <- rep(c('(a)', '(b)', '(c)', '(d)'), each = nrow(output))

fig2 <- ggplot(data = DF, aes(x = PropNests, 
                              y = OSR, 
                              z = Proportion)) +
  geom_contour_filled(bins = 5) +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  labs(fill = 'Proportion \n correct \n') +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  geom_text(aes(x = 0.125, y = 0.45, 
                label = label, 
                group = label), 
            size = 5, colour = 'white',
            inherit.aes = FALSE) +
  facet_grid(rows = vars(Fertilization_Mode), cols = vars(Sample_Label)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm"))

fig2

# save contour plot
ggsave(fig2,
       file = paste('C://Users/Vic//Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/', 
       fig, '_', data_title, '.png', sep = ''), 
       height = 6, width = 12)

