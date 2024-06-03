# figure 1 for chapter 1 - power analyses

# load libraries
library(patchwork)
library(ggplot2)

# load figures
load("~/Projects/iliketurtles3/output/power analysis/figures1e+06.Rdata")

# separate out individual figures
A <- figs[[1]] + theme(legend.position = 'none') + xlab('') + ggtitle('a. Random')
B <- figs[[2]] + theme(legend.position = 'none') + xlab('') + ylab('') + ggtitle('b. Exponential')
C <- figs[[3]] + theme(legend.position = 'none') + xlab('') + ggtitle('c. Dominant (50%)')
D <- figs[[4]] + xlab('') + ylab('') + ggtitle('d. Dominant (70%)')
E <- figs[[5]] + theme(legend.position = 'none') + ggtitle('e. Dominant (90%)')
G <- figs[[6]] + theme(legend.position = 'none') + ylab('') + ggtitle('f. Mixed dominant')


# patchwork <- (A + B) / (C + D) / (E + G)
patchwork <- (A + B) / (C + D) / (E + G)

# add lowercase letter tags
final_fig <- patchwork + plot_annotation(tag_levels = "a")

# save
ggsave(filename = 'figure1.png', 
       path = 'C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/',
       plot = patchwork, 
       width = 7, 
       height = 7)
