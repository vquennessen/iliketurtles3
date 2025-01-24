# figure 1 for chapter 1 - power analyses

# load libraries
library(patchwork)
library(ggplot2)

# load figures
load("~/Projects/multiple_paternity_power_analyses/output/figures1e+06.Rdata")

# text sizes
title_size <- 10
axis_title_size <- 15
axis_ticks_size <- 10

# separate out individual figures
A <- figs[[1]] + theme(legend.position = 'none') + xlab('') + 
  ylab('Proportion correct') + ggtitle('(a) Random') +
  theme(axis.text = element_text(size = axis_ticks_size), 
        axis.title = element_text(size = axis_title_size), 
        title = element_text(size = title_size)) +
  theme(axis.title.y = element_text(vjust = 4)) +
  theme(plot.margin = margin(0, 0.25, 0, 0, 'cm'))

B <- figs[[2]] + theme(legend.position = 'none') + xlab('') + ylab('') + 
  ggtitle('(b) Exponential') +
  theme(axis.text = element_text(size = axis_ticks_size), 
        axis.title = element_text(size = axis_title_size), 
        title = element_text(size = title_size))

C <- figs[[3]] + theme(legend.position = 'none') + xlab('') + 
  ylab('Proportion correct') + ggtitle('(c) Dominant (50%)') +
  theme(axis.text = element_text(size = axis_ticks_size), 
        axis.title = element_text(size = axis_title_size), 
        title = element_text(size = title_size))  +
  theme(axis.title.y = element_text(vjust = 4)) +
  theme(plot.margin = margin(0, 0.25, 0, 0, 'cm'))

D <- figs[[4]] + labs(col = 'Number of \n fathers \n') + xlab('') + ylab('') + 
  ggtitle('(d) Dominant (70%)') +
  theme(axis.text = element_text(size = axis_ticks_size), 
        axis.title = element_text(size = axis_title_size), 
        title = element_text(size = title_size), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12))

E <- figs[[5]] + theme(legend.position = 'none') + xlab('Hatchlings sampled') +
  ylab('Proportion correct') + ggtitle('(e) Dominant (90%)') +
  theme(axis.text = element_text(size = axis_ticks_size), 
        axis.title = element_text(size = axis_title_size), 
        title = element_text(size = title_size))  +
  theme(axis.title.y = element_text(vjust = 4)) +
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(plot.margin = margin(0, 0.25, 0, 0, 'cm'))

G <- figs[[6]] + theme(legend.position = 'none') + xlab('Hatchlings sampled') + 
  ylab('') + ggtitle('(f) Mixed Dominant') + 
  theme(axis.text = element_text(size = axis_ticks_size), 
        axis.title = element_text(size = axis_title_size), 
        title = element_text(size = title_size)) +
  theme(axis.title.x = element_text(vjust = -2))


# patchwork <- (A + B) / (C + D) / (E + G)
patchwork <- (A + B) / (C + D) / (E + G)  

patchwork

patchwork2 <- patchwork +
  plot_annotation(theme = theme(plot.margin = margin(0.5, 0, 0.5, 0.75, 'cm')))

patchwork2

# add lowercase letter tags
final_fig <- patchwork2 + plot_annotation(tag_levels = "a")

# save
ggsave(filename = 'fig2.png', 
       path = 'C://Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/',
       plot = patchwork2, 
       width = 7, 
       height = 7)
