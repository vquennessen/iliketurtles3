# set working directory
setwd("~/Projects/iliketurtles3")

# load libraries
library(ggplot2)

# temperatures
x <- seq(from = 20, to = 40, length = 1000)

# patricio et al.
t_piv1 <- 29.2
k1 <- -1.4
y1 <- 1/(1 + exp(-k1*(x - t_piv1)))

# embryogrowth (all greens)
t_piv2 <- 29.2
k2 <- -0.56
y2 <- 1/(1 + exp(-k2*(x - t_piv2)))

# make dataframe
TRN <- data.frame(Temperature = x,
                  Model = rep(c('West Africa', 'Suriname'), each = 1000),
                  Proportion_Male = c(y1, y2))

# plot
fig <- ggplot(data = TRN, aes(x = Temperature, y = Proportion_Male, 
                              col = Model, lty = Model)) +
  geom_line(lwd = 2) +
  geom_hline(yintercept = c(0.05, 0.50, 0.95), lty = 1, lwd = 1.5) +
  geom_vline(xintercept = t_piv1, col = 'darkgrey', lty = 1, lwd = 1.5) +
  # geom_line(data = TRN, aes(lwd = 2, lty = Model) +
  scale_linetype_manual(values = c('dotted', 'dotdash')) +
  geom_line(lwd = 2, aes(lty = '11')) +
  # scale_linetype_manual(values = c(3, 6)) +  
  ylab('Proportion hatchlings male') +
  xlab('Temperature (\u00B0C)') +
  # ggtitle('Thermal Reaction Norm') +
  theme_gray() +
  theme(axis.title.y = element_text(margin = margin(r = 15, l = 10)), 
        axis.title.x = element_text(margin = margin(t = 15, b = 10)), 
        # plot.title = element_text(margin = margin(b = 10, t = 10), 
        #                           size = 30), 
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 20), 
        legend.position = c(0.8, 0.7), 
        legend.key.width = unit(3.75, "line"))

ggsave("figures/thermal_reaction_norms.png", 
       plot = last_plot(), 
       height = 5, 
       width = 8)
