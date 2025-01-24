# set working directory
setwd("~/Projects/iliketurtles3")

# load libraries
library(ggplot2)

# temperatures
x <- seq(from = 20, to = 40, length = 100)

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
                  Model = rep(c('West Africa', 'Suriname'), each = 100),
                  Proportion_Male = c(y1, y2))

# WA <- subset(TRN, Model == 'West Africa')
# SN <- subset(TRN, Model == 'Suriname')

# plot
fig <- ggplot(data = TRN, aes(x = Temperature, y = Proportion_Male, 
                              col = Model, lty = Model)) +
  geom_hline(yintercept = c(0.05, 0.50, 0.95), lwd = 1, lty = 1) +
  geom_vline(xintercept = t_piv1, col = 'darkgrey', lwd = 1, lty = 1) +
  geom_line(lwd = 2) +
  scale_color_manual(values = c('#F8766D', '#00BFC4')) +
  scale_linetype_manual(values = c(4, 5)) +
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
        legend.position = 'none')

        # legend.text = element_text(size = 15), 
        # legend.title = element_text(size = 20), 
        # legend.position = c(0.8, 0.7), 
        # legend.key.width = unit(9, "line"))

ggsave("figures/thermal_reaction_norms.png", 
       plot = last_plot(), 
       height = 5, 
       width = 8)
