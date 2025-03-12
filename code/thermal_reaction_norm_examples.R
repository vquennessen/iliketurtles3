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
                  Population = rep(c('West Africa', 'Suriname'), each = 100),
                  Proportion_Male = c(y1, y2))

# WA <- subset(TRN, Model == 'West Africa')
# SN <- subset(TRN, Model == 'Suriname')

# plot
fig <- ggplot(data = TRN, aes(x = Temperature, y = Proportion_Male, 
                              col = Population, lty = Population)) +
  geom_hline(yintercept = c(0.05, 0.50, 0.95), lwd = 1, lty = 1) +
  geom_vline(xintercept = 29.2, col = 'gray50', lwd = 1, lty = 1) +
  geom_vline(xintercept = 31.8, col = 'gray80', lwd = 1, lty = 1) +
  geom_vline(xintercept = 27, col = '#00BFC4', lty = 5, 
             lwd = 0.75, alpha = 0.5) +
  geom_vline(xintercept = 31.3, col = '#00BFC4', lty = 5, 
             lwd = 0.75, alpha = 0.5) +
  geom_vline(xintercept = 34.4, col = '#F8766D', lty = 4, 
             lwd = 0.75, alpha = 0.5) +
  geom_vline(xintercept = 23.8, col = '#F8766D', lty = 4, 
             lwd = 0.75, alpha = 0.5) +
  geom_line(lwd = 2) +
  scale_color_manual(values = c('#F8766D', '#00BFC4')) +
  scale_linetype_manual(values = c(4, 5)) +
  ylab('Proportion hatchlings male') +
  xlab('Temperature (\u00B0C)') +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.position = 'top',
        # legend.position = 'inside', 
        # legend.position.inside = c(0.8, 0.75),
        legend.key.width = unit(9.63, "line")) +
  annotate("text", x = 39.4, y = 0.1, label = "0.05", size = 5) +
  annotate("text", x = 39.4, y = 0.55, label = "0.50", size = 5) +
  annotate("text", x = 39.4, y = 1.00, label = "0.95", size = 5) +
  annotate("text", x = 28.1, y = 0.45, label = "29.2\u00B0C", size = 5) +
  annotate("text", x = 33, y = 1.00, label = "31.8\u00B0C", size = 5)


fig

ggsave("figures/thermal_reaction_norms.png", 
       plot = last_plot(), 
       height = 5, 
       width = 8)
