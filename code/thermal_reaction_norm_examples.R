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
  geom_vline(xintercept = t_piv1, col = 'darkgrey', lwd = 1, lty = 1) +
  geom_line(lwd = 2) +
  scale_color_manual(values = c('#F8766D', '#00BFC4')) +
  scale_linetype_manual(values = c(4, 5)) +
  ylab('Proportion hatchlings male') +
  xlab('Temperature (\u00B0C)') +
  theme_bw() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20),
        # legend.position = 'none', +
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.position = 'inside', 
        legend.position.inside = c(0.75, 0.7),
        legend.key.width = unit(8, "line")) +
  annotate("text", x = 39.4, y = 0.08, label = "0.05", size = 6) +
  annotate("text", x = 39.4, y = 0.53, label = "0.50", size = 6) +
  annotate("text", x = 39.4, y = 0.98, label = "0.95", size = 6) +
  annotate("text", x = 28.6, y = 0, label = "29.2", size = 6)


fig

ggsave("figures/thermal_reaction_norms.png", 
       plot = last_plot(), 
       height = 5, 
       width = 8)
