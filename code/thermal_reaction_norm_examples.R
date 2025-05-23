# set working directory
setwd("~/Projects/iliketurtles3")

# load libraries
library(ggplot2)

# emergence success variables
hatch_success_A <- 0.86                   # logistic by temp - A
hatch_success_k <- -1.7                   # logistic by temp - beta
hatch_success_t0 <- 32.7                  # logistic by temp - t0

# temperatures
x <- seq(from = 20, to = 40, by = 0.01)

# patricio et al.
t_piv1 <- 29.2
# k1 <- -1.4 # Patricio et al 2017, TRT 
k1 <- -1.34
y1 <- round(1/(1 + exp(-k1*(x - t_piv1))), 5)

# embryogrowth (all greens)
t_piv2 <- 29.2
k2 <- -0.561
y2 <- round(1/(1 + exp(-k2*(x - t_piv2))), 5)

# emergence success
# ES <- hatch_success_A / (1 + exp(-hatch_success_k * (x - hatch_success_t0)))


# make dataframe
TRN <- data.frame(Temperature = x,
                  Population = rep(c('West Africa', 
                                     'Suriname'
                                     # 'Emergence Success'
                                     ), each = length(x)),
                  Proportion_Male = c(y1, 
                                      y2 
                                      # ES
                                      ))

# WA <- subset(TRN, Model == 'West Africa')
# SN <- subset(TRN, Model == 'Suriname')

# plot
fig <- ggplot(data = TRN, aes(x = Temperature, y = Proportion_Male, 
                              col = Population, lty = Population)) +
  geom_hline(yintercept = c(0.05, 0.50, 0.95), lwd = 1, lty = 1) +
  geom_vline(xintercept = 29.2, col = 'black', lwd = 1.5, lty = 1) +
  geom_vline(xintercept = 30.5, col = 'gray60', lwd = 1, lty = 1) +
  geom_vline(xintercept = 27, col = '#6600FF', lty = 5, 
             lwd = 0.75, alpha = 0.5) +
  geom_vline(xintercept = 31.4, col = '#6600FF', lty = 5, 
             lwd = 0.75, alpha = 0.5) +
  geom_vline(xintercept = 34.4, col = '#FF3300', lty = 4, 
             lwd = 0.75, alpha = 0.5) +
  geom_vline(xintercept = 23.8, col = '#FF3300', lty = 4, 
             lwd = 0.75, alpha = 0.5) +
  geom_line(lwd = 2) +
  scale_color_manual(values = c(
    # '#33CC33', 
                                '#FF3300', '#6600FF')) +
  scale_linetype_manual(values = c(
    # 1, 
    4, 5)) +
  ylab('Proportion hatchlings male \n Hatchling emergence success') +
  xlab('Temperature (\u00B0C)') +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 15),
        # legend.text = element_text(size = 12),
        # legend.title = element_text(size = 15),
        # legend.position = 'top',
        # # legend.position = 'inside', 
        # # legend.position.inside = c(0.8, 0.75),
        # legend.key.width = unit(9.63, "line"), 
        legend.position = 'none') +
  annotate("label", x = 39.4, y = 0.1, label = "0.05", size = 5, 
           label.size = 0) +
  annotate("label", x = 39.4, y = 0.55, label = "0.50", size = 5, 
           label.size = 0) +
  annotate("label", x = 39.4, y = 1.00, label = "0.95", size = 5, 
           label.size = 0)
  # annotate("label", x = 28.4, y = 0, label = "29.2\u00B0C", size = 5, 
  #          label.size = 0) +
  # annotate("label", x = 32.65, y = 1, label = "30.5\u00B0C", size = 5, 
  #          label.size = 0)


fig


# ggsave("figures/thermal_reaction_norms.png", 
#        plot = last_plot(), 
#        height = 5, 
#        width = 8)
