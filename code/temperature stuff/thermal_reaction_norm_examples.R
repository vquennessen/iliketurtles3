# load libraries
library(ggplot2)

# temperatures
x <- seq(from = 20, to = 40, length = 1000)

# patricio et al.
t_piv1 <- 29.2
k1 <- -1.4
y1 <- 1/(1 + exp(k1*(x - t_piv1)))

# embryogrowth (all greens)
t_piv2 <- 29.4
k2 <- -0.56
y2 <- 1/(1 + exp(k2*(x - t_piv2)))

# make dataframe
TRN <- data.frame(Temperature = x,
                  Model = rep(c('Patricio', 'embryogrowth'), each = 1000),
                  Proportion_Male = c(y1, y2))

# plot
ggplot(data = TRN, aes(x = Temperature, y = Proportion_Male, col = Model)) +
  geom_line() +
  geom_hline(yintercept = c(0.05, 0.50, 0.95), lty = 2) 
