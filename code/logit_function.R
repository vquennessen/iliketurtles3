# west african population logit function for temperature to proportion male
# dr. ana caldras patricio

# set working directory
setwd("~/Projects/iliketurtles/code")

# load libraries
library(ggplot2)
library(dplyr)

# load in data
logit <- read.csv("C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/Dr Ana Caldas Patricio/logit_points.csv")

# pivotal temperature
diff <- abs(logit$p_males - 0.50)
pivot_temp <- logit$Temp[which(diff == min(diff))]

# plot points
ggplot(data = logit, aes(x = Temp, y = p_males)) +
  geom_vline(xintercept = pivot_temp, size = 1, col = 'gray40', lty = 2) +
  geom_path(col = 'darkblue', size = 2) +
  ylab("Proportion Male") +
  xlab("Temperature (\u00B0C)") +
  theme_grey() +
  theme(text = element_text(size = 20), 
        axis.title.y = element_text(margin = margin(r = 15, l = 10)), 
        axis.title.x = element_text(margin = margin(t = 15, b = 10))) +
  annotate("text", x = 30, y = 0.94, label = "29.2 \u00B0C", size = 6)

# find true k for logistic function
T_piv <- 29.2
#K <- 1.4
K <- seq(from = 1, to = 2, by = 0.001)
k_logit <- data.frame(k = rep(K, each = length(logit$Temp)),
                      temp = rep(logit$Temp, times = length(K)), 
                      AC_p_male = rep(logit$p_males, times = length(K)))
k_logit$Prop_male <- 1/(1 + exp(k_logit$k*(k_logit$temp - T_piv)))

k_logit %>%
  group_by(k) %>%
  mutate(diff = abs(Prop_male - AC_p_male)) %>%
  summarize(sum = sum(diff)) %>%
  arrange(sum)

# true k = 1.4

# plot different k values
T_piv <- 29.2
K <- seq(from = 0.7, to = 2.1, by = 0.1)
k_logit2 <- data.frame(k = rep(K, each = length(logit$Temp)),
                      temp = rep(logit$Temp, times = length(K))) 
k_logit2$Prop_male <- 1/(1 + exp(k_logit2$k*(k_logit2$temp - T_piv)))

ggplot(data = logit, aes(x = Temp, y = p_males)) +
  geom_vline(xintercept = pivot_temp, size = 1, col = 'gray40', lty = 2) +
  geom_path(col = 'darkblue', size = 2) +
  geom_line(data = k_logit2, aes(x = temp, y = Prop_male, 
                                 col = as.factor(k))) +
  ylab("Proportion Male") +
  xlab("Temperature (\u00B0C)") +
  theme_grey() +
  theme(text = element_text(size = 20), 
        axis.title.y = element_text(margin = margin(r = 15, l = 10)), 
        axis.title.x = element_text(margin = margin(t = 15, b = 10))) +
  annotate("text", x = 30, y = 0.94, label = "29.2 \u00B0C", size = 6)
