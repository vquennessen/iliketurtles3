# plot emergence success for different temperature scenarios over time

# set working directory
setwd('~/Projects/iliketurtles3/code/')

# remove environment variables
rm(list = ls())

# load libraries
library(dplyr)
library(ggplot2)
library(viridis)

# load in data
load("~/Projects/iliketurtles3/output/TS_b800_10y_n10000_all_outputs.Rdata")

# extract values from just one model and mating function
to_plot <- all_outputs %>%
  filter(TRT == 'Narrow transitional range') %>%
  filter(Beta == 1.17) %>%
  filter(Abundance == 'Hatchlings')

ggplot(data = to_plot, 
       aes(x = Year, y = Emergence_Success, col = Scenario)) +
  geom_line(lwd = 1) +
  # scale_color_manual(values = rocket(15)[1:10]) +
  scale_color_manual(values = turbo(12)[3:12]) +
  ylab('Emergence success \n P(eggs survive and emerge)') +
  labs(col = 'Temperature \n increase \n scenario (\u00B0C)')

ggsave(filename = 'emergence_success.png', 
       plot = last_plot(), 
       path = '~/Projects/iliketurtles3/figures/', 
       width = 6, height = 3.5)

##### plot emergence success ~ temp ############################################

A <- 0.86               # logistic by temp - A
k <- -1.7               # logistic by temp - beta
t0 <- 32.7              # logistic by temp - t0

temps <- seq(from = 25, to = 35, by = 0.1)

emergence_success <- A / (1 + exp(-k * (temps - t0)))

ES <- data.frame(Temperature = temps, 
                 Emergence_Success = emergence_success)

ggplot(ES, aes(x = Temperature, y = Emergence_Success)) +
  geom_path(lwd = 1, 
            col = turbo(12)[12]) +
  xlab('Temperature (\u00B0C)') +
  ylab('Emergence Success') +
  theme_bw() +
  geom_vline(xintercept = 33, col = 'red', lty = 2, lwd = 1) +
  geom_vline(xintercept = 35, col = 'red', lty = 1, lwd = 1)

ggsave(filename = 'emergence_success1.png', 
       plot = last_plot(), 
       path = '~/Projects/iliketurtles3/figures/', 
       width = 6, height = 3.5)
