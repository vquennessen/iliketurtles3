# plot deterministic SAD proportions over time

rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# one plot for each model and beta
models <- c('P_base', 'GM_base')
# models <- c('P_base')

betas <- c(1.17, 2, 2.86, 3.82, 5.02, 6.64, 9.01, 12.91, 20.63, 43.7)
# betas <- c(20.63, 43.7)

burn_in <- 800
temp_stochasticity <- TRUE
TS <- ifelse(temp_stochasticity == TRUE, 'TS_', '')

# load SAD object
load(
  paste('~/Projects/iliketurtles3/output/SAD_deterministic_', TS, 'b', burn_in, 
        '.Rdata', sep = ''))

for (m in 1:length(models)) {
  
    model <- models[m]
  
  for (b in 1:length(betas)) {
    
    beta <- betas[b]
    
    A <- SADdf %>%
      filter(Model == model) %>%
      filter(Beta == beta) %>%
      # filter(Year > 749) %>%
      ggplot(aes(x = Year, y = Proportion, col = factor(Age))) +
      geom_path() +
      facet_wrap(facets = vars(Sex), 
                 scales = 'free') +
      guides(col = 'none')
    
    B <- SADdf %>%
      filter(Model == model) %>%
      filter(Beta == beta) %>%
      group_by(Model, Beta, Sex, Age) %>%
      # filter(Year > 749) %>%
      ggplot(aes(x = Year, y = PSR)) +
      geom_path() +
      ylim(c(0, 0.2)) +
      ggtitle('PSR')
    
    C <- SADdf %>%
      filter(Model == model) %>%
      filter(Beta == beta) %>%
      group_by(Model, Beta, Sex, Age) %>%
      # filter(Year > 749) %>%
      ggplot(aes(x = Year, y = OSR)) +
      geom_path() +
      ylim(c(0, 0.7)) +
      ggtitle('OSR')
    
    fig <- A + (B / C) +
      plot_layout(widths = c(12, 6))

    ggsave(filename = paste('SAD_deterministic_', TS, 'b', burn_in, '_',
                            model, '_beta', beta, '.png', sep = ''), 
           fig, 
           path = '~/Projects/iliketurtles3/figures/SAD deterministic', 
           width = 18, height = 12)
    
    # ggsave(filename = paste('SAD_deterministic_', burn_in, ' burn-in_', 
    #                         model, '_beta', beta, '.png', sep = ''), 
    #        fig, 
    #        path = '~/Projects/iliketurtles3/figures/SAD deterministic')
    
  }
  
}

# # plot the thing - p base
# 
# p2 <- SADdf %>%
#   filter(Model == 'P_base') %>%
#   filter(Age > 20, Age < 50) %>%
#   filter(Year > 700) %>%
#   filter(Beta == 2) %>%
#   filter(Sex == 'MM') 
# 
# ggplot(data = p2, aes(x = Year, y = Abundance, col = factor(Age))) +
#   geom_line()
# 
# ggplot(data = p2, aes(x = Year, y = Proportion, col = factor(Age))) +
#   geom_path()
# 
# SADdf %>%
#   filter(Model == 'P_base') %>%
#   filter(Beta == 2) %>%
#   ggplot(aes(x = Year, y = Proportion, col = factor(Age))) +
#   geom_path() +
#   facet_wrap(vars(Sex), scales = 'free')
# 
# totals <- SADdf %>%
#   filter(Sex == 'MM') %>%
#   filter(Model == 'P_base') %>%
#   filter(Beta == 2) %>%
#   group_by(Model, Beta, Year) %>%
#   summarise(total_abundance = sum(Abundance))
# 
#   ggplot(data = totals,
#          aes(x = Year, y = total_abundance)) +
#   geom_point()
#   
# 
#     # facet_wrap(vars(Sex), scales = 'free')

# differences between deterministic with and without temp stochasticity?
load("~/Projects/iliketurtles3/output/SAD_deterministic_b800.Rdata")
D800 <- SADdf

load("~/Projects/iliketurtles3/output/SAD_deterministic_TS_b800.Rdata")
TS800 <- SADdf

diffDF <- D800 %>%
  mutate(diff_proportion = D800$Proportion - TS800$Proportion) %>%
  mutate(diff_PSR = D800$PSR - TS800$PSR) %>%
  mutate(diff_OSR = D800$OSR - TS800$OSR) %>%
  mutate(diff_abundance = D800$Abundance - TS800$Abundance)
  

# difference in proportions
diffDF %>%
  filter(Year == max(Year)) %>%
  filter(Sex == 'IF') %>%
  ggplot(aes(x = Age, 
             y = diff_proportion, 
             col = factor(Beta), 
             lty = Model)) +
  geom_line(lwd = 1) +
  ylim(c(-0.001, 0.005))
  # facet_wrap(vars(Sex))

# difference in PSR
diffDF %>%
  filter(Year == max(Year)) %>%
  ggplot(aes(x = Age, y = diff_PSR, col = factor(Beta))) +
  geom_point() +
  facet_wrap(vars(Model))

# deterministic SAD has lower PSR, bigger difference for GM base
