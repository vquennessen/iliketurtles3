# plot initialize population test results

# set working directory
setwd('~/Projects/iliketurtles3/code')

# load libraries
library(dplyr)
library(magrittr)
library(ggplot2)

# how many sims
nsims <- 1000

# how many burn-in years
burn_in <- 

load("~/Projects/iliketurtles3/output/SAD_n5.Rdata")
raw_N5B100 <- SAD

load("~/Projects/iliketurtles3/output/SAD_n1000_b100.Rdata")
raw_N1000B100 <- SAD

##### nsims 5 burn-in 100 ######################################################
N5B100 <- raw_N5B100 %>%
  group_by(Model, Beta, Sex, Age) %>%
  mutate(prop_median = median(Proportion), 
         prop_Q25 = quantile(Proportion, probs = 0.25), 
         prop_Q75 = quantile(Proportion, probs = 0.75), 
         abundance_median = median(Abundance), 
         abundance_Q25 = quantile(Abundance, probs = 0.25), 
         abundance_Q75 = quantile(Abundance, probs = 0.75))

### proportions
N5B100_props <- ggplot(data = N5B100, 
                       aes(x = Age, 
                           y = prop_median, 
                           col = Model, 
                           lty = factor(Beta))) +
  facet_wrap(facets = vars(Sex), 
             scale = 'free') +
  geom_line() +
  # ylim(c(0, 1)) +
  geom_ribbon(aes(ymin = prop_Q25,
                  ymax = prop_Q75,
                  col = NULL,
                  fill = factor(Beta)),
              alpha = 0.25,
              show.legend = FALSE)

N5B100_props

### abundances
N5B100_abundance <- ggplot(data = N5B100, 
                           aes(x = Age, 
                               y = abundance_median, 
                               col = Model, 
                               lty = factor(Beta))) +
  facet_wrap(facets = vars(Sex), 
             scale = 'free') +
  geom_line() +
  geom_ribbon(aes(ymin = abundance_Q25,
                  ymax = abundance_Q75,
                  col = NULL,
                  fill = factor(Beta)),
              alpha = 0.25,
              show.legend = FALSE)

N5B100_abundance

##### nsims 1000 burn-in 100 ###################################################

N1000B100 <- raw_N1000B100 %>%
  group_by(Model, Beta, Sex, Age) %>%
  summarise(prop_median = median(Proportion), 
         prop_Q25 = quantile(Proportion, probs = 0.25), 
         prop_Q75 = quantile(Proportion, probs = 0.75), 
         abundance_median = median(Abundance), 
         abundance_Q25 = quantile(Abundance, probs = 0.25), 
         abundance_Q75 = quantile(Abundance, probs = 0.75), 
         .groups = 'drop')

### proportions
N1000B100_props <- ggplot(data = N1000B100, 
                          aes(x = Age, 
                              y = prop_median, 
                              col = Model, 
                              lty = factor(Beta))) +
  facet_wrap(facets = vars(Sex), 
             scale = 'free') +
  geom_line() +
  # ylim(c(0, 1)) +
  geom_ribbon(aes(ymin = prop_Q25,
                  ymax = prop_Q75,
                  col = NULL,
                  fill = factor(Model)),
              alpha = 0.25,
              show.legend = FALSE)

N1000B100_props

### abundances
N1000B100_abundance <- ggplot(data = N1000B100, 
                              aes(x = Age, 
                                  y = abundance_median, 
                                  col = Model, 
                                  lty = factor(Beta))) +
  facet_wrap(facets = vars(Sex), 
             scale = 'free') +
  geom_line() +
  geom_ribbon(aes(ymin = abundance_Q25,
                  ymax = abundance_Q75,
                  col = NULL,
                  fill = factor(Model)),
              alpha = 0.1,
              show.legend = FALSE)

N1000B100_abundance

##### general SAD figs #########################################################

nsims <- 1000
burn_in <- 200

load(paste('~/Projects/iliketurtles3/output/SAD_n', 
           nsims, '_b', burn_in, '.Rdata', sep = ''))

DF <- SAD

# load("~/Projects/iliketurtles3/output/SAD_n1000_b200_P_base_beta1.17.Rdata")
# P1 <- SAD
# P1$Num_sims <- 1000
# P1$Burn_in <- 200
# load("~/Projects/iliketurtles3/output/SAD_n1000_b200_P_base_beta2.86.Rdata")
# P2 <- SAD
# P2$Num_sims <- 1000
# P2$Burn_in <- 200
# load("~/Projects/iliketurtles3/output/SAD_n1000_b100_P_base_beta1.17.Rdata")
# P3 <- SAD
# P3$Num_sims <- 1000
# P3$Burn_in <- 100
# load("~/Projects/iliketurtles3/output/SAD_n1000_b100_P_base_beta2.86.Rdata")
# P4 <- SAD
# P4$Num_sims <- 1000
# P4$Burn_in <- 100
# 
# 
# DF <- rbind(P1, P2, P3, P4)

DF_summarized <- DF %>%
  filter(!is.na(Proportion)) %>%
  filter(!is.na(Abundance)) %>%
  # group_by(Model, Beta, Sex, Age) %>%
  group_by(Beta, Sex, Age, Burn_in) %>%
  summarise(prop_median = median(Proportion), 
            prop_Q25 = quantile(Proportion, probs = 0.25), 
            prop_Q75 = quantile(Proportion, probs = 0.75), 
            abundance_median = median(Abundance), 
            abundance_Q25 = quantile(Abundance, probs = 0.25), 
            abundance_Q75 = quantile(Abundance, probs = 0.75), 
            .groups = 'drop')

### proportions
Props <- ggplot(data = P_all_summarized, 
                          aes(x = Age, 
                              y = prop_median, 
                              # col = Model, 
                              # lty = factor(Beta)
                              col = factor(Beta), 
                              lty = factor(Burn_in)
                              )) +
  facet_wrap(facets = vars(Sex), 
             scale = 'free') +
  geom_line() +
  # ylim(c(0, 1)) +
  geom_ribbon(aes(ymin = prop_Q25,
                  ymax = prop_Q75,
                  col = NULL,
                  # fill = factor(Model)),
                  fill = factor(Beta)),
              
              alpha = 0.25,
              show.legend = FALSE)

Props

### abundances
N1000B100_abundance <- ggplot(data = N1000B100, 
                              aes(x = Age, 
                                  y = abundance_median, 
                                  col = Model, 
                                  lty = factor(Beta))) +
  facet_wrap(facets = vars(Sex), 
             scale = 'free') +
  geom_line() +
  geom_ribbon(aes(ymin = abundance_Q25,
                  ymax = abundance_Q75,
                  col = NULL,
                  fill = factor(Model)),
              alpha = 0.1,
              show.legend = FALSE)

N1000B100_abundance
