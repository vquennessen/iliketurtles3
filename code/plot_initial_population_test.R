# plot initialize population test results

# set working directory
setwd('~/Projects/iliketurtles3/code')

# load libraries
library(dplyr)
library(magrittr)
library(ggplot2)

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

DF <- SAD %>%
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
