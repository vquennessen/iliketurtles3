# plot initialize population test results

# set working directory
setwd('~/Projects/iliketurtles3/code')

# load libraries
library(dplyr)
library(magrittr)
library(ggplot2)

# plotting parameters
Nsims <- c(100, 1000, 10000)
Burn_ins <- c(100, 200, 300)

SAD <- data.frame(Model = NULL, 
                  Beta = NULL, 
                  Sex = NULL, 
                  Age = NULL, 
                  Abundance = NULL,
                  Proportion = NULL,
                  Adjusted_Abundance = NULL, 
                  prop_median = NULL,
                  prop_Q25 = NULL,
                  prop_Q75 = NULL,
                  abundance_median = NULL,
                  abundance_Q25 = NULL,
                  abundance_Q75 = NULL)

for (n in 1:length(Nsims)) {
  
  nsims <- Nsims[n]
  
  for (b in 1:length(Burn_ins)) {
    
    burn_in <- Burn_ins[b]
    
    # # make facet panels have consistent y axes by sex across runs
    # facet_bounds <- read.table(header = TRUE,
    #                            text =  
    #                              "Sex   ymin  ymax       breaks
    #                           IF    0     0.4        3
    #                           IM    0     0.05       3
    #                           MF    0     0.00175    2
    #                           MM    0     0.000125   2",
    #                            stringsAsFactors = FALSE)
    # 
    # xylims <- with(facet_bounds,
    #                data.frame(prop_median = c(ymin, ymax),
    #                           Sex = c(Sex, Sex), 
    #                           Model = 'P_base', 
    #                           Beta = 1.17))
    
    # load in object
    
    if (file.exists(paste('~/Projects/iliketurtles3/output/SAD_n', nsims, 
                          '_b', burn_in, '.Rdata', sep = ''))) {
      
      load(paste('~/Projects/iliketurtles3/output/SAD_n', nsims, 
                 '_b', burn_in, '.Rdata', sep = ''))
      
      # build dataframe with median, IQR, etc.
      DF <- SADdf %>%
        mutate(Nsims = nsims) %>%
        mutate(Burn_in = burn_in) %>%
        filter(!is.na(Proportion)) %>%
        filter(!is.na(Abundance)) %>%
        group_by(Beta, Sex, Age, Model, Nsims, Burn_in) %>%
        summarise(prop_median = median(Proportion), 
                  prop_Q25 = quantile(Proportion, probs = 0.25), 
                  prop_Q75 = quantile(Proportion, probs = 0.75), 
                  abundance_median = median(Abundance), 
                  abundance_Q25 = quantile(Abundance, probs = 0.25), 
                  abundance_Q75 = quantile(Abundance, probs = 0.75), 
                  .groups = 'drop') %>%
        mutate(Nsims = paste('nsims = ', Nsims, sep = '')) %>%
        mutate(Burn_in = paste('burn-in = ', Burn_in, sep = ''))
      
      SAD <- rbind(SAD, DF)
      
    }
    
  }
  
}

sexes <- c('IF', 'IM', 'MF', 'MM')

for (s in 1:length(sexes)) {
  
  sex <- sexes[s]
  
  to_plot <- SAD %>%
    filter(Sex == sex)
  
  ### proportions
  Props <- ggplot(data = to_plot, 
                  aes(x = Age, 
                      y = prop_median, 
                      lty = Model,
                      col = factor(Beta)
                  )) +
    facet_grid(rows = vars(Nsims), 
               cols = vars(factor(Burn_in))) +
    geom_line() +
    geom_ribbon(aes(ymin = prop_Q25,
                    ymax = prop_Q75,
                    col = NULL,
                    fill = factor(Beta)),
                alpha = 0.15,
                show.legend = FALSE) +
    # geom_point(data = xylims, x = NA) +
    ggtitle(paste('sex = ', sex, sep = ''))
  
  Props
  
  # ggsave(filename = paste('props_SAD_n', nsims, '_b', burn_in, '.png', sep = ''),
  #        plot = Props, 
  #        path = '~/Projects/iliketurtles3/figures/SAD', 
  #        width = 10, height = 7)
  
  ggsave(filename = paste('SAD_props_', sex, '.png', sep = ''),
         plot = Props,
         path = '~/Projects/iliketurtles3/figures/SAD',
         width = 10, height = 7)
  
  ### abundances
  Abundances <- ggplot(data = to_plot, 
                       aes(x = Age, 
                           y = abundance_median, 
                           lty = Model, 
                           col = factor(Beta))) +
    facet_grid(rows = vars(Nsims), 
               cols = vars(factor(Burn_in))) +
    geom_line() +
    geom_ribbon(aes(ymin = abundance_Q25,
                    ymax = abundance_Q75,
                    col = NULL,
                    fill = factor(Beta)),
                alpha = 0.15,
                show.legend = FALSE) +
    ggtitle(paste('sex = ', sex, sep = ''))
  
  
  Abundances
  
  # ggsave(filename = paste('abundances_SAD_n', nsims, '_b', burn_in, '.png', 
  #                         sep = ''),
  #        plot = Abundances, 
  #        path = '~/Projects/iliketurtles3/figures/SAD', 
  #        width = 10, height = 7)
  
  ggsave(filename = paste('SAD_abundances_', sex, '.png', sep = ''),
         plot = Abundances,
         path = '~/Projects/iliketurtles3/figures/SAD',
         width = 10, height = 7)
  
}
