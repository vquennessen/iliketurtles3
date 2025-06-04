# table and/or heatmap of ideal incubation temperatures

# load libraries
library(ggplot2)
library(tidyverse)
library(readr)

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

# osrs
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
betas <- as.numeric(OSRs_to_betas(osrs))
# betas <- c(43.71, 20.64, 12.92, 9.02, 6.65, 5.03, 3.83, 2.87, 2.01, 1)

# ideal hatchling sex ratios based on mating system
IHSR <- (1.47 * osrs) / (1.47 * osrs + 3.87 * (1 - osrs))
# [1] 0.01960000 [2] 0.04049587 [3] 0.06282051 [4] 0.08672566 [5] 0.11238532 
# [6] 0.14000000 [7] 0.16980198 [8] 0.20206186 [9] 0.23709677 [10] 0.26737194

# temperatures that give those IHSRs based on thermal reaction norm
# narrow TRT population; k = -1.34, pivotal temp = 29.2
ITemps_narrow <- (log( 1 / IHSR - 1) + 1.34 * 29.2) / 1.34
# [1] 32.11972 [2] 31.56210 [3] 31.21686 [4] 30.95693 [5] 30.74224 
# [6] 30.55469 [7] 30.38435 [8] 30.22497 [9] 30.07214 [10] 29.95224

# wide TRT population, k = -0.561, pivotal temp = 29.2
ITemps_wide <- (log( 1 / IHSR - 1) + 0.561 * 29.2) / 0.561
# [1] 36.17403 [2] 34.84210 [3] 34.01746 [4] 33.39659 [5] 32.88379 
# [6] 32.43581 [7] 32.02893 [8] 31.64823 [9] 31.28318 [10] 30.99679

# ideal values
ideals <- data.frame(
  Min_OSR = as.numeric(osrs), 
  Ideal_Hatchling_Sex_Ratio = round(as.numeric(IHSR), 3), 
  Narrow_TRT = round(as.numeric(ITemps_narrow), 3),
  Wide_TRT = round(as.numeric(ITemps_wide), 3)
)

# save as table
write_csv(ideals, file = '../output/ideals.csv')

# make heatmap
ideal_temps_heatmap <- ideals %>%
  pivot_longer(cols = c(3, 4), 
               names_to = "Population", 
               values_to = "Temperature") %>%
  ggplot(aes(x = as.factor(Min_OSR), 
           y = Population, 
           fill = Temperature)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c() +
  labs(fill = "Temperature \n (\u00B0C)") +
  xlab("Minimum OSR required for 99% female reproductive success") +
  scale_y_discrete(labels = c("Narrow \n TRT    ", 
                              "Wide \n TRT  "
                              )) +
  theme_bw()

ideal_temps_heatmap

ggsave(ideal_temps_heatmap, 
       file = '~/Projects/iliketurtles3/figures/ideal_temps_heatmap.png', 
       height = 2, width = 7)
