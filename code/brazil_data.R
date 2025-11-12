# analysis of genetic info from brazil

# set working directory
setwd('~/Projects/iliketurtles3/code/')

# load libraries

# load data
BSR_Info_VIC_2025_MaleContribution <- read_csv(
  "~/Projects/iliketurtles3/data/BSR_Info_VIC_2025 - MaleContribution.csv")

# clean up data
clean_data <- BSR_Info_VIC_2025_MaleContribution %>%
  select(-c(7, 8)) %>%
  filter(LowNestSampleSize == 'No') %>%
  mutate(Season = substring(mom_nest_combo, 10, 11)) %>%
  mutate(Female = substring(mom_nest_combo, 5, 8)) %>%
  mutate(Nest = substring(mom_nest_combo, 13, 16)) 

##### how many males do females mate with in one season? #######################

males_per_female <- clean_data %>%
  filter(Season != 'S1') %>%
  select(Season, Female, dad) %>%
  group_by(Season, Female) %>%
  mutate(nMales = n())
