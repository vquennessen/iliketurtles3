# extract biological parameters from FdN data

# load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(reshape)

##### load in and clean data ###################################################

# set working directory
setwd('~/Projects/iliketurtles3/code')

load('../data/females.Rdata')
load('../data/nests.Rdata')

##### eggs per nest ############################################################

# remove nests with no total egg counts
nests_egg_counts <- nests %>%
  filter(!is.na(OVOS_TOT)) 

# scatterplot of eggs per nest colored by season
ggplot(data = nests_egg_counts, aes(x = as.factor(Season), 
                                    y = OVOS_TOT)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.4) + 
  geom_boxplot(alpha = 0.5, 
               aes(fill = as.factor(Season))) +
  labs(x = '\n Season', 
       y = 'Total eggs per nest \n') + 
  theme(legend.position = 'none')

# overlapping density plot, colored by season 
ggplot(data = nests_egg_counts, aes(x = OVOS_TOT, 
                                    fill = as.factor(Season), 
                                    alpha = 0.5)) + 
  geom_density(aes(group = as.factor(Season), 
                   alpha = 0.5)) +
  guides(alpha = 'none') +
  labs(x = '\n Eggs per nest', 
       y = 'Density \n',
       fill = 'Season')

# overall mean
mean(nests_egg_counts$OVOS_TOT) 
# 102.2625
# 100.6486

# overall median
median(nests_egg_counts$OVOS_TOT) 
# 101.5
# 100

# overall SD
sd(nests_egg_counts$OVOS_TOT) 
# 21.42993
# 23.08206

# average for each season
mean_eggs <- nests %>%
  group_by(Season) %>%
  summarise(Mean = mean(OVOS_TOT, na.rm = TRUE))
# season 1: 101
# season 2: 101
# season 3: 103
# Season 4: 98.2

# median for each season
median_eggs <- nests %>%
  group_by(Season) %>%
  summarise(Median = median(OVOS_TOT, na.rm = TRUE))
# season 1: 98
# season 2: 101
# season 3: 103
# Season 4: 100

# SD for each season
sd_eggs <- nests %>%
  group_by(Season) %>%
  summarise(SD = sd(OVOS_TOT, na.rm = TRUE))
# season 1: 27.1
# season 2: 18.8
# season 3: 20.9
# Season 4: 25.2

# SD and mean for all seasons minus season one
eggs_S2_S3_S4 <- nests %>%
  filter(Season != 2020) 

sd(eggs_S2_S3_S4$OVOS_TOT, na.rm = TRUE)
# 20.1344
# 22.60997

mean(eggs_S2_S3_S4$OVOS_TOT, na.rm = TRUE)
# 102.4625
# 100.5793

##### hatching success #########################################################

# remove nests with no hatching success calculated
nests_HS <- nests %>%
  filter(!is.na(Hatching_success)) 

# scatterplot of eggs per nest colored by season
ggplot(data = nests_HS, aes(x = as.factor(Season), 
                            y = Hatching_success)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.4) + 
  geom_boxplot(alpha = 0.5, 
               aes(fill = as.factor(Season))) +
  labs(x = '\n Season', 
       y = 'Hatching success \n') + 
  theme(legend.position = 'none')

# overlapping density plot, colored by season 
ggplot(data = nests_HS, aes(x = Hatching_success, 
                            fill = as.factor(Season), 
                            alpha = 0.5)) + 
  geom_density(aes(group = as.factor(Season), 
                   alpha = 0.5)) +
  guides(alpha = 'none') +
  labs(x = '\n Hatching Success', 
       y = 'Density \n',
       fill = 'Season')

# overall mean
mean(nests_egg_counts$Hatching_success) 
# 0.8150182
# 0.8241024

# overall median
median(nests_egg_counts$Hatching_success) 
# 0.8849624
# 0.897509

# overall SD
sd(nests_egg_counts$Hatching_success) 
# 0.191776
# 0.1960529

# average for each season
mean_HS <- nests %>%
  group_by(Season) %>%
  summarise(Mean = mean(Hatching_success, na.rm = TRUE))
# season 1: 0.835
# season 2: 0.782
# season 3: 0.829
# Season 4: 0.838

# median for each season
median_HS <- nests %>%
  group_by(Season) %>%
  summarise(Median = median(Hatching_success, na.rm = TRUE))
# season 1: 0.86 
# season 2: 0.847
# season 3: 0.903
# Season 4: 0.909

# SD for each season
sd_HS <- nests %>%
  group_by(Season) %>%
  summarise(SD = sd(Hatching_success, na.rm = TRUE))
# season 1: 0.119
# season 2: 0.185
# season 3: 0.212
# Season 4: 0.202

# all seasons but season 1
HS_S2_S3_S4 <- nests %>%
  filter(Season != 2020) 

# SD for all seasons minus season one
sd(HS_S2_S3_S4$Hatching_success, na.rm = TRUE)
# 0.2032503
# 0.2029219

# mean for all seasons minus season one
mu <- mean(HS_S2_S3_S4$Hatching_success, na.rm = TRUE)
# 0.8110467
# 0.8229106

# variance for all seasons minus season one
sigma_sq <- var(HS_S2_S3_S4$Hatching_success, na.rm = TRUE)
# 0.04131066
# 0.04117732

# calculate beta distribution parameters
alpha <- mu*((mu*(1 - mu))/sigma_sq - 1)
# 2.089414
beta <- alpha / mu - alpha
# 0.4496393



##### number of nests ##########################################################

# nests with females ID'd
nests_females <- nests %>%
  filter(!is.na(Female))

# number of nests for each female, including those where eggs were not counted
nest_counts <- count(nests_females, Female, Season)

# scatterplot of number of nests vs. female number
ggplot(data = nest_counts, aes(x = Female, y = n, col = as.factor(Season))) +
  geom_point() + 
  labs(y = 'Number of nests', 
       color = 'Season')

# excluding first season
nests_females_noS1 <- nests %>%
  filter(!is.na(Female)) %>%
  filter(Season != 2020)

# number of nests
nest_counts_noS1 <- count(nests_females_noS1, Female)

# scatterplot of number of nests vs. female number without Season 1
ggplot(data = nest_counts_noS1, aes(x = Female, y = n)) +
  geom_point()

# boxplot by season
# scatterplot of eggs per nest colored by season
ggplot(data = nest_counts, aes(x = as.factor(Season), y = n)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.4) + 
  geom_boxplot(alpha = 0.5, 
               aes(fill = as.factor(Season))) +
  labs(x = '\n Season', 
       y = 'Number of nests \n') + 
  theme(legend.position = 'none')

# overlapping density plot, colored by season 
ggplot(data = nest_counts, aes(x = n, fill = as.factor(Season), alpha = 0.5)) + 
  geom_density(aes(group = as.factor(Season), 
                   alpha = 0.5)) +
  guides(alpha = 'none') +
  labs(x = '\n Number of nests', 
       y = 'Density \n',
       fill = 'Season')

# overall mean
mean(nest_counts$n) 
# 4.464912
# 4.635838

# overall median
median(nest_counts$n) 
# 5

# overall SD
sd(nest_counts$n) 
# 2.053186
# 2.1214

# average for each season
mean_nests <- nest_counts %>%
  group_by(Season) %>%
  summarise(Mean = mean(n, na.rm = TRUE))
# season 1: 3.73
# season 2: 4.36
# season 3: 5.34
# Season 4: 4.95

# median for each season
median_nests <- nest_counts %>%
  group_by(Season) %>%
  summarise(Median = median(n, na.rm = TRUE))
# season 1: 3
# season 2: 5
# season 3: 5
# Season 4: 5

# SD for each season
sd_nests <- nest_counts %>%
  group_by(Season) %>%
  summarise(SD = sd(n, na.rm = TRUE))
# season 1: 2.02
# season 2: 1.81
# season 3: 1.96
# Season 4: 2.26

# SD for all seasons minus season one
nests_S2_S3_S4 <- nest_counts %>%
  filter(Season != 2020) 

sd(nests_S2_S3_S4$n, na.rm = TRUE)
# 1.94
# 2.089752

# mean for all seasons minus season one
mean(nests_S2_S3_S4$n, na.rm = TRUE)
# 4.942029
# 4.945312

# median for all seasons minus season one
median(nests_S2_S3_S4$n, na.rm = TRUE)
# 5

##### remigration interval for females #########################################

# for each female
female_seasons <- nests_females %>%
  select(Female, Season) %>%
  group_by(Female) %>%
  mutate(Seasons = list(unique(Season))) %>%
  group_by(Female) %>%
  mutate(Interval = c(max(as.numeric(unlist(Seasons))) - min(as.numeric(unlist(Seasons))))) %>%
  filter(Interval > 0) %>%
  group_by(Female) %>%
  summarise(mean(Interval))

# 2 years: females 11 and 43 (2)
# 3 years: females 1, 21, 35, 39 (4)

# overall mean remigration interval
mean(female_seasons$`mean(Interval)`)
# 2.666666667 (2 and 2/3 = 8/3)

##### number of males captured - 39 ############################################

male_captures <- read.csv('../data/2022_2023_in_Water.csv')

# num_males <- nrow(subset(male_captures, Tag.status == 'New recruit'))
# num_males

# take 2

tag1 <- gsub('BRA ', '', male_captures$Flipper_Tag_.1)
tag1.1 <- na.omit(as.numeric(gsub('BR ', '', tag1)))

tag2 <- gsub('BRA ', '', male_captures$Flipper_Tag_.2)
tag2.1 <- c(na.omit(as.numeric(gsub('BR ', '', tag2))), 
            rep(0, length(tag1.1) - length(tag2.1)))

sums <- tag1.1 + tag2.1

# double check that same sums are from same 
new_males <- data.frame(Tag1 = tag1.1, 
                        Tag2 = tag2.1, 
                        sum = sums)

length(sums)
length(unique(sums))
# 39 unique males

##### hatchling production #####################################################

load('../data/nests.Rdata')

hatchlings <- nests %>%
  #filter(Season != 2020) %>%
  group_by(Season) %>%
  summarize(nests = n(), 
            eggs = sum(OVOS_TOT, na.rm = TRUE),
            hatchlings = sum(VIVOS, na.rm = TRUE)) %>%
  mutate(avg_hatchlings = nests*100.6486*0.8241024)

# mean hatchlings = 14681, 10451, 18580, 24386


##### number of nests destroyed / predated by season ###########################

noS1 <- nests %>%
  # filter(Season != 2020) %>%
  filter(!is.na(HIST_NINHO)) %>%
  filter(HIST_NINHO != '') %>%
  group_by(Season, HIST_NINHO) %>%
  summarize(n = n())
