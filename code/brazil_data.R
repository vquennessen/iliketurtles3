# analysis of genetic info from brazil

# set working directory
setwd('~/Projects/iliketurtles3/code/')

# load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(e1071)
library(vcd)
library(MASS)
library(EnvStats)
library(goft)
library(fitdistrplus)
library(gofgamma)
library(extraDistr)

# load data
BSR_Info_VIC_2025_MaleContribution <- read_csv(
  "~/Projects/iliketurtles3/data/BSR_Info_VIC_2025 - MaleContribution.csv")

# clean up data
clean_data <- BSR_Info_VIC_2025_MaleContribution %>%
  dplyr::select(mom_nest_combo, dad, hatchling_count, NestTotalHatchCount, 
         propdadcontribute, LowNestSampleSize) %>%
  filter(LowNestSampleSize == 'No') %>%
  mutate(Season = substring(mom_nest_combo, 10, 11)) %>%
  mutate(Female = substring(mom_nest_combo, 5, 8)) %>%
  mutate(Nest = substring(mom_nest_combo, 13, 16)) 

##### are all males represented in all clutches? ###############################
# NA, no more than one clutch represented per female per season # # # # # # # # 
males_across_clutches <- clean_data %>%
  group_by(Season, Female) %>%
  filter(length(unique(Nest)) > 1)

##### how many males do females mate with in one season? #######################

males_per_female <- clean_data %>%
  dplyr::select(Season, Female) %>%
  group_by(Season, Female) %>%
  mutate(nMales = n())

round(prop.table(table(males_per_female$nMales)), 2)
#    1    2    3    4    6    7 
# 0.09 0.30 0.12 0.36 0.08 0.05 

hist(males_per_female$nMales)

summary(males_per_female$nMales)
#    Min. 1st Qu.  Median   Mean  3rd Qu.    Max. 
#  1.000   2.000   3.000   3.315   4.000   7.000 
var(males_per_female$nMales)
# 2.437978
sd(males_per_female$nMales)
# 1.561403
skewness(males_per_female$nMales)
# 0.6068969
kurtosis(males_per_female$nMales)
# -0.1987063

##### what distribution works best for modeling ################################

shapiro.test(males_per_female$nMales)
# data:  males_per_female$nMales
# W = 0.88836, p-value = 4.397e-09

fit_poisson <- goodfit(males_per_female$nMales, type = 'poisson')
summary(fit_poisson)
#                       X^2 df     P(> X^2)
# Likelihood Ratio 87.49267  4 4.487101e-18

fit_binomial <- goodfit(males_per_female$nMales, type = 'binomial')
summary(fit_binomial)
#                       X^2 df     P(> X^2)
# Likelihood Ratio 97.32354  4 3.651611e-20

fit_nbinomial <- goodfit(males_per_female$nMales, type = 'nbinomial')
summary(fit_nbinomial)
#                       X^2 df     P(> X^2)
# Likelihood Ratio 87.98148  3 5.943666e-19

fit_gamma <- fitdistr(males_per_female$nMales, 'gamma', lower = c(0, 0))
# shape       rate   
# 4.3324323   1.3068912 
# (0.4888012) (0.1563366)

fit_gamma2 <- gofTest(males_per_female$nMales, distribution = 'gamma')
# P-value: 8.350986e-08
# Alternative Hypothesis: True cdf does not equal the Gamma Distribution.

fit_gamma3 <- gamma_test(males_per_female$nMales)
# V = -0.97988, p-value = 0.4884

# # # # # BEST FIT # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
fit_gamma4 <- fitdist(males_per_female$nMales, 'gamma', discrete = TRUE)
summary(fit_gamma4)
# Loglikelihood:  -263.2182   AIC:  530.4363   BIC:  536.4035 

hist(males_per_female$nMales, freq = FALSE)
lines(density(males_per_female$nMales), col = 'blue', lwd = 2)
curve(dgamma(x, 
             shape = fit_gamma$estimate['shape'], 
             rate = fit_gamma$estimate['rate']), 
      add = TRUE, col = 'red', lwd = 2)

plot(fit_gamma4)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

fit_weibull <- fitdist(males_per_female$nMales, 'weibull')
summary(fit_weibull)
# Loglikelihood:  -264.3671   AIC:  532.7342   BIC:  538.7014

fit_lnorm <- fitdist(males_per_female$nMales, 'lnorm')
summary(fit_lnorm)
# Loglikelihood:  -266.3124   AIC:  536.6249   BIC:  542.5921

fit_norm <- fitdist(males_per_female$nMales, 'norm')
summary(fit_norm)
# Loglikelihood:  -271.7187   AIC:  547.4373   BIC:  553.4045

fit_poisson2 <- fitdist(males_per_female$nMales, 'pois')
summary(fit_poisson2)
# Loglikelihood:  -270.5726   AIC:  543.1452   BIC:  546.1288 

fit_geom <- fitdist(males_per_female$nMales, 'geom')
summary(fit_geom)
# Loglikelihood:  -341.0678   AIC:  684.1356   BIC:  687.1193 

fit_nbinom2 <- fitdist(males_per_female$nMales, 'nbinom')
summary(fit_nbinom2)
# Loglikelihood:  -270.5726   AIC:  545.1452   BIC:  551.1124 

fit_exp <- fitdist(males_per_female$nMales, 'exp')
summary(fit_exp)
# Loglikelihood:  -320.9778   AIC:  643.9557   BIC:  646.9393 

fit_logis <- fitdist(males_per_female$nMales, 'logis')
summary(fit_logis)
# Loglikelihood:  -273.1424   AIC:  550.2847   BIC:  556.2519 



##### does contribution relate to number of males total? #######################

contributions <- clean_data %>%
  group_by(Season, Female, Nest) %>%
  mutate(nMales = n()) %>%
  mutate(c1 = sort(propdadcontribute, decreasing = TRUE)[1]) %>%
  mutate(c2 = sort(propdadcontribute, decreasing = TRUE)[2]) %>%
  mutate(c3 = sort(propdadcontribute, decreasing = TRUE)[3]) %>%
  mutate(c4 = sort(propdadcontribute, decreasing = TRUE)[4]) %>%
  mutate(c5 = sort(propdadcontribute, decreasing = TRUE)[5]) %>%
  mutate(c6 = sort(propdadcontribute, decreasing = TRUE)[6]) %>%
  mutate(c7 = sort(propdadcontribute, decreasing = TRUE)[7]) %>%
  select(Season, Female, Nest, nMales, c1, c2, c3, c4, c5, c6, c7)

mod1 <- lm(c1 ~ nMales + 0, data = contributions)  
summary(mod1)
# Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
# nMales 0.150639   0.009999   15.07   <2e-16 ***
# Multiple R-squared:  0.6102,	Adjusted R-squared:  0.6075 

mod2 <- lm(c2 ~ nMales + 0, data = contributions)
summary(mod2)
# Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
# nMales 0.051476   0.002816   18.28   <2e-16 ***
# Multiple R-squared:  0.7168,	Adjusted R-squared:  0.7147 

mod3 <- lm(c3 ~ nMales + 0, data = contributions)
summary(mod3)
# Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
# nMales 0.027419   0.001513   18.13   <2e-16 ***
# Multiple R-squared:  0.7887,	Adjusted R-squared:  0.7863 

mod4 <- lm(c4 ~ nMales + 0, data = contributions)
summary(mod4)
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# nMales 0.0156406  0.0007664   20.41   <2e-16 ***
# Multiple R-squared:  0.8561,	Adjusted R-squared:  0.8541 

mod5 <- lm(c5 ~ nMales + 0, data = contributions)
summary(mod5)
# Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
# nMales 0.010521   0.001377   7.639  4.7e-07 ***
# Multiple R-squared:  0.7643,	Adjusted R-squared:  0.7512 

mod6 <- lm(c6 ~ nMales + 0, data = contributions)
summary(mod6)
# Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
# nMales 0.006948   0.000524   13.26 9.96e-11 ***
# Multiple R-squared:  0.9071,	Adjusted R-squared:  0.902 

# mod7 <- lm(c7 ~ nMales + 0, data = contributions)
# summary(mod7)
# Warning message:
# In summary.lm(mod7) : essentially perfect fit: summary may be unreliable

contributions2 <- clean_data %>%
  group_by(Season, Female, Nest) %>%
  mutate(nMales = n()) %>%
  group_by(Season, Female, Nest, nMales) %>%
  mutate(Rank = rank(desc(propdadcontribute), ties.method = 'first'))

# not separated by number of contributing males
no_nMales <- ggplot(data = contributions2, 
       aes(x = factor(Rank), y = propdadcontribute)) +
  geom_boxplot()

# separated by contributing males
yes_nMales <- ggplot(data = contributions2, 
       aes(x = factor(Rank), y = propdadcontribute,
           fill = factor(nMales))) +
  geom_boxplot()

boxplots <- no_nMales / yes_nMales

boxplots

contributions2 %>%
  group_by(nMales, Rank) %>%
  summarize(avg_contribution = median(propdadcontribute)) %>%
  ggplot(aes(x = Rank, y = avg_contribution, col = factor(nMales))) +
  geom_line()
