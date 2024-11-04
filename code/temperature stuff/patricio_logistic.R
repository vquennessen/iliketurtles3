# patricio logistic 

# load libraries
library(dplyr)

# load in raw data
Patricio <- read.csv("C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/Dr Ana Caldas Patricio/raw_data.csv")

# glm
mod1 <- glm(data = Patricio, Male ~ Temp, family = 'binomial')

# summary
summary(mod1)

# Call:
#   glm(formula = Male ~ Temp, family = "binomial", data = Patricio)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.3334  -0.5675  -0.3473   0.3743   2.3704  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  45.3120     7.7553   5.843 5.14e-09 ***
#   Temp         -1.5433     0.2615  -5.903 3.58e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 173.21  on 134  degrees of freedom
# Residual deviance: 106.53  on 133  degrees of freedom
# AIC: 110.53
# 
# Number of Fisher Scoring iterations: 5