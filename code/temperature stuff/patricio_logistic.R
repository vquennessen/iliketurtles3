# patricio logistic 

# load libraries
library(dplyr)

# load in raw data
Patricio <- read.csv("C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/Dr Ana Caldas Patricio/raw_data.csv")

# glm
mod1 <- glm(data = Patricio, Male ~ Temp, family = 'binomial')

# summary
summary(mod1)
