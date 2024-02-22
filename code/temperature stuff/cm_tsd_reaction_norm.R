##################################################
####### Extract green turtle sex ratio data ######
### and fit TSD reaction norms for general use ###
##################################################

library(embryogrowth)
library(dplyr)

## Pull the green turtle data from the TSD database:
CM_all <- subset(DatabaseTSD, Species=="Chelonia mydas" & (!is.na(Sexed) & Sexed!=0) &
                   !is.na(Correction.factor))

##### incorporate Patricio et al. data #########################################

# adjust correction factor?
# CM_all$Correction.factor = 0.50

# load in Patricio data
patricio <- read.csv("C://Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/Dr Ana Caldas Patricio/logit_points.csv")
P_DF <- patricio %>%
  filter(!is.na(males)) %>%
  mutate(Species = "Chelonia mydas", 
         Country = "Guinea-Bissau", 
         Area = "Poilao Island", 
         Longitude = 10.86668, 
         Latitude = -15.71664, 
         Subspecies = NA, 
         RMU = "Atlantic, South Central & Atlantic, East", 
         Incubation.temperature = Temp, 
         Incubation.temperature.Constant = FALSE, 
         Incubation.temperature.Accuracy = NA, 
         Incubation.temperature.SD = NA, 
         Incubation.temperature.Amplitude = NA, 
         SecondThird.Incubation.temperature.Amplitude = NA, 
         Correction.factor = 0, 
         Egg.mass.mean = NA, 
         Egg.mass.sd = NA, 
         IP.min = NA, 
         IP.max = NA,
         IP.mean = NA,
         IP.SD = NA, 
         IP.SE = NA, 
         SCL.hatchlings.mean = NA,
         SCL.hatchlings.sd = NA,
         Mass.hatchlings.mean = NA, 
         Mass.hatchlings.sd = NA, 
         Total = NA, 
         Hatched = NA, 
         NotHatched = NA, 
         Undeveloped = NA, 
         Unidentified = NA, 
         Intersexes = NA, 
         Males = males, 
         Females = females, 
         Sexed = males + females, 
         Clutch = NA, 
         Reference = "Patricio et al. 2017", 
         Note = NA, 
         Version = NA) 

P_DF2 <- P_DF[, 5:42]

all_data <- rbind(rename(CM_all, 
                         "SecondThird.Incubation.temperature.Amplitude" = 
                         "2ndThird.Incubation.temperature.Amplitude"), P_DF2)

## Fit both datasets with the logistic model
tsdL2 <- with (all_data, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature-Correction.factor, 
                          equation="logistic", replicate.CI=NULL))

# [1] "The pivotal temperature is 29.388 CI95% 29.388;29.388"
# [1] "The transitional range of temperatures is 3.622 CI95% 3.622;3.622"
# [1] "The lower limit of transitional range of temperatures is 27.577 CI95% 27.577;27.577"
# [1] "The upper limit of transitional range of temperatures is 31.199 CI95% 31.199;31.199"
# [1] "The S parameter value is -0.615"


# load libraries
library(ggplot2)

# temperatures
x <- seq(from = 20, to = 40, length = 1000)

# patricio et al.
t_piv1 <- 29.2
k1 <- -1.4
y1 <- 1/(1 + exp(-k1*(x - t_piv1)))

# embryogrowth (all greens)
t_piv2 <- 29.4
k2 <- -0.56
y2 <- 1/(1 + exp(-k2*(x - t_piv2)))

# both together
t_piv3 <- 29.388
# t_piv3 <- 29.290
k3 <- -0.615
# k3 <- -0.652
y3 <- 1/(1 + exp(-k3*(x - t_piv3)))

# make dataframe
TRN <- data.frame(Temperature = x,
                  Model = rep(c('Patricio', 'embryogrowth', 'Combined'), each = 1000),
                  Proportion_Male = c(y1, y2, y3))

# plot
ggplot(data = TRN, aes(x = Temperature, y = Proportion_Male, col = Model)) +
  geom_line(lwd = 2) +
  geom_hline(yintercept = c(0.05, 0.50, 0.95), lty = 2) +
  geom_point(data = P_DF2, aes(x = Incubation.temperature, y = Males/Sexed), 
             col = 'cornflowerblue') +
  geom_point(data = CM_all, aes(x = Incubation.temperature, y = Males/Sexed), 
             col = 'green3')



################################################################################

## Fit the logistic model
tsdL <- with (CM_all, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature-Correction.factor, 
                          equation="logistic", replicate.CI=NULL))

# [1] "The pivotal temperature is 29.368 CI95% 29.368;29.368"
# [1] "The transitional range of temperatures is 3.304 CI95% 3.304;3.304"
# [1] "The lower limit of transitional range of temperatures is 27.716 CI95% 27.716;27.716"
# [1] "The upper limit of transitional range of temperatures is 31.020 CI95% 31.020;31.020"
# [1] "The S parameter value is -0.561"

## Fit the Hill model
tsdH <- with (CM_all, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature-Correction.factor, 
                          equation="Hill", replicate.CI=NULL))

## Fit the Richards model
tsdR <- with (CM_all, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature-Correction.factor, 
                          equation="A-logistic", replicate.CI=NULL))

## Fit the Flexit model
tsdF <- with (CM_all, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature-Correction.factor, 
                          equation="Flexit", replicate.CI=NULL))

## Fit the Double-Richards model (this model is super buggy)
tsdDR <- with (CM_all, tsd(males=Males, females=Females, 
                           temperatures=Incubation.temperature-Correction.factor, 
                           equation="Double-A-logistic", replicate.CI=NULL))

## Assume that sex is determined genetically (i.e. fit a GSD model)
gsd <- with (CM_all, tsd(males=Males, females=Females, 
                         temperatures=Incubation.temperature-Correction.factor, 
                         equation="GSD", replicate.CI=NULL))

## Compare the models using AIC, AIC for small sample sizes (AICc - recommeneded here), and using BIC
compare_AIC(Logistic_Model=tsdL, Hill_model=tsdH, Alogistic_model=tsdR, 
            flexit=tsdF, DoubleAlogistic_model=tsdDR, GSD_model=gsd)
compare_AICc(Logistic_Model=tsdL, Hill_model=tsdH, Alogistic_model=tsdR, 
             DoubleAlogistic_model=tsdDR, GSD_model=gsd, factor.value = -1)
compare_BIC(Logistic_Model=tsdL, Hill_model=tsdH, Alogistic_model=tsdR, 
            DoubleAlogistic_model=tsdDR, GSD_model=gsd, factor.value = -1)
