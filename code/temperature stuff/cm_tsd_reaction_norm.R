##################################################
####### Extract green turtle sex ratio data ######
### and fit TSD reaction norms for general use ###
##################################################

library(embryogrowth)
library(dplyr)

## Pull the green turtle data from the TSD database:
CM_all <- DatabaseTSD %>%
  filter(Species == "Chelonia mydas") %>%
  filter(!is.na(Sexed)) %>%
  filter(Sexed != 0) %>%
  filter(!is.na(Incubation.temperature.corrected))

##### incorporate Patricio et al. data #########################################

# adjust correction factor?
# CM_all$Correction.factor = 0.50

# load in Patricio data
patricio <- read.csv("C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/Dr Ana Caldas Patricio/logit_points.csv")
P_DF <- patricio %>%
  filter(!is.na(males)) %>%
  mutate(Species = "Chelonia mydas", 
         Country = "Guinea-Bissau", 
         Area = "Poilao Island", 
         Longitude = 10.86668, 
         Latitude = -15.71664, 
         Subspecies = NA, 
         RMU.2010 = "Atlantic, South Central & Atlantic, East",
         RMU.2023 = 'North Atlantic & South Atlantic',
         Incubation.temperature.set = NA,              
         Incubation.temperature.recorded = NA,
         Incubation.temperature.corrected = Temp, 
         Duplicated.data = NA,                       
         Duplicate = NA,
         Incubation.temperature.Constant = FALSE, 
         Incubation.temperature.Accuracy = NA, 
         Incubation.temperature.SD = NA, 
         Incubation.temperature.Amplitude = NA, 
         `2ndThird.Incubation.temperature.Amplitude` = NA, 
         Correction.factor = 0, 
         Egg.mass.mean = NA, 
         Egg.mass.sd = NA, 
         IP.min = NA, 
         IP.max = NA,
         IP.mean = NA,
         IP.SD = NA, 
         IP.SE = NA, 
         Length.hatchlings.mean = NA,
         Length.hatchlings.sd = NA,
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
         Number.clutch = NA,
         Clutch = NA, 
         Box = NA,
         Reference = "Patricio et al. 2017", 
         Note = NA, 
         Digital_Identifier = '10.3354/meps12242',
         Version = NA) %>%
  select(Species:Version)


all_data <- rbind(P_DF, CM_all)

## Fit both datasets with the logistic model
tsdL2 <- with (all_data, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature.corrected, 
                          equation="logistic", replicate.CI=NULL))

### new (10/6/2025) (with temperatures = Incubation.temperature.corrected)
# [1] "The pivotal temperature is 29.227"
# [1] "The transitional range of temperatures is 4.838"
# [1] "The lower limit of transitional range of temperatures is 26.808"
# [1] "The upper limit of transitional range of temperatures is 31.646"
# [1] "The S parameter value is -0.822" --> k = -1.217

### old (with temperatures = Incubation.temperature - Correction.factor)
# [1] "The pivotal temperature is 29.388 CI95% 29.388;29.388"
# [1] "The transitional range of temperatures is 3.622 CI95% 3.622;3.622"
# [1] "The lower limit of transitional range of temperatures is 27.577 CI95% 27.577;27.577"
# [1] "The upper limit of transitional range of temperatures is 31.199 CI95% 31.199;31.199"
# [1] "The S parameter value is -0.615"

## Fit just Patricio dataset with the logistic model
tsdL2 <- with (P_DF, tsd(males=Males, females=Females, 
                             temperatures=Incubation.temperature.corrected, 
                             equation="logistic", replicate.CI=NULL))

### new (10/6/2025) (with temperatures = Incubation.temperature.corrected)
# [1] "The pivotal temperature is 29.360"
# [1] "The transitional range of temperatures is 3.816"
# [1] "The lower limit of transitional range of temperatures is 27.452"
# [1] "The upper limit of transitional range of temperatures is 31.268"
# [1] "The S parameter value is -0.648" --> k = -1.543

## Fit just GM dataset with the logistic model
tsdL2 <- with (CM_all, tsd(males=Males, females=Females, 
                         temperatures=Incubation.temperature.corrected, 
                         equation="logistic", replicate.CI=NULL))

### new (10/6/2025) (with temperatures = Incubation.temperature.corrected)
# [1] "The pivotal temperature is 29.274"
# [1] "The transitional range of temperatures is 5.860"
# [1] "The lower limit of transitional range of temperatures is 26.344"
# [1] "The upper limit of transitional range of temperatures is 32.205"
# [1] "The S parameter value is -0.995" --> k = -1.005

### old (with temperatures = Incubation.temperature - Correction.factor)
# [1] "The pivotal temperature is 29.368 CI95% 29.368;29.368"
# [1] "The transitional range of temperatures is 3.304 CI95% 3.304;3.304"
# [1] "The lower limit of transitional range of temperatures is 27.716 CI95% 27.716;27.716"
# [1] "The upper limit of transitional range of temperatures is 31.020 CI95% 31.020;31.020"
# [1] "The S parameter value is -0.561"



################################################################################

## Fit the logistic model
tsdL <- with (CM_all, tsd(males = Males, 
                          females = Females, 
                          temperatures = Incubation.temperature.corrected, 
                          equation = "logistic", 
                          replicate.CI = NULL))

### with temperatures = Incubation.temperature - Correction.factor
# [1] "The pivotal temperature is 29.368 CI95% 29.368;29.368"
# [1] "The transitional range of temperatures is 3.304 CI95% 3.304;3.304"
# [1] "The lower limit of transitional range of temperatures is 27.716 CI95% 27.716;27.716"
# [1] "The upper limit of transitional range of temperatures is 31.020 CI95% 31.020;31.020"
# [1] "The S parameter value is -0.561"

#### with temperatures = Incubation.temperature.corrected
# [1] "The pivotal temperature is 29.274"
# [1] "The transitional range of temperatures is 5.860"
# [1] "The lower limit of transitional range of temperatures is 26.344"
# [1] "The upper limit of transitional range of temperatures is 32.205"
# [1] "The S parameter value is -0.995"

## Fit the Hill model
tsdH <- with (CM_all, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature.corrected, 
                          equation="Hill", replicate.CI=NULL))

## Fit the Richards model
tsdR <- with (CM_all, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature.corrected, 
                          equation="A-logistic", replicate.CI=NULL))

## Fit the Flexit model
tsdF <- with (CM_all, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature.corrected, 
                          equation="Flexit", replicate.CI=NULL))

## Fit the Double-Richards model (this model is super buggy)
tsdDR <- with (CM_all, tsd(males=Males, females=Females, 
                           temperatures=Incubation.temperature.corrected, 
                           equation="Double-A-logistic", replicate.CI=NULL))

## Assume that sex is determined genetically (i.e. fit a GSD model)
gsd <- with (CM_all, tsd(males=Males, females=Females, 
                         temperatures=Incubation.temperature.corrected, 
                         equation="GSD", replicate.CI=NULL))

## Compare the models using AIC, AIC for small sample sizes (AICc - recommeneded here), and using BIC
compare_AIC(Logistic_Model=tsdL, Hill_model=tsdH, Alogistic_model=tsdR, 
            flexit=tsdF, DoubleAlogistic_model=tsdDR, GSD_model=gsd)
compare_AICc(Logistic_Model=tsdL, Hill_model=tsdH, Alogistic_model=tsdR, 
             DoubleAlogistic_model=tsdDR, GSD_model=gsd, factor.value = -1)
compare_BIC(Logistic_Model=tsdL, Hill_model=tsdH, Alogistic_model=tsdR, 
            DoubleAlogistic_model=tsdDR, GSD_model=gsd, factor.value = -1)
