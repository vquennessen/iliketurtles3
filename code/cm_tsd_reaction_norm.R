##################################################
####### Extract green turtle sex ratio data ######
### and fit TSD reaction norms for general use ###
##################################################

library(embryogrowth)

## Pull the green turtle data from the TSD database:
CM_all <- subset(DatabaseTSD, Species=="Chelonia mydas" & (!is.na(Sexed) & Sexed!=0) &
                   !is.na(Correction.factor))

## Fit the logistic model
tsdL <- with (CM_all, tsd(males=Males, females=Females, 
                          temperatures=Incubation.temperature-Correction.factor, 
                          equation="logistic", replicate.CI=NULL))

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
