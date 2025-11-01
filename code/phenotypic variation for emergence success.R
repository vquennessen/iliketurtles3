# logistic model for temperature-mediated emergence success

# load libraries
library(readr)

# load in data from 
#' Laloe€ J-O, Cozens J, Renom B,  Taxonera A, Hays GC. Climate change and 
#' temperature-linked  hatchling mortality at a globally important sea turtle 
#' nesting  site. Glob Change Biol. 2017;23:4922–4931. 
#' https://doi.org/10.1111/gcb.13765  

laloe_2017_raw_data <- read_csv(
  "~/Projects/iliketurtles3/data/laloe_2017_raw_data.csv")

# clean up the data - use only emergence success, unless there is none, and then 
# use hatching success
clean_data <- laloe_2017_raw_data %>%
  rowwise() %>%
  mutate(success = case_when(is.na(emergence_success) ~ hatching_success, 
                                       TRUE ~ emergence_success))

# logistic model as a function of incubation temperature, an asymptote (Asym), 
# a midpoint (xmid), and 1/slope (scal)

model1 <- nls(success ~ SSlogis(incubation_temp, Asym, xmid, scal), 
              clean_data)

# summary of coefficients, including estimate and standard error
coef(summary(model1))

#        Estimate Std. Error    t value      Pr(>|t|)
# xmid 32.7034315  0.1198597 272.847712 6.049604e-154

# standard error = sqrt(variance / sample size)
# variance = standard_error^2 * sample size

coef_matrix <- summary(model1)$coefficients
xmid_se <- coef_matrix["xmid", "Std. Error"]

xmid_se^2 * nrow(clean_data)
# variance = 1.58