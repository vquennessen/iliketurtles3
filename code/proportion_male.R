proportion_male <- function(clutch_temp, k, pivotal_temp) {
  
  props_male <- 1 / (1 + exp(-k * (clutch_temp - (pivotal_temp))))
  
  return(props_male)
  
}