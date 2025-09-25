probability_male <- function(clutch_temps, k, pivotal_temp) {
  
  probs_male <- 1 / (1 + exp(-k * (clutch_temps - (pivotal_temp))))
  
  return(probs_male)
  
}