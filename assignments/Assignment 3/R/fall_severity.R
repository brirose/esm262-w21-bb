## case when assignment 3
## determines fall factor severity for a climb based on fall factor
## takes inputs to calculate fall factor
## lengths can be any unit, so long as same

library(tidyverse)


fall_severity <- function (fall_length, rope_length){

  #  Error checking to ensure variables greater than 0
  
  fall_length = ifelse(
    fall_length <= 0,
    return("Fall length must be greater than zero"),
    fall_length
    )
    
  
  rope_length=ifelse(
    (rope_length <= 0), 
    return("Rope length must be greater than zero"), 
    rope_length
  )

  # calculate fall factor based on ratio of fall length to rope length
  fall_factor = fall_length/rope_length
  
  # classify severity of fall
  fall_severity = case_when(
    fall_factor <= 0.5 ~ "minor",
    fall_factor <= 1 ~ "moderate",
    fall_factor <= 1.5 ~ "severe",
    fall_factor <= 2 ~ "very severe",
    TRUE ~ "oops something went wrong"
  )
  
  
  # return fall severity
  return(fall_severity)
  
}