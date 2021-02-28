## while loop A3
## lengths in feet
## rope elongation a proportion between 0 and 0.4
## ignores many important parameters, not to be used

gear_placement <- function(climb_height, fall_distance){
  
  climb_height = ifelse(
    climb_height <= 0,
    return("Climb height must be greater than zero"),
    climb_height
  )
  
  fall_distance = ifelse(
    fall_distance > 0 & fall_distance < climb_height,
    fall_distance,
    return("fall distance must be greater than zero and less than climb height")
  )
  
  clip_count <- 1
  fall <- climb_height
  
  while(fall > fall_distance && clip_count <= 15){
    
    fall = climb_height/clip_count
    
    if(fall > fall_distance){
      clip_count = clip_count + 1
    }else return(clip_count)
  }
}


