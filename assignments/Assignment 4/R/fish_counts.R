## fish counts
## Returns the most common fish, the rarest fish, the total number of fish
## returns plot when requested
library(tidyverse)

fish_counts <- function(fish, plot = FALSE){
  
  # convert input list to factor
  fish <- as.factor(fish)
  
  # summarize fish
  fish_summary <- summary(fish)
  
  # find common fish
  common_fish <- which.max(fish_summary)
  
  # find rare fish
  rare_fish <- which.min(fish_summary)
  
  # find total number of fish
  total_fish <- sum(fish_summary)
  
  # make plot
  fish_plot <-  ggplot(data.frame(fish_summary), 
         aes(fish_summary, fill=fish_summary))+
    geom_histogram(stat="count")+
    labs(title="Total fish caught")
  
  ifelse(plot == TRUE,
         return(common_fish, rare_fish, total_fish, fish_plot),
         return(common_fish, rare_fish, total_fish))
}