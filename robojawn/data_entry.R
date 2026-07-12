# Robojawn data manip
rm(list = ls())
load("robojawn/robojawn_prescout.rda")
load("robojawn/scores.rda")

library(tidyverse)
library(scoutR)

add_score <- function(blue1, blue2, blue3, red1, red2, red3, 
                      blue_auto = NA, red_auto = NA,  
                      blue_total = NA, red_total = NA){
    load("robojawn/scores.rda")
    scores <- rbind(
        scores, 
        data.frame(red1 = red1, red2 = red2, red3 = red3, 
                   blue1 = blue1, blue2 = blue2, blue3 = blue3, 
                   red_auto = red_auto, blue_auto = blue_auto, 
                   red_total = red_total, blue_total = blue_total)
    )
    save(scores, file = "robojawn/scores.rda")
}

# Saving these as an example of how I entered data
# # Q26
# add_score(5181, 122, 157, 321, 9994, 484, 
#           blue_auto = 38, red_auto = 12, blue_total = 154, red_total = 64)
# 
# # Q27
# add_score(5000, 316, 2539, 190, 427, 6328, 
#           blue_auto = 120, red_auto = 119, blue_total = 316, red_total = 437)

load("robojawn/scores.rda")
