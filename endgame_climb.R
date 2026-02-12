library(ggplot2)
library(dplyr)
library(tidyverse)

#team <- c(rep("team 1", 4), rep("team 2", 4), rep("team 3", 4), rep("team 4", 4), rep("team 5", 5), rep("team 6", 4) )


endgame_graph <- function(team_num) {
    data <- read.csv("shinyapp/data/test_data/data.csv")
    
    number_of_teams <- length(unique(data$team))
    
    specific_data <- data |>
        select(team, endgame_climb) |>
        filter(team %in% team_num) |>
        mutate(
            endgame_climb = factor(
                endgame_climb, 
                ordered = TRUE, 
                levels = c("F", "No", "L1", "L2", "L3")))|>
        group_by(team, endgame_climb) |>
        summarise(
            number_of_climbs = n()
        )
    
    ggplot(specific_data, aes(fill = endgame_climb, y = number_of_climbs, x = factor(team))) + 
        geom_bar(position="stack", stat="identity") +
        labs(title = "Endgame climb",
             x = "Team",
             y = "Number of Climbs") + 
        scale_fill_manual(
            values = c("F" = "#f2b5d4", "No" = "#f7d6e0", "L1" = "#eff7f6", "L2" = "#b2f7ef", "L3" = "#7bdff2"),
            labels = c("F" = "Fail", "No" = "Didn't attempt", "L1" = "L1", "L2" = "L2", "L3" = "L3")
        ) +
        theme_bw()
    
}


endgame_graph(612)


    
    
    
    
    
