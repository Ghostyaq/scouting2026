library(ggplot2)
library(dplyr)
library(tidyverse)

endgame_graph <- function(raw, teams) {
    
    number_of_teams <- length(unique(raw$team))
    
    specific_data <- raw |>
        filter(team %in% teams) |>
        mutate(
            endgame_climb = factor(
                endgame_climb, 
                ordered = TRUE, 
                levels = c("F", "No", "L1", "L2", "L3")))|>
        group_by(team, endgame_climb) |>
        summarise(
            number_of_climbs = n()
        )
    
    ggplot(specific_data, 
           aes(fill = endgame_climb, y = number_of_climbs, x = factor(team))) + 
        geom_bar(position = "stack", stat = "identity") +
        labs(title = "Endgame climb",
             x = "Team",
             y = "Number of Climbs") + 
        scale_fill_manual(
            values = c("F" = "#f2b5d4", "No" = "#f7d6e0", "L1" = "#eff7f6", 
                       "L2" = "#b2f7ef", "L3" = "#7bdff2"),
            labels = c("F" = "Fail", "No" = "Didn't attempt", "L1" = "L1", 
                       "L2" = "L2", "L3" = "L3")
        ) +
        theme_bw()
    
}

#raw <- read.csv("shinyapp/data/test_data/data.csv")
#endgame_graph(raw, c(449, 612))


    
    
    
    
    
