library(tidyverse)
library(ggbeeswarm)

raw <- read.csv("shinyapp/data/vaale/data.csv")
team_list <- c("4472", "449")

bump_trench_boxplot <- function(raw, team_list){
    filtered_df <- raw |> filter(team %in% team_list)
    df_bump <- filtered_df |>
        select(team, count = teleop_bump) |> 
        mutate(obstacle = "Bump")
    
    df_trench <- filtered_df |> 
        select(team, count = teleop_trench) |> 
        mutate(obstacle = "Trench")
    
    combined_df <- rbind(df_bump, df_trench)
    combined_df$team <- 
        factor(combined_df$team, levels = team_list, ordered = TRUE)
    
    ggplot(combined_df, aes(x = team, y = count, fill = obstacle)) + 
        geom_boxplot(position = position_dodge(width = .75)) +
        ggbeeswarm::geom_quasirandom(
            shape = 21, color = "black", 
            alpha = 0.8, size = 3,
            aes(fill = obstacle),
            dodge.width = 0.8
        ) +
        labs(title = "Mean Crossing Comparison",
             x = "Team Number",
             y = "Average Times Crossed",
             fill = "Obstacle Type") + 
        theme_bw() + 
        {if (length(team_list) == 6)
            theme(
                axis.text.x = element_text(
                    color = ifelse(
                        levels(combined_df$team) %in% team_list[1:3],
                        "red", 
                        "blue"), size = 15)
            )
            else NULL
        }
}