library(tidyverse)
library(ggplot2)


dataframe <- read.csv("shinyapp/data/mdbet/data.csv")
team_id <- c(449, 888, 4821, 116, 1731, 6882)

    colors <- c("blue", "red")
    selected_team <- dataframe |>
        filter(team %in% c(team_id)) |>
        mutate(team = factor(team, levels = team_id),
               alliance_color = ifelse(team %in% team_id[1:3], "red", "blue"))
    
    ggplot(
        selected_team, 
        aes(x = `match`, y = driver_rating, color = alliance_color, group = alliance_color)
    ) + 
        scale_color_manual(values = colors) +
        geom_line() + 
        geom_point() +
        theme(strip.text.x = element_blank()) +
        ylim(0, 5) +
        labs(
            x = "Match",
            y = "Driver Rating",
            color = "Alliance color",
            title = "Driver Rating") + 
        theme_bw() +
        facet_wrap(vars(team)) +
        theme(legend.position = "none")