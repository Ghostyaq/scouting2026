library(ggplot2)
library(tidyverse)

cycles_history <- function(raw, team_num){
    past <- raw |>
        group_by(team)|>
        filter(team==team_num)|>
        mutate(
            match = match,
            auto_cycles = auto_cycles,
            tele_cycles = num_cycles + (num_cycles_tenths/10),
            total_cycles = auto_cycles + tele_cycles,
        ) |>
        pivot_longer(
            cols = c("auto_cycles", "tele_cycles", "total_cycles"),
                    names_to = "Type of Cycle", 
                    values_to = "Cycles"
        )
        
    ggplot(past, aes(x= match, y = Cycles, color = `Type of Cycle`)) + 
        geom_line(aes(y=Cycles)) +
        scale_y_continuous() +
        scale_x_continuous(breaks=past$match) + ylab("Cycles")+
        theme_bw()
}
raw <- read.csv("shinyapp/data/test_data/data.csv")
cycles_history(raw,449)