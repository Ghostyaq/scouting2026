install.packages("beeswarm")
library(beeswarm)

beeswarm_plot(raw, selected_teams) {
    
    df <- raw |>
        group_by(team) |>
        filter(team %in% selected_teams)
    beeswarm(
        teleop_trench
        teleop_bump
    )
}

raw <- read.csv("shinyapp/data/week0/data.csv")
beeswarm_plot(raw, 449)