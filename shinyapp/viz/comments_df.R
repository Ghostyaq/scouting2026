raw <- read.csv("shinyapp/data_files/all_data/data.csv")

comments_df <- data.frame(
    team = raw$team,
    comments = raw$commentOpen,
    match = raw$match
) |>
    arrange(team, desc(match)) |>
    filter(comments > 0)