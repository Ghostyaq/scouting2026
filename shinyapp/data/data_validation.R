raw <- read.csv('shinyapp/data/vaale/data.csv')
schedule <- read.csv('shinyapp/data/vaale/schedule.csv')

robot_order <- c("R1", "R2", "R3", "B1", "B2", "B3")
raw$robot <- factor(raw$robot, levels = robot_order, ordered = TRUE)
data <- raw |>
    arrange(match, robot) |>
    rowwise() |>
    mutate(
        scout_key = paste(match, robot, team)
    )

long_schedule <- schedule |>
    pivot_longer(
        cols = c(R1, R2, R3, B1, B2, B3),
        names_to = "robot",
        values_to = "team"
    ) |>
    rowwise() |>
    mutate(
        scout_key = paste(match, robot, team)
    )

missed_matches <- anti_join(long_schedule, data, by = "scout_key")
non_existent_matches <- anti_join(data, long_schedule, by = "scout_key")
double_scouted <- data[(
    duplicated(data[, "scout_key"]) | 
        duplicated(data[, "scout_key"], 
                   fromLast = TRUE)), ]
