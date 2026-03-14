library(ggplot2)
library(tidyverse)

get_mode <- function(x) {
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
}


data <- read.csv("wpilogs/robot_data_elim12.csv")
brownouts_only <- data[data$X.SystemStats.BrownedOut == 'true', ]
data$left_shoot_vel <- as.numeric(data$X.Shooter.LeftLeaderVelocityRadPerSec)
data$right_shoot_vel <- as.numeric(data$X.Shooter.RightLeaderVelocityRadPerSec)
data$left_shoot_stat_curr <- as.numeric(data$X.Shooter.LeftLeaderStatorCurrentAmps)
data$right_shoot_stat_curr <- as.numeric(data$X.Shooter.RightLeaderStatorCurrentAmps)
data$is_autonomous <- as.logical(data$X.SystemStats.SystemActive) & 
    as.logical(data$X.DriverStation.Autonomous)
var_names <- data.frame(vars = names(data))
shooting <- data[data$left_shoot_vel > 0,] |>
    select(Timestamp, left_shoot_vel, right_shoot_vel, left_shoot_stat_curr, 
           right_shoot_stat_curr, is_autonomous) |> 
    filter(Timestamp > 190) |>
    filter(left_shoot_vel > 130) |>
    filter(right_shoot_vel > 130) |>
    filter(!is.na(left_shoot_vel)) |>
    filter(is_autonomous)

left_df <- shooting |>
    select(left_shoot_vel, left_shoot_stat_curr, Timestamp)
    
left_df <- left_df |>
    mutate(group = kmeans(select(left_df, left_shoot_vel, left_shoot_stat_curr), 2)$cluster)

left_df <- left_df |>
    mutate(true_false = left_df$group != get_mode(left_df$group))

groups_left_df <- left_df |>
    summarize(group = left_df$group,
              true_false = left_df$true_false,
              shift_true_false = c(FALSE, left_df$true_false[-nrow(left_df)]))

groups_left_dif <- groups_left_df |>
    filter(true_false != shift_true_false)

shooting$left_z_scores = scale(shooting$left_shoot_vel)
shooting$right_z_scores = scale(shooting$right_shoot_vel)

ggplot(shooting, aes(x = Timestamp)) + 
    geom_line(aes(y = left_shoot_vel), color = "#a7000a", size = 0.1) +
    geom_line(aes(y = right_shoot_vel), color = "blue", size = 0.1) + 
    scale_y_continuous() + 
    #scale_y_continuous(breaks = seq(0, 300, by = 20)) +
    theme_bw()

ggplot(left_df, aes(x = left_shoot_vel, y = left_shoot_stat_curr, color = group)) +
    geom_point()

ggplot(left_df, aes(x = Timestamp, y = left_shoot_vel, color = group)) +
    geom_point()

sum(shooting$left_z_scores < -4) # 65 balls shot for < -2, 293 for < -1
sum(shooting$right_z_scores < -4)

# 154 shot during the entirety of qual 70