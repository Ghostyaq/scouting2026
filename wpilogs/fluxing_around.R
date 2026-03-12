library(ggplot2)
library(tidyverse)

data <- read.csv("wpilogs/robot_data_elim9.csv")
brownouts_only <- data[data$X.SystemStats.BrownedOut == 'true', ]
data$left_shoot_vel <- as.numeric(data$X.Shooter.LeftLeaderVelocityRadPerSec)
data$right_shoot_vel <- as.numeric(data$X.Shooter.RightLeaderVelocityRadPerSec)
var_names <- data.frame(vars = names(data))
shooting <- data[data$left_shoot_vel > 0,] |>
    select(Timestamp, left_shoot_vel, right_shoot_vel) |>
    filter(Timestamp > 190) |>
    filter(left_shoot_vel > 130) |>
    filter(right_shoot_vel > 130) |>
    filter(!is.na(left_shoot_vel))

shooting$left_z_scores = scale(shooting$left_shoot_vel)
shooting$right_z_scores = scale(shooting$right_shoot_vel)

ggplot(shooting, aes(x = Timestamp)) + 
    geom_line(aes(y = left_shoot_vel), color = "#a7000a", size = 0.1) +
    geom_line(aes(y = right_shoot_vel), color = "blue", size = 0.1) + 
    scale_y_continuous() + 
    #scale_y_continuous(breaks = seq(0, 300, by = 20)) +
    theme_bw()

sum(shooting$left_z_scores < -4) # 65 balls shot for < -2, 293 for < -1
sum(shooting$right_z_scores < -4)

# 154 shot during the entirety of qual 70