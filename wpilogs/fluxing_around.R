library(ggplot2)
library(tidyverse)

data <- read.csv("wpilogs/robot_data_qual70.csv")
brownouts_only <- data[data$X.SystemStats.BrownedOut == 'true', ]
data$left_shoot_vel <- as.numeric(data$X.Shooter.LeftLeaderVelocityRadPerSec)
var_names <- data.frame(vars = names(data))
shooting <- data[data$left_shoot_vel > 0,] |>
    select(Timestamp, left_shoot_vel) |>
    #filter(Timestamp > 170) |>
    filter(left_shoot_vel > 150) |>
    filter(!is.na(left_shoot_vel))

shooting$z_scores <- scale(shooting$left_shoot_vel)

ggplot(shooting, aes(x = Timestamp, y = left_shoot_vel)) + 
    geom_line() + 
    scale_y_continuous(breaks = seq(0, 300, by = 20)) +
    theme_bw()

sum(shooting$z_scores < -1.35) # 65 balls shot for < -2, 293 for < -1

# 154 shot during the entirety of qual 70

ggplot(data, aes(x = Timestamp, y = left_shoot_vel)) +
    geom_line() + 
    theme_bw()
