library(scoutR)

scouted_data <- read.csv("shinyapp/data/test_data/tba_data.csv")
tba_data <- event_matches("2025vagle")

sum(!(scouted_data$red_tele_fuel == tba_data$red_teleopFuel))
sum(!(scouted_data$blue_tele_fuel == tba_data$blue_teleopFuel))
sum(!(scouted_data$red_auto_fuel == tba_data$red_autoFuel))
sum(!(scouted_data$blue_auto_fuel == tba_data$blue_autoFuel))

