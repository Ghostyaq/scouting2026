library(tidyverse)
library(scoutR)

get_schedule <- function(event_key){
    tba <- event_matches(event_key, match_type = c("quals"))
    schedule <- data.frame(
        match = tba$match_number,
        R1 = as.integer(gsub("frc", "", tba$red1)),
        R2 = as.integer(gsub("frc", "", tba$red2)),
        R3 = as.integer(gsub("frc", "", tba$red3)),
        B1 = as.integer(gsub("frc", "", tba$blue1)),
        B2 = as.integer(gsub("frc", "", tba$blue2)),
        B3 = as.integer(gsub("frc", "", tba$blue3))
    )
    
    return(schedule)
}

get_QRScout_teams <- function(event_key){
    tba <- event_teams(event_key)
    teams <- sort(unique(as.integer(tba$team_number)))
    
    format <- data.frame(team = teams) |>
        mutate(
            team = as.character(team),
            string = paste0('\"', team, '\": "', team, '\",')
        )
    
    remove_comma <- format[length(teams), ]$string
    remove_comma <- substr(remove_comma, 1, nchar(remove_comma) - 1)
    format[length(teams), ]$string <- remove_comma
    
    write.csv(format$string, "QRScout/teams.csv", 
              row.names = FALSE, quote = FALSE)
}
