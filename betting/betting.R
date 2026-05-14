bets <- read_csv("betting/bets.csv")

event_key <- "2026mdbet"

starting_balance = 1000

odds <- data.frame(match_number = 1:max(bets$match_number), 
                   chance_red = 0, chance_blue = 0, red_mult = 0, blue_mult = 0)

winner_is_red <- FALSE

for(i in 1:max(bets$match_number)){
    odds$chance_red[i] = match_sb(paste0(event_key, "_qm", as.character(i)))$pred$red_win_prob
    odds$chance_blue[i] = 1 - odds$chance_red[i]
    odds$red_mult[i] = 1/odds$chance_red[i]
    odds$blue_mult[i] = 1/odds$chance_blue[i]
    winner_is_red[i] <- match_sb(paste0(event_key, "_qm", as.character(i)))$result$winner == "red"
}

bets <- mutate(bets, prediction_is_red = prediction == "Red", 
               winner_is_red = winner_is_red[match_number]) |> 
    select(scout_initials, match_number, 
           prediction_is_red, winner_is_red, point_bet)

bets <- bets |> mutate(delta = ifelse(xor(winner_is_red, prediction_is_red),
                              point_bet * -1,
                            ifelse(winner_is_red,
                                   point_bet * (red_mult -1),
                                   point_bet * (blue_mult-1)
                                            )))

balances <- bets |> group_by(scout_initials) |> summarize(balance = starting_balance + sum(delta))
