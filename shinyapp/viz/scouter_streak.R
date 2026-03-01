
high_streak <- function(raw){
    current_match = max(raw$match)
    all_matches <- 1:current_match
    streak_df <- raw |>
        mutate(
            scout = toupper(scout),
            scout = trimws(scout),
            scout = gsub("[^[:alpha:]]", "", scout)
        ) |>
        group_by(scout) |>
        summarise(
            scouted_matches = list(unique(match))
        ) |>
        rowwise() |>
        mutate(
            missed_matches = list(setdiff(all_matches, scouted_matches)),
            streak = current_match - max(missed_matches)
        ) |>
        filter(streak>0)
    
    ggplot(streak_df, aes(x = `scout`, streak)) + 
        geom_bar(position = "stack", stat = "identity", fill = "chartreuse2") + 
        labs(title = "Current Streak", 
             x = "Scouts", y = "Matches") +
        theme_bw()
}
