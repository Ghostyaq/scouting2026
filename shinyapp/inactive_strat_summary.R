#Inactive strategy summary graph

inactive_stategy_summary <- function(raw, selected_teams) {
    
    comments <- raw |>
        group_by(team) |>
        filter(team %in% selected_teams) |>
        mutate(team = as.factor(team)) |>
        summarise(
            a_pass_1 = length(grep("1", inactive_strat)),
            b_theif_2 = length(grep("2", inactive_strat)),
            c_defense_oz_3 = length(grep("3", inactive_strat)),
            d_defense_nz_4 = length(grep("4", inactive_strat)),
            e_intaked_full_5 = length(grep("5", inactive_strat))
        ) |>
        
        pivot_longer(cols = c("a_pass_1",
                              "b_theif_2",
                              "c_defense_oz_3",
                              "d_defense_nz_4",
                              "e_intaked_full_5"),
                     names_to = "comment_type",
                     values_to = "level")
    
    team <- raw |>
        group_by(team)


    ggplot(comments, aes(fill = comment_type, 
                         x = team, 
                         y = level)) +
        geom_bar(position = "stack", stat = "identity") +
        labs(title = "Comments Summary", x = "Teams", y = "# of comments") +
        scale_fill_manual(
            values = c("e_intaked_full_5" = "#f2b5d4", 
                       "d_defense_nz_4" = "#f7d6e0",
                       "c_defense_oz_3" = "#eff7f6", 
                       "b_theif_2" = "#b2f7ef", 
                       "a_pass_1" = "#7bdff2" ),
            labels = c("e_intaked_full_5" = "Intaked full (5)", 
                       "d_defense_nz_4" = "defense nz (4)", 
                       "c_defense_oz_3" = "defense oz (3)", 
                       "b_theif_2" = "theif (2)", 
                       "a_pass_1" = "pass (1)" )) +
        theme_bw()
}

raw <- read.csv("shinyapp/data/test_data/data.csv")
inactive_stategy_summary(raw, c(449, 612, 611, 122))