#Inactive strategy summary graph

inactive_stategy_summary <- function(raw, selected_teams) {
    comments <- raw |>
        group_by(team) |>
        filter(team %in% selected_teams) |>
        mutate(team = as.factor(team)) |>
        summarise(
            a_pass_1 = length(grep("1", inactive_strat)),
            b_herd_2 = length(grep("2", inactive_strat)),
            c_theif_3 = length(grep("3", inactive_strat)),
            d_defense_oz_4 = length(grep("4", inactive_strat)),
            e_defense_nz_5 = length(grep("5", inactive_strat)),
            f_intaked_full_6 = length(grep("6", inactive_strat))
        ) |>
        
        pivot_longer(cols = c("a_pass_1",
                              "b_herd_2",
                              "c_theif_3",
                              "d_defense_oz_4",
                              "e_defense_nz_5",
                              "f_intaked_full_6"),
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
            values = c("f_intaked_full_6" = "#f2b5d4", 
                       "e_defense_nz_5" = "#f7d6e0",
                       "d_defense_oz_4" = "#eff7f6", 
                       "c_theif_3" = "#b2f7ef", 
                       "b_herd_2" = "#7bdff2",
                       "a_pass_1" = "#358c8f" ),
            labels = c("f_intaked_full_6" = "Intaked full (6)", 
                       "e_defense_nz_5" = "defense nz (5)", 
                       "d_defense_oz_4" = "defense oz (4)", 
                       "c_theif_3" = "theif (3)",
                       "b_herd_2" = "herd (2)",
                       "a_pass_1" = "pass (1)" )) +
        theme_bw()
}

raw <- read.csv("shinyapp/data/test_data/data.csv")
inactive_stategy_summary(raw, c(449, 612, 611, 122))