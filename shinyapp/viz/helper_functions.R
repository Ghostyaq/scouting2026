library(tidyverse)
library(ggplot2)
library(plotly)
library(scoutR)

bump_trench_boxplot <- function(raw, team_list){
    filtered_df <- raw |> filter(team %in% team_list)
    df_bump <- filtered_df |>
        select(team, count = teleop_bump) |> 
        mutate(obstacle = "Bump")
    
    df_trench <- filtered_df |> 
        select(team, count = teleop_trench) |> 
        mutate(obstacle = "Trench")
    
    combined_df <- rbind(df_bump, df_trench)
    combined_df$team <- 
        factor(combined_df$team, levels = team_list, ordered = TRUE)
    
    ggplot(combined_df, aes(x = team, y = count, fill = obstacle)) + 
        geom_boxplot(position = position_dodge(width = .75)) +
        ggbeeswarm::geom_quasirandom(
            shape = 21, color = "black", 
            alpha = 0.8, size = 3,
            aes(fill = obstacle),
            dodge.width = 0.8
        ) +
        labs(title = "Mean Crossing Comparison",
             x = "Team Number",
             y = "Average Times Crossed",
             fill = "Obstacle Type") + 
        theme_bw() + 
        {if (length(team_list) == 6)
            theme(
                axis.text.x = element_text(
                    color = ifelse(
                        levels(combined_df$team) %in% team_list[1:3],
                        "red", 
                        "blue"), size = 15)
            )
            else NULL
        }
}

plot_driver_rating_graph <- function(dataframe, team_id) {
    selected_team <- dataframe |>
        filter(team %in% c(team_id)) |>
        mutate(team = factor(team))
    ggplot(selected_team, aes(x = match, y = driver_rating, 
                              color = team, group = team)) + 
        geom_line() + 
        geom_point() +
        theme_bw() +
        scale_x_continuous(breaks = c(selected_team$match)) +
        ylim(0, 5) +
        labs(
            x = "Match",
            y = "Driver Rating",
            color = "Teams",
            title = "Driver Rating") + 
        theme_bw()
}

endgame_graph <- function(raw, teams) {
    number_of_teams <- length(unique(raw$team))
    data <- raw |>
        filter(team %in% teams) |>
        mutate(
            endgame_climb = factor(
                endgame_climb, 
                ordered = TRUE, 
                levels = c("F", "No", "L1", "L2", "L3")))|>
        group_by(team, endgame_climb) |>
        summarise(
            number_of_climbs = n()
        )
    
    data$team = factor(data$team, levels = teams, ordered = TRUE)
    
    ggplot(data, aes(fill = endgame_climb, y = number_of_climbs, x = team)) + 
        geom_bar(position = "stack", stat = "identity") +
        labs(title = "Endgame climb",
             x = "Team",
             y = "Number of Climbs") + 
        scale_fill_manual(
            values = c("F" ="#E6CCB2", "No" = "#DDB892", "L1" = "#B08968", 
                       "L2" = "#9C6644", "L3" = "#7F5539"),
            labels = c("F" = "Fail", "No" = "Didn't attempt", "L1" = "L1", 
                       "L2" = "L2", "L3" = "L3")
        ) +
        theme_bw() + 
        {if (length(teams) == 6)
            theme(
                axis.text.x = element_text(
                    color = ifelse(
                        levels(data$team) %in% teams[1:3],
                        "red", 
                        "blue"), size = 15)
            )
            else NULL
        }
}

# event_key needed to write pridge.csv to the right folder (switch to a .R?)
pridge_calculation_offline <- function(event_key) {
    data_dir_path <- paste0("shinyapp/data/", event_key)
    schedule <- read.csv(paste0(data_dir_path, "/schedule.csv"))
    tba_data <- read.csv(paste0(data_dir_path, "/tba_data.csv"))
    statbotics_data <- read.csv(paste0(data_dir_path, "/statbotics_data.csv"))
    
    unique_teams <- sort(unique(unlist(schedule[,2:7])))
    design <- matrix(0, 
                     nrow = length(unique(tba_data$match)) * 2, 
                     ncol = length(unique_teams))
    colnames(design) <- unique_teams
    matches <- unique(tba_data$match)
    
    long_schedule <- schedule |>
        pivot_longer(
            cols = c("R1", "R2", "R3", "B1", "B2", "B3"),
            names_to = "robot",
            values_to = "team"
        )
    
    for (i in 1:nrow(design)) {
        chipotle <- filter(
            long_schedule,
            match == matches[ceiling(i/2)], 
            substring(robot, 1, 1) == ifelse(i %% 2, "B", "R"))
        design[i, as.character(chipotle$team)] = 1
    }
    
    response <- tba_data |>
        pivot_longer(
            cols = names(tba_data)[2:5],
            names_to = "alliance",
            values_to = "score"
        )
    
    auto_priors <- statbotics_data$auto_fuel_epa
    tele_priors <- statbotics_data$tele_fuel_epa
    names(auto_priors) <- names(tele_priors) <- statbotics_data$team
    grid <- seq(0, 20, length.out = 1000)
    tele_fuel_columns <- c('red_tele_fuel', 'blue_tele_fuel') # 80 char limit
    
    auto_mses <- scoutR:::pridge_lambda_cv(
        design, 
        response$score[!(response$alliance %in% tele_fuel_columns)], 
        auto_priors, grid, plot_mses = FALSE)
    
    tele_mses <- scoutR:::pridge_lambda_cv(
        design, 
        response$score[response$alliance %in% tele_fuel_columns], 
        tele_priors, grid, plot_mses = FALSE)
    
    auto_lambda_opt <- grid[which.min(auto_mses)]
    tele_lambda_opt <- grid[which.min(tele_mses)]
    
    auto_fuel <- round(scoutR:::prior_ridge(
        design, 
        response$score[
            (response$alliance %in% c('red_auto_fuel', 'blue_auto_fuel'))],  
        auto_lambda_opt, auto_priors), 2)
    tele_fuel <- round(scoutR:::prior_ridge(
        design, 
        response$score[
            response$alliance %in% c('red_tele_fuel', 'blue_tele_fuel')],  
        tele_lambda_opt, tele_priors), 2)
    
    auto_fuel_opr <- round(scoutR:::prior_ridge(
        design, 
        response$score[
            (response$alliance %in% c('red_auto_fuel', 'blue_auto_fuel'))],  
        0, auto_priors), 2)
    tele_fuel_opr <- round(scoutR:::prior_ridge(
        design, 
        response$score[
            response$alliance %in% c('red_tele_fuel', 'blue_tele_fuel')],  
        0, tele_priors), 2)
    
    priors_df <- data.frame(
        team = unique_teams, 
        auto_fuel, tele_fuel, 
        auto_fuel_opr, tele_fuel_opr,
        auto_fuel_epa = statbotics_data$auto_fuel_epa,
        tele_fuel_epa = statbotics_data$tele_fuel_epa)
    write.csv(
        priors_df, 
        paste0("shinyapp/data/", event_key, "/pridge.csv"), row.names = FALSE)
}

recent_team_epas <- function(event_key) {
    sb_data <- team_events_sb(event = paste0(2026, event_key))
    
    teams <- sapply(sb_data, function(tm){tm$team})
    epa_fuel_auto <- sapply(sb_data, function(tm){tm$epa$breakdown$auto_fuel})
    epa_fuel_tele <- sapply(sb_data, function(tm){
        tm$epa$breakdown$teleop_fuel + tm$epa$breakdown$endgame_fuel})

    epas <- data.frame(
        team = teams, 
        auto_fuel_epa = epa_fuel_auto,
        tele_fuel_epa = epa_fuel_tele)
    return(epas)
}

pridge_calculation_online <- function(event_key){
    matches <- event_matches(paste0("2026", event_key), match_type = "quals")
    
    schedule <- data.frame(
        match = matches$match_number,
        R1 = matches$red1,
        R2 = matches$red2,
        R3 = matches$red3,
        B1 = matches$blue1,
        B2 = matches$blue2,
        B3 = matches$blue3
    ) |>
        rowwise() |>
        mutate(
            R1 = as.numeric(gsub("frc", "", R1)),
            R2 = as.numeric(gsub("frc", "", R2)),
            R3 = as.numeric(gsub("frc", "", R3)),
            B1 = as.numeric(gsub("frc", "", B1)),
            B2 = as.numeric(gsub("frc", "", B2)),
            B3 = as.numeric(gsub("frc", "", B3)),
        )
    
    blue_auto_fuel <- sapply(matches[['blue_hubScore']], \(x) x$autoCount)
    blue_tele_fuel <- sapply(matches[['blue_hubScore']], \(x) x$teleopCount)
    
    red_auto_fuel <- sapply(matches[['red_hubScore']], \(x) x$autoCount)
    red_tele_fuel <- sapply(matches[['red_hubScore']], \(x) x$teleopCount)
    
    extracted_data <- data.frame(
        match = matches$match_number, 
        blue_auto_fuel,
        blue_tele_fuel,
        red_auto_fuel,
        red_tele_fuel)
    
    file_path_1 <- paste0("shinyapp/data/", event_key, "/tba_data.csv")
    write.csv(extracted_data, file_path_1, row.names = FALSE)
    
    statbotics_data <- recent_team_epas(event_key)
    file_path_2 <- paste0("shinyapp/data/", event_key, "/statbotics_data.csv")
    write.csv(statbotics_data, file_path_2, row.names = FALSE)
    
    pridge_calculation_offline(event_key)
}

plot_scouting_graph <- function(raw) {
    scout <- raw$scout
    scout_count <- count(raw, scout, sort = TRUE, name = "number_of_times")|>
        mutate(percentile = percent_rank(number_of_times))
    
    still_graph <- ggplot(scout_count, aes(
        text = paste("Scout:", scout, "|| Count:", number_of_times),
        x = reorder(scout, number_of_times, decreasing = TRUE),
        y = number_of_times,
        fill = percentile)) +
        geom_col() +
        theme_bw() +
        theme(legend.position = "none") + 
        scale_fill_gradient2(high = "forestgreen", mid = "grey90", low = "firebrick2", midpoint = 0.5) +
        labs(
            x = "Scout Initials",
            y = "Number of Times Scouted",
            title = "Scout and Their Number of Times Scouted")
    
    ggplotly(still_graph, tooltip = "text")
}

stacked_bar_chart <- function(raw, schedule, pridge, order, teams, flip){
    data <- summary_stats(raw, pridge, teams = NULL) |>
        select(Team, `Auto Fuel`, `Tele Fuel`, `ACP`, Climb, `Total Score`) |>
        rename(`Auto Climb` = ACP) |>
        filter(Team %in% teams)
    
    if (order) {
        team_order <- arrange(data, desc(`Total Score`))$Team
    } else {
        team_order <- teams
    }
    
    data <- pivot_longer(
        data,
        cols = c('Auto Fuel', 'Tele Fuel', 'Auto Climb', 'Climb'),
        names_to = 'type',
        values_to = 'score',
    )
    
    data$Team <- factor(data$Team, levels = team_order, ordered = TRUE)
    data$type <- factor(
        data$type, 
        c("Auto Fuel", "Auto Climb", "Tele Fuel", "Climb"), 
        ordered = TRUE)
    
    ggplot(data, aes(x = Team, y = score, fill = type)) +
        geom_bar(stat = "identity") + 
        labs(
            title = "Stacked Bar Chart", x = "Team", y = "Climb + PRidge Score"
        ) + 
        scale_fill_manual(
            values = c("Auto Fuel" ="#6B705C", 
                       "Auto Climb" = "#A5A58D",
                       "Tele Fuel" = "#B7B7A4",
                       "Climb" = "#DDBEA9"
            ) 
        ) +
        theme_bw() +
        {if (length(teams) == 6)
            theme(
                axis.text.x = element_text(
                    color = ifelse(
                        levels(data$Team) %in% teams[1:3],
                        "red", 
                        "blue"), size = 15)
            )
            else NULL
        } +
        {if (flip) coord_flip() else NULL}
}

summary_stats <- function(raw, pridge, teams = NULL) {
    if (is.null(teams)) teams <- sort(unique(pridge$team))
    result <- raw |>
        filter(team %in% teams) |>
        group_by(team) |>
        summarise(
            `Matches Played` = n(),
            Climb = mean(
                ifelse(endgame_climb == "L1", 10, 
                       ifelse(endgame_climb == "L2", 20, 
                              ifelse(endgame_climb == "L3", 30, 0)))),
            ACP = mean(auto_climb * 15, na.rm = TRUE),
            `Auto Cycles` = mean(auto_cycles / 10, na.rm = TRUE),
            `Tele Cycles` = mean(num_cycles + num_cycles_tenths / 10, na.rm = TRUE),
            `Total Cycles` = `Auto Cycles` + `Tele Cycles`,
            `Auto Bump` = sum(auto_bump, na.rm = TRUE),
            `Tele Trench` = mean(teleop_trench, na.rm = TRUE),
            `Tele Bump` = mean(teleop_bump, na.rm = TRUE),
            `Auto Climb` = sum(auto_climb, na.rm = TRUE),
            Driver = mean(driver_rating, na.rm = TRUE),
            `Quick Climb` = sum(climb_less_than_5, na.rm = TRUE),
            `Solo Shot` = sum(solo_shooting, na.rm = TRUE),
            Died = sum(died, na.rm = TRUE),
            Card = sum(card != 'No Card', na.rm = TRUE)
        ) |>
        left_join(pridge) |>
        mutate(
            `Auto Fuel` = auto_fuel,
            `Tele Fuel` = tele_fuel,
            `Total Fuel` = `Auto Fuel` + `Tele Fuel`,
            `Total Score` = `Auto Fuel` + `Tele Fuel` + ACP + Climb
        ) |>
        select(
            Team = team, `Auto Fuel`, `Tele Fuel`, `Total Fuel`, `Total Score`,
            `Auto Cycles`, `Tele Cycles`, `Total Cycles`, `Auto Bump`,
            `Tele Bump`, `Tele Trench`, `Auto Climb`, Climb, `Quick Climb`, 
            Driver, `Solo Shot`, Died, Card, `Matches Played`, ACP) |>
        modify_if(~is.numeric(.), ~round(., 2))
    
    result <- result[order(match(result$Team, teams)), ]
    return(result)
}

comments_df <- function(raw, team_list = NULL) { 
    data <- raw |>
        select(team, match, comments) |>
        filter(comments > 0) |>
        filter(team %in% team_list) |>
        rowwise() |>
        mutate(
            team = factor(team, levels = team_list, ordered = TRUE),
            match = as.integer(match)
        ) |>
        arrange(team, desc(match))
    
    return(data)
}

yap_graph <- function(raw) {
    spliting <- strsplit(raw$comments, split = " ")
    
    raw$number_of_yaps <- sapply(spliting, length)
    
    scout_comments <- raw |>
        group_by(scout) |>
        summarize(
            mean_yaps = round(mean(number_of_yaps), digits = 2),
            count = n()
        ) |>
        mutate(percentile = percent_rank(mean_yaps))|>
        mutate(
            scout_name = reorder(scout, mean_yaps, decreasing = TRUE)
        )
    
    plot <- ggplot(scout_comments, aes(x = scout_name, y = mean_yaps, fill = percentile)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        labs(title = "Comments Summary: Mean Yappage per Scout", 
             x = "Scouts", y = "Mean yappage") +
        scale_fill_gradient2(high = "forestgreen", mid = "grey90", low = "firebrick2", midpoint = 0.5) +
        theme_bw()
    
    ggplotly(plot)
}

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
        mutate(percentile = percent_rank(streak)) |>
        filter(streak > 0)
    
    ggplot(streak_df, aes(x = `scout`, streak, fill = percentile)) + 
        geom_bar(position = "stack", stat = "identity") + 
        labs(title = "Current Streak", 
             x = "Scouts", y = "Matches") +
        scale_fill_gradient2(high = "firebrick2", mid = "grey90", low = "cornflowerblue", midpoint = 0.5) +
        theme_bw()
}

normalize_column <- function(x) {
    if (sd(x, na.rm = TRUE) == 0) {
        return(rep(0, length(x)))
    }
    
    normalized <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    normalized[is.nan(normalized)] <- 0
    return(normalized)
}

calculate_team_scores <- function(weights, team_data){
    numeric_cols <- names(team_data)
    normalized_data <- team_data
    
    for (col in numeric_cols) {
        normalized_data[[col]] <- normalize_column(team_data[[col]])
    }
    
    team_scores <- team_data[, "Team", drop = FALSE]
    team_scores$`Team Score` <- 0
    
    for (col in numeric_cols) {
        if (col %in% names(weights)) {
            weight_val <- weights[[col]]
            team_scores$`Team Score` <-
                team_scores$`Team Score` + (normalized_data[[col]] * weight_val)
            team_scores$`Team Score` <- round(team_scores$`Team Score`, 2)
        }
    }
    
    team_scores <- merge(team_scores, team_data, by = "Team")
    team_scores <- team_scores[order(-team_scores$`Team Score`), ]
    return(team_scores)
}

weights_modal <- function(weights) {
    modalDialog(
        title = "Adjust Team Weighting Factors",
        size = "l",
        fluidRow(
            column(6,
                   sliderInput(
                       "weight_auto_fuel", "Auto Fuel", min = -20, max = 20, 
                       value = weights$`Auto.Fuel`, step = 1),
                   sliderInput(
                       "weight_tele_fuel", "Tele Fuel", min = -20, max = 20, 
                       value = weights$`Tele.Fuel`, step = 1),
                   sliderInput(
                       "weight_total_fuel", "Total Fuel", min = -20, max = 20, 
                       value = weights$`Total.Fuel`, step = 1),
                   sliderInput(
                       "weight_total_score", "Total Score", min = -20, max = 20, 
                       value = weights$`Total.Score`, step = 1),
                   sliderInput(
                       "weight_auto_cycle", "Auto Cycles", min = -20, max = 20, 
                       value = weights$`Auto.Cycles`, step = 1),
                   sliderInput(
                       "weight_tele_cycle", "Tele Cycles", min = -20, max = 20, 
                       value = weights$`Tele.Cycles`, step = 1),
                   sliderInput(
                       "weight_total_cycle", "Total Cycles", min = -20, max = 20, 
                       value = weights$`Total.Cycles`, step = 1),
                   sliderInput(
                       "weight_auto_bump", "Auto Bump", min = -20, max = 20, 
                       value = weights$`Auto.Bump`, step = 1),
                   sliderInput(
                       "weight_tele_bump", "Tele Bump", min = -20, max = 20, 
                       value = weights$`Tele.Bump`, step = 1),
                   sliderInput(
                       "weight_tele_trench", "Tele Trench", min = -20, max = 20, 
                       value = weights$`Tele.Trench`, step = 1)
            ),
            column(6,
                   sliderInput(
                       "weight_auto_climb", "Auto Climb", min = -20, max = 20, 
                       value = weights$`Auto.Climb`, step = 1),
                   sliderInput(
                       "weight_climb", "Climb", min = -20, max = 20, 
                       value = weights$`Climb`, step = 1),
                   sliderInput(
                       "weight_quick_climb", "Quick Climb", min = -20, max = 20, 
                       value = weights$`Quick.Climb`, step = 1),
                   sliderInput(
                       "weight_driver", "Driver", min = -20, max = 20, 
                       value = weights$`Driver`, step = 1),
                   sliderInput(
                       "weight_died", "Died", min = -20, max = 20, 
                       value = weights$Died, step = 1),
                   sliderInput(
                       "weight_card", "Card", min = -20, max = 20, 
                       value = weights$Card, step = 1)
            )
        ),
        
        footer = tagList(
            modalButton("Cancel"),
            actionButton("reset_weights", "Reset to Default", class = "btn-warning"),
            actionButton("apply_weights", "Apply Weights", class = "btn-primary")
        )
    )
}

inactive_stategy_summary <- function(raw, selected_teams, order, flip) {
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
        
        pivot_longer(
            cols = c("a_pass_1", "b_herd_2", "c_theif_3", "d_defense_oz_4",
                     "e_defense_nz_5", "f_intaked_full_6"),
            names_to = "comment_type",
            values_to = "level")
    
    team_order <- selected_teams
    comments$team <- factor(comments$team, levels = team_order, ordered = TRUE)
    comments$comment_type <- factor(
        comments$comment_type, 
        levels = c(
            "a_pass_1", "b_herd_2", "c_theif_3", "d_defense_oz_4", 
            "e_defense_nz_5", "f_intaked_full_6"), 
        ordered = TRUE
    )
    
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
        theme_bw() +
        {if (length(team_order) == 6)
            theme(
                axis.text.x = element_text(
                    color = ifelse(
                        levels(comments$team) %in% team_order[1:3],
                        "red", 
                        "blue"), size = 15)
            )
            else NULL
        }
}

auto_type_graph <- function(raw, order, teams, flip) {
    auto_type_data <- raw |>
        filter(team %in% teams) |>
        mutate(
            auto_type = factor(
                auto_type, 
                ordered = TRUE, 
                levels = c("1", "2", "3")))|>
        group_by(team, auto_type) |>
        summarise(
            auto_type_numbers = n()
        )
    
    team_order <- teams
    
    auto_type_data$team <- factor(auto_type_data$team, levels = team_order, ordered = TRUE)
    auto_type_data$auto_type <- factor(
        auto_type_data$auto_type, 
        c("1", "2", "3"), 
        ordered = TRUE)
    
    ggplot(auto_type_data, 
           aes(fill = auto_type, y = auto_type_numbers, x = factor(team))) + 
        geom_bar(position = "stack", stat = "identity") +
        labs(title = "Auto Types",
             x = "Team",
             y = "Number of Different Auto Types") + 
        scale_fill_manual(
            values = c("1" = "#996D99", 
                       "2" = "#CC91CC", "3" = "#F7B5F7"),
            labels = c("1" = "Depot", 
                       "2" = "Outpost/HP", "3" = "Neutral")
        ) +
        theme_bw()  +
        {if (length(teams) == 6)
            theme(
                axis.text.x = element_text(
                    color = ifelse(
                        levels(auto_type_data$team) %in% teams[1:3],
                        "red", 
                        "blue"), size = 15)
            )
            else NULL
        }
}

score_pred <- function(data, red, blue){
    red_total_score <- sum(data[data$Team %in% red, ]$`Total Score`)
    blue_total_score <- sum(data[data$Team %in% blue, ]$`Total Score`)
    red_auto_score <- sum(data[data$Team %in% red, ]$`Auto Fuel`)
    blue_auto_score <- sum(data[data$Team %in% blue, ]$`Auto Fuel`)
    
    paste0(
        "Final Scores: ", 
        "<span style='color:red;'>", round(red_total_score, digits = 0), 
        "<span style='color:black;'>", " - ", 
        "<span style='color:blue;'>", round(blue_total_score, digits = 0), 
        "<span style='color:black;'>", "\nAuto Scores: ",
        "<span style='color:red;'>", round(red_auto_score, digits = 0), 
        "<span style='color:black;'>", " - ", 
        "<span style='color:blue;'>", round(blue_auto_score, digits = 0))
}

#raw <- read.csv('shinyapp/data/test_data/data.csv')
#schedule <- read.csv('shinyapp/data/test_data/schedule.csv')
#tba_data <- read.csv('shinyapp/data/test_data/tba_data.csv')
#pridge <- read.csv('shinyapp/data/test_data/pridge.csv')
#teams_interested <- c(449, 611)