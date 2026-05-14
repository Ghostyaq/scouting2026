pridge_hopper_offline <- function(event_key) {
    data_dir_path <- paste0("shinyapp/data/", event_key)
    schedule <- read.csv(paste0(data_dir_path, "/schedule.csv"))
    tba_data <- read.csv(paste0(data_dir_path, "/tba_data.csv"))
    statbotics_data <- read.csv(paste0(data_dir_path, "/statbotics_data.csv"))
    raw <- read.csv(paste0(data_dir_path, "/data.csv"))
    pridge <- read.csv(paste0(data_dir_path, "/pridge.csv"))
    
    unique_teams <- sort(unique(unlist(schedule[,2:7])))
    design <- matrix(0, 
                     nrow = length(unique(raw$match)) * 2, 
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
            raw,
            match == matches[ceiling(i/2)], 
            (team == ifelse(i %% 2, schedule[matches[ceiling(i/2)],5], schedule[matches[ceiling(i/2)],2]) |
                team == ifelse(i %% 2, schedule[matches[ceiling(i/2)],6], schedule[matches[ceiling(i/2)],3]) |
                team == ifelse(i %% 2, schedule[matches[ceiling(i/2)],7], schedule[matches[ceiling(i/2)],4])) &
                team %in% unique_teams) 
        design[i, as.character(chipotle$team)] = as.integer(chipotle$auto_cycles + chipotle$num_cycles*10 + chipotle$num_cycles_tenths)/10
        #if (length(unique(chipotle$team)) != 3){
        #    print(ceiling(i/2))
        #}
    }
    
    response <- tba_data |> 
        mutate(blue_fuel = blue_auto_fuel + blue_tele_fuel, 
               red_fuel = red_auto_fuel + red_tele_fuel) |>
        select(match, blue_fuel, red_fuel)
    
    response <- response |>
        pivot_longer(
            cols = names(response)[2:3],
            names_to = "alliance",
            values_to = "score"
        ) |> 
        filter(match <= nrow(design)/2)

    
    auto_priors <- statbotics_data$auto_fuel_pre_epa
    tele_priors <- statbotics_data$tele_fuel_pre_epa
    names(auto_priors) <- names(tele_priors) <- statbotics_data$team
    grid <- seq(0, 0.5, length.out = 1000)
    
    cycles_df <- raw |> 
        group_by(team) |>
        summarize(cycles = mean(auto_cycles/10 + num_cycles + num_cycles_tenths/10))
    
    cols_to_keep <- colSums(design != 0) >= 2
    cycles_df <- cycles_df[cols_to_keep, ]
    design <- design[, cols_to_keep]
    auto_priors <- auto_priors[cols_to_keep]
    tele_priors <- tele_priors[cols_to_keep]
    priors <- (auto_priors + tele_priors)/cycles_df$cycles
    
    mses <- scoutR:::pridge_lambda_cv(
        design, 
        response$score, 
        priors, grid, plot_mses = TRUE)
    
    lambda_opt <- grid[which.min(mses)]
    
    fuel <- round(scoutR:::prior_ridge(
        design, 
        response$score,  
        lambda_opt, priors), 2)
    
    priors_df <- data.frame(team = colnames(design), 
                            HOPpeR = fuel, 
                            total_fuel = fuel*cycles_df$cycles, 
                            mean_cycles = round(cycles_df$cycles,2))
    for (i in 1:nrow(pridge)){
        if(!cols_to_keep[i]){
            priors_df[nrow(priors_df) + 1,] = c(unique_teams[i], 0, 0, 0)
            rownames(priors_df)[nrow(priors_df)] = unique_teams[i]
        }
    }
    priors_df <- priors_df |> arrange(as.integer(team))
    
    pridge <- pridge |> mutate(HOPpeR = priors_df$HOPpeR, cycles = priors_df$mean_cycles)
    
    write.csv(
        pridge, 
        paste0("shinyapp/data/", event_key, "/pridge.csv"), row.names = FALSE)
}
