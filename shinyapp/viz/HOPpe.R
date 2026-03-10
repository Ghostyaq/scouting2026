pridge_hopper_offline <- function(event_key) {
    data_dir_path <- paste0("shinyapp/data/", event_key)
    schedule <- read.csv(paste0(data_dir_path, "/schedule.csv"))
    tba_data <- read.csv(paste0(data_dir_path, "/tba_data.csv"))
    statbotics_data <- read.csv(paste0(data_dir_path, "/statbotics_data.csv"))
    
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
            substring(robot, 1, 1) == ifelse(i %% 2, "B", "R"),)
        design[i, as.character(chipotle$team)] = as.integer(chipotle$num_cycles*10 + chipotle$num_cycles_tenths)
    }
    
    response <- tba_data |>
        pivot_longer(
            cols = names(tba_data)[2:5],
            names_to = "alliance",
            values_to = "score"
        )
    response = response[1:300,]
    
    auto_priors <- statbotics_data$auto_fuel_epa
    tele_priors <- statbotics_data$tele_fuel_epa
    names(auto_priors) <- names(tele_priors) <- statbotics_data$team
    grid <- seq(0, 20, length.out = 1000)
    tele_fuel_columns <- c('red_tele_fuel', 'blue_tele_fuel') # 80 char limit
    
    cols_to_keep <- apply(design != 0, 2, any)
    design <- design[, cols_to_keep]
    auto_priors <- auto_priors[cols_to_keep]
    tele_priors <- tele_priors[cols_to_keep]
    
    auto_mses <- scoutR:::pridge_lambda_cv(
        design, 
        response$score[!(response$alliance %in% tele_fuel_columns)], 
        auto_priors, grid, plot_mses = FALSE, n_cores = 1)
    
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
    
    priors_df <- data.frame(team = colnames(design), auto_fuel, tele_fuel)
    write.csv(
        priors_df, 
        paste0("shinyapp/data/", event_key, "/HOPpeR.csv"), row.names = FALSE)
}