library(tidyverse)
library(scoutR)
library(purrr)
get_epa_progression <- function(team_key){
    tm1 <- team_matches_sb(team = team_key, elim = FALSE, year = 2015)
    tm2 <- team_matches_sb(team = team_key, elim = FALSE, year = 2016)
    tm3 <- team_matches_sb(team = team_key, elim = FALSE, year = 2017)
    tm4 <- team_matches_sb(team = team_key, elim = FALSE, year = 2018)
    tm5 <- team_matches_sb(team = team_key, elim = FALSE, year = 2019)
    tm6 <- team_matches_sb(team = team_key, elim = FALSE, year = 2022)
    tm7 <- team_matches_sb(team = team_key, elim = FALSE, year = 2023)
    tm8 <- team_matches_sb(team = team_key, elim = FALSE, year = 2024)
    tm9 <- team_matches_sb(team = team_key, elim = FALSE, year = 2025)
    tm <- rbind(tm1, tm2, tm3, tm4, tm5, tm6, tm7, tm8, tm9)
    #tm <- team_matches_sb(team = team_key, elim = FALSE)
    
    result <- map(tm, ~{
        data.frame(
            team = .x$team, 
            year = .x$year,
            event = .x$event,
            match = as.integer(sub(".*_qm", "", .x$match)),
            alliance = .x$alliance,
            time = .x$time, 
            week = .x$week,
            pre_epa = .x$epa$total_points
            #pre_epa = pluck(.x, "epa", "total_points", .default = NA)
        )}) |>
        list_rbind()
    return(result)
}

pridge_lambda_cv <- function(
        design, response, priors, lambda, plot_mses = TRUE, n_cores = NULL
){
    design <- as.matrix(design)
    
    # Leave one core free by default
    if (is.null(n_cores)) {
        n_cores <- max(1, parallel::detectCores() - 1)
    }
    return(pridge_loocv(design, response, lambda, priors))
}

fit_event_pridge <- function(
        event_key, lambda, n_cores = NULL
){
    matches <- event_matches(event_key, match_type = "qual")
    
    design <- as.matrix(lineup_design_matrix(matches))
    response <- c(matches$blue_score, matches$red_score)
    
    sb_data <- team_events_sb(event = event_key)
    epas <- sapply(sb_data, function(te){te$epa$stats$start})
    names(epas) <- sapply(sb_data, function(te){te$team})
    
    mses <- pridge_lambda_cv(
        design, response, epas, lambda, n_cores = n_cores, plot_mses = FALSE)
    return(mses)
}

calculate_all_team_epas <- function(){
    lambda_opt <- read.csv("pridge_pct_improvement15-25.csv") |>
        filter(!is.na(lambda_opt)) 
    unique_teams <- lambda_opt |>
        rowwise() |>
        mutate(
            teams = list(sort(sapply(event_teams(key)$team_number, as.integer)))
        ) # ~three minutes
    
    unique_teams <- sort(as.integer(unique(unlist(unique_teams$teams))))
    
    team_info <- lapply(unique_teams, get_epa_progression) |> #35 mins
        reduce(rbind) |>
        unique() #added post
    write.csv(team_info, "all_epas_for_all_teams.csv", row.names = FALSE)
}

calculate_real_scores <- function(){
    # I ran it overnight, not sure precisely how long it took (1 hour)
    real_scores <- map_dfr(
        sort(data$key), 
        ~event_matches(event_key = .x, match_type = "quals")) 
    #write.csv(real_scores, "data/real_scores.csv") #Debugging Checkpoint
    
    real_scores <- real_scores |>
        select(blue_score, red_score, key) |>
        pivot_longer(
            cols = c("blue_score", "red_score"),
            names_to = "alliance",
            values_to = "scores"
        ) |>
        rowwise() |>
        mutate(
            event = sub(paste0("_qm", ".*"), "", key),
            match = as.integer(sub(".*_qm", "", key))
        ) |>
        arrange(event, match, alliance)
    
    write.csv(real_scores, "prior_ridge/data/real_scores.csv")
}

# Calculate per-match estimated EPA alliance score, match the number of rows 
# between the real_scores and EPA-predicted scores, losing ~100 matches
# Takes quite a while to run, ~1 min
calculate_match_info <- function(real_scores, lambda_opt){
    data <- team_info |>
        group_by(event, match, alliance) |>
        summarise(
            pred_score = sum(pre_epa),
            num_teams = n() # Debugging, making sure 3 teams per alliance (true)
        ) |>
        ungroup() |>
        rowwise() |>
        mutate(
            event_key = paste0(event, "_qm", match)
        ) |>
        filter(event_key %in% real_scores$key)
    
    data$score <- real_scores$scores
    data <- data |>
        mutate(
            residual = pred_score - score
        ) |>
        group_by(event) |>
        summarise(
            EPA_mse = mean(residual^2)
        )
}

#-------------------- LOADING FROM CSVS --------------------

lambda_opt <- read.csv("prior_ridge/data/pridge_pct_improvement15-25.csv") |>
    filter(!is.na(lambda_opt)) 
team_info <- read.csv("prior_ridge/data/all_epas_for_all_teams.csv") |>
    arrange(event, match, alliance) |>
    filter(event %in% lambda_opt$key) |>
    unique()
real_scores <- read.csv("prior_ridge/data/real_scores.csv")

#data <- calculate_match_info(real_scores)
data <- read.csv("prior_ridge/data/final_data.csv")

data$key <- lambda_opt$key
data$pct_imp <- lambda_opt$pct_imp
data$lambda_opt <- lambda_opt$lambda_opt
data$pridge_mse <- lambda_opt$pridge_mse

data$rows <- 1:length(data$key)

years_interested <- c(2015:2019, 2022:2025) #2015:2019, 2022:2025
data2 <- filter(data, as.integer(substring(key, 0, 4)) %in% years_interested)

#write.csv(data2, "prior_ridge/data/epa_pridge_mse.csv")

ggplot(data2, aes(x = rows)) + 
    geom_line(aes(y = EPA_mse), color = "#a9000a") + 
    geom_line(aes(y = pridge_mse), color = "blue") +
    scale_y_log10() +
    labs(
        title = "EPA MSE vs. pridge MSE per event",
        x = "Event",
        y = "MSE, logarithmic"
    ) + 
    theme_bw()


# Possible TBD:
#   Stop scoutR local function spamming and just use pridge_loocv() with a
#   self-made design, response, priors, and the lambda_opt from pridge_pct...
#   Not impossible, given that we have all_epas_for_all_teams.csv where we could
#   derive match schedule and real_scores.csv for the final TBA scores
#   Is this (non-influencing final graph, only API calls) improvement worth 
#   the time spent?