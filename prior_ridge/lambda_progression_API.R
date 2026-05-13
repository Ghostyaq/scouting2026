library(tidyverse)
library(scoutR)
library(ggplot2)

calc_by_match <- function(curr, design, response, priors, event_key, tba_data, sb_data){
    design_l <- design[1:(curr * 2), ]
    response_l <- response[1:(curr * 2)]
    
    attempt <- tryCatch({
        mses <- pridge_lambda_cv(design_l, response_l, priors, grid)
        lambda_opt <- grid[which.min(mses)]
        mh <- prior_ridge(design_l, response_l, lambda_opt, priors)
        mh <- round(mh, 2)
        message("Succeeded on ", event_key, ", Match ", curr)
        list(mh, lambda_opt)
    }, 
    error = function(e){
        message("Failed on ", event_key, ", Match ", curr)
        message(e$message)
        list(priors, NA)
    })
    
    pridge <- attempt[[1]]
    lambda_opt <- attempt[[2]]
    
    pridge_blue_score <- sum(design[2 * curr + 1, ] * pridge)
    pridge_red_score <- sum(design[2 * curr + 2, ] * pridge)
    epa_blue_score <- sb_data |> 
        filter(match == (curr + 1)) |>
        filter(alliance == "blue") |>
        pull(pre_epa) |>
        sum()
    epa_red_score <- sb_data |> 
        filter(match == (curr + 1)) |>
        filter(alliance == "red") |>
        pull(pre_epa) |>
        sum()
    real_blue_score <- response[2 * curr + 1]
    real_red_score <- response[2 * curr + 2]
    pridge_match_acc <- !xor(
        pridge_red_score > pridge_blue_score, 
        real_red_score > real_blue_score
    )
    epa_match_acc <- !xor(
        epa_red_score > epa_blue_score, 
        real_red_score > real_blue_score
    )
    
    result <- data.frame(
        match = curr + 1, pridge_blue_score, pridge_red_score, epa_blue_score, 
        epa_red_score, real_blue_score, real_red_score, 
        pridge_match_acc, epa_match_acc, lambda_opt)
    return(result)
}

acc_comp_event <- function(event_key){
    print(event_key)
    
    sb_data <- tryCatch({
        
    })
    tba_data <- all_tba_data |> filter(event == event_key) |> unique()
    
    design <- matrix(0, nrow(sb_data) / 3, length(unique(sb_data$team)))
    colnames(design) <- sort(unique(sb_data$team))
    
    # Assumption: We sort the design matrix by 1B, 1R, 2B, 2R, etc.
    for (i in 1:nrow(design)) {
        mh <- sb_data |> 
            filter(match == ceiling(i / 2)) |>
            filter(alliance == ifelse(i %% 2, "blue", "red"))
        design[i, as.character(mh$team)] = 1
    }
    
    priors <- sb_data |> 
        arrange(team) |> 
        group_by(team) |> 
        filter(match == min(match)) |>
        pull(pre_epa)
    
    response <- tba_data |> arrange(match, alliance) |> pull(scores)
    grid <- exp(seq(log(0.01), log(20), length.out = 100))
    
    matches_to_predict = 0:(max(tba_data$match) - 1)
    print(max(tba_data$match) - 1)
    result <- map(
        matches_to_predict, 
        ~calc_by_match(.x, design, response, priors, event_key, tba_data, sb_data)) |>
        list_rbind()
    
    data.frame(event_key, result)
}

#a <- acc_comp_event("2025vaale")

#ggplot(a, aes(x = match, y = lambda_opt)) + 
#    geom_line() + 
#    ylim(min = 0 , max = 25) + 
#    theme_bw()

#reefscape_sb <- all_sb_data |> filter(year == 2025) |> pull(event) |> unique()
#reefscape_tba <- all_tba_data |> 
#    filter(substr(event, 1, 4) == "2025") |> 
#    pull(event) |> unique()
#reefscape_between <- intersect(reefscape_sb, reefscape_tba)
#final_result <- map(reefscape_between, acc_comp_event) |> 
#    list_rbind()

all_teams <- 1:11527
tm1 <- team_matches_sb(team == all_teams, elim = FALSE, year = 2026)
