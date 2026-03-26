library(tidyverse)
library(scoutR)

# event_key - event key without year prefix (ex. "mdbet")
# team_order - vector of team numbers defining which teams we want from the 
#   result, and in what order
alliance_selection_prep <- function(event_key, team_order){
    pit <- read_csv(paste0("pit_scouting/pit_scouting_", event_key, ".csv"))
    cidx <- c(grep("Team number", colnames(pit)), 
              grep("Drive base", colnames(pit)), 
              grep("Build quality", colnames(pit)))
    pit <- pit[, cidx]
    colnames(pit) <- c("team", "drive", "build_rtg")
    pit <- pit |>
        arrange(team)
    
    raw <- read_csv(paste0("shinyapp/data/", event_key, "/data.csv"))
    summ <- raw |>
        group_by(team) |>
        summarize(driver_rtg = round(mean(driver_rating, na.rm = TRUE), 2), 
                  avg_crossings = round(mean(teleop_bump, na.rm = TRUE) + 
                                            mean(teleop_trench, na.rm = TRUE), 2))
    
    tangibles <- event_tangibles(paste0("2026", event_key))
    tangibles$team <- scoutR:::id2int(tangibles$id)
    tangibles <- tangibles |>
        mutate(climb_score = 15 * autotower_level1_pct + 
                   10 * endgametower_level1_pct) |>
        select(team, climb_score) |>
        arrange(team)
    
    opr <- round(coef(fit_event_lr(paste0("2026", event_key))), 2)
    names(opr) <- as.numeric(scoutR:::id2int(names(opr)))
    auto_opr <- round(coef(fit_event_lr(paste0("2026", event_key), 
                                        response = "totalAutoPoints")), 2)
    names(auto_opr) <- names(opr)
    
    sb_data <- team_events_sb(event = paste0("2026", event_key))
    epas <- sapply(sb_data, function(te) {return(te$epa$stats$start)})
    names(epas) <- sapply(sb_data, function(te) {return(te$team)})
    # select only the EPAs whose names appear in the OPRs
    epas <- epas[names(opr)]
    
    matches <- event_matches(paste0("2026", event_key), match_type = "qual")
    design <- as.matrix(lineup_design_matrix(matches))
    response <- c(matches$blue_score, matches$red_score)
    mses <- pridge_lambda_cv(design, response, epas, 
                             grid = seq(0.001, 20, length.out = 1000))
    lambda_opt <- as.numeric(names(mses)[which.min(mses)])
    pridge <- round(scoutR:::prior_ridge(design, response, lambda_opt, epas), 2)
    
    advanced <- data.frame(
        team = as.numeric(names(opr)), pRidge = pridge, opr = opr, 
        auto_opr = auto_opr, epa = epas
    )
    
    result <- merge(advanced, pit, by = "team")
    result <- merge(result, tangibles, by = "team")
    result <- merge(result, summ, by = "team")
    
    # filter out any teams that we don't want in the output and order them
    result <- result[na.omit(match(team_order, result$team)), ]
    write_csv(result, file = paste0("shinyapp/data/", event_key, 
                                    "/selection_prep.csv"))
    return(result)
}
