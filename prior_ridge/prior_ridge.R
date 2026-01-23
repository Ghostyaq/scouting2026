library(scoutR)
library(glmnet)
library(tidyverse)
library(plotly)

rm(list = ls())

prior_ridge <- function(X, y, lambda, beta_0) {
    p <- ncol(X)
    lambda <- diag(lambda, p)
    solve(crossprod(X) + lambda, crossprod(X, y) + lambda %*% beta_0)
}

event_code <- "2023txfor"

evnt <- event_matches(event_code, match_type = "qual") |>
    dplyr::select(red1, red2, red3, blue1, blue2, blue3, red_score, blue_score)

# helper fxn does c(blue, red) so I match that with the response vector
design <- as.matrix(lineup_design_matrix(evnt))
response <- c(evnt$blue_score, evnt$red_score)

opr <- round(coefficients(fit_event_lr(event_code)), digits = 2)

# what do I need to do more? 
# well, I need to define whether we're using pre-event EPA as the baseline, 
# or whether we're using updated versions. Because that will matter a great
# deal to the selection of lambda. 

# obviously the model would be better with constant updates to the ELO baseline
# but that's computationally tricky, both for computing the actual coefficients
# and for the optimization of lambda. 

# for now, let's just muck around assuming that we're using the pre-event EPA
te <- team_events_sb(event = event_code)
start_epas <- sapply(te, function(te){te$epa$stats$start})
names(start_epas) <- sapply(te, function(te){te$team})
preelim_epas <- sapply(te, function(te){te$epa$stats$pre_elim})
names(preelim_epas) <- names(start_epas)

# this will optimize lambda for standard ridge regression
lambdas <- 10^seq(3, -2, by = -.1)
cv_fit <- cv.glmnet(as.matrix(design), response, alpha = 0, nlambda = 10000)
lambda <- cv_fit$lambda.min

# ok, so I think the optimal lambda is around 5 for ridge regression. 
# With a prior we probably want to regularize a little less. But we can
# start there for now.
priored <- prior_ridge(as.matrix(design), response, lambda, start_epas)[, 1]
priored <- round(priored, digits = 2)

diff <- priored - preelim_epas

comparison <- cbind(start_epas, opr, priored, preelim_epas, diff)

viz <- data.frame(comparison)
viz$team <- rownames(comparison)

plt <- ggplot(viz, aes(team = team, x = priored, y = preelim_epas)) + 
    geom_point() + 
    geom_abline(intercept = 0, slope = 1, lty = 2, col = "red") + 
    labs(title = paste0(event_code, " prior-ridge against pre-elim EPAs"), 
         x = "Prior Ridge Coefficient", y = "Pre-elim Statbotics EPA")

ggplotly(plt)

###########################
#### Optimizing Lambda ####
###########################

# I think we should proceed with a *fixed* EPA baseline, rather than a movable
# one, at least for now. For one, it simplifies the optimization (great!), but
# additionally the "baseline" has a more consistent interpretation now. Also, 
# realistically recomputing the EPAs will be *computationally intensive*.

# How should we hold out the test set? The last couple rounds of matches? Or
# maybe purely random? LOOCV?
