rm(list = ls())
load("robojawn/robojawn_prescout.rda")
load("robojawn/scores.rda")

library(tidyverse)
library(scoutR)

grid = exp(seq(log(0.01), log(20), length.out = 100))

priors <- robojawn_prescout$epa
names(priors) <- robojawn_prescout$id

fit_opr <- fit_lineup_lm(
    scores, responses = list(red = scores$red_total, blue = scores$blue_total)
)

oprs <- coef(fit_opr)
names(oprs) <- substr(names(oprs), 2, nchar(oprs))

design <- as.matrix(lineup_design_matrix(scores))
response <- c(scores$blue_total, scores$red_total)

mses <- pridge_lambda_cv(design, response, priors, grid)

lambda_opt <- grid[which.min(mses)]
result <- prior_ridge(design, response, lambda_opt, priors)
result <- round(result, 2)
names(result) <- substr(names(result), 2, nchar(names(result)))
