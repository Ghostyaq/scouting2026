library(tidyverse)
library(scoutR)
library(tidygeocoder)
library(leaflet)
library(broom)

years <- 2023:2026
chs <- c("MD", "VA", "DC")

temp <- lapply(years, events) |> list_rbind()
chesapeake <- temp |> filter(state_prov %in% chs) |> filter(event_type == 1)
rm(temp)

interested <- chesapeake |>
    select(key, week, year, event_type_string)

interested <- interested |>
    rowwise() |>
    mutate(
        opr_stats = list(tidy(summary(event_oprs(key)$opr))),
        num_teams = length(event_teams(key)$key)
    ) |>
    unnest(opr_stats)

ggplot(interested, aes(x = week, y = num_teams)) +
    geom_bar(stat = "identity") + 
    theme_bw() + 
    facet_wrap(~year)
