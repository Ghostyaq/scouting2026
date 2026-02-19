library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(tidyverse)
library(shinythemes)
library(data.table)
library(stringi)
library(shinycssloaders)
library(hexbin)
library(ggbeeswarm)
library(bslib)
library(fmsb)
library(here)
library(shiny.fluent)
library(colourpicker)

data = read.csv("data.csv")
pridge = read.csv("pridge.csv")
tba_data = read.csv("tba_data.csv")
schedule = read.csv("schedule.csv")

#--------------------------------------------FUNCTION-------------------------------------------------------------------

#SUMMARIZE STATS - HELPER
summary_stats <- function(raw, schedule, pridge) {
    teams <- sort(unique(unlist(schedule[,2:7])))
    data <- raw |>
        filter(team %in% teams) |>
        group_by(team) |>
        summarise(
            Team = mean(team),
            `Matches Played` = n(),
            Climb = round(mean(
                ifelse(endgame_climb == "L1", 10, 
                       ifelse(endgame_climb == "L2", 20, 
                              ifelse(endgame_climb == "L3", 30, 0)))), 2),
            ACP = round(mean(auto_climb * 15), 2),
            `Auto Fuel` = round(pridge$auto_fuel[pridge$team == team], 2),
            `Tele Fuel` = round(pridge$tele_fuel[pridge$team == team], 2),
            `Total Fuel` = `Auto Fuel` + `Tele Fuel`,
            `Total Score` = `Auto Fuel` + `Tele Fuel` + ACP + Climb,
            `Auto Cycles` = round(mean(auto_cycles / 10), 2),
            `Tele Cycles` = round(mean(num_cycles + num_cycles_tenths / 10), 2),
            `Total Cycles` = `Auto Cycles` + `Tele Cycles`,
            `Auto Bump` = sum(auto_bump),
            `Tele Trench` = round(mean(teleop_trench), 2),
            `Tele Bump` = round(mean(teleop_bump), 2),
            `Auto Climb` = sum(auto_climb),
            Driver = round(mean(driver_rating), 2),
            `Quick Climb` = sum(climb_less_than_5),
            `Solo Shot` = sum(solo_shooting),
            Died = sum(died),
            Card = sum(card != 'No Card')
        ) |>
        select(
            Team, `Auto Fuel`, `Tele Fuel`, `Total Fuel`, `Total Score`,
            `Auto Cycles`, `Tele Cycles`, `Total Cycles`, `Auto Bump`,
            `Tele Bump`, `Tele Trench`, `Auto Climb`, Climb, `Quick Climb`, 
            Driver, `Solo Shot`, Died, Card, `Matches Played`, ACP)
}

#TRENCH BUMP BOXPLOT
bump_trench_boxplot <- function(raw, team_list){
    filtered_df <- raw |> filter(team %in% team_list)
    df_bump <- filtered_df |>
        select(team, count=teleop_bump) |> 
        mutate(obstacle = "Bump")
    
    df_trench <- filtered_df |> 
        select(team, count = teleop_trench) |> 
        mutate(obstacle = "Trench")
    
    combined_df <- rbind(df_bump,df_trench)
    
    ggplot(combined_df, aes(x = factor(team), y = count, fill = obstacle)) + 
        geom_boxplot(position = position_dodge(width = .75)) +
        labs(title = "Mean Crossing Comparison",
             x = "Team Number",
             y = "Average Times Crossed",
             fill = "Obstacle Type") + 
        theme_bw() +
        theme(
            legend.position = "bottom"
        )
}

#DRIVER RATING
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
        labs(
            x = "Match",
            y = "Driver Rating",
            color = "Teams",
            title = "Driver Rating") + 
        theme_bw() +
        theme(
            legend.position = "bottom"
        )
}

#ENDGAME SPREAD - BAR CHART
endgame_graph <- function(raw, teams) {
    number_of_teams <- length(unique(raw$team))
    specific_data <- raw |>
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
    
    ggplot(specific_data, 
           aes(fill = endgame_climb, y = number_of_climbs, x = factor(team))) + 
        geom_bar(position = "stack", stat = "identity") +
        labs(title = "Endgame climb",
             x = "Team",
             y = "Number of Climbs") + 
        scale_fill_manual(
            values = c("F" = "#f2b5d4", "No" = "#f7d6e0", "L1" = "#eff7f6", 
                       "L2" = "#b2f7ef", "L3" = "#7bdff2"),
            labels = c("F" = "Fail", "No" = "Didn't attempt", "L1" = "L1", 
                       "L2" = "L2", "L3" = "L3")
        ) +
        theme_bw() +
        theme(
            legend.position = "bottom"
        )
    
}

#IDK
prior_ridge <- function(X, y, lambda, beta_0) {
    stopifnot("lambda must be a single value" = {length(lambda) == 1})
    stopifnot("coefficients in beta_0 must match ncol(X)" =
                  {length(beta_0) == ncol(X)})
    p <- ncol(X)
    lambda <- diag(lambda, p)
    solve(crossprod(X) + lambda, crossprod(X, y) + lambda %*% beta_0)[, 1]
}

#IDK
pridge_calculation <- function(raw, schedule, tba_data, event_key) {
    unique_teams <- sort(unique(unlist(schedule[,2:7])))
    design <- matrix(0, 
                     nrow = length(unique(raw$match)) * 2, 
                     ncol = length(unique_teams))
    colnames(design) <- unique_teams
    
    for (i in 1:nrow(design)) {
        chipotle <- filter(
            raw,
            match == ceiling(i/2), 
            substring(robot, 1, 1) == ifelse(i %% 2, "R", "B"))
        design[i, as.character(chipotle$team)] = 1
    }
    
    response <- tba_data |>
        pivot_longer(
            cols = names(tba_data)[2:5],
            names_to = "alliance",
            values_to = "score"
        )
    
    priors <- rep(25, length(unique_teams))
    auto_priors <- rep(8, length(unique_teams))
    grid <- seq(0, 20, length.out = 1000)
    
    auto_mses <- pridge_lambda_cv(
        design, 
        response$score[!(response$alliance %in% c('red_score', 'blue_score'))], 
        auto_priors, grid, plot_mses = FALSE)
    
    tele_mses <- pridge_lambda_cv(
        design, 
        response$score[response$alliance %in% c('red_score', 'blue_score')], 
        priors, grid, plot_mses = FALSE)
    
    auto_lambda_opt <- grid[which.min(auto_mses)]
    tele_lambda_opt <- grid[which.min(tele_mses)]
    
    auto_fuel <- prior_ridge(
        design, 
        response$score[!(response$alliance %in% c('red_score', 'blue_score'))],  
        auto_lambda_opt, priors)
    tele_fuel <- prior_ridge(
        design, 
        response$score[response$alliance %in% c('red_score', 'blue_score')],  
        tele_lambda_opt, priors)
    
    priors_df <- data.frame(team = unique_teams, auto_fuel, tele_fuel)
    write.csv(priors_df, 
              paste0("shinyapp/data/", event_key, "/pridge.csv"), row.names = FALSE)
    getwd()
}

#SCOUTS
plot_scouting_graph <- function(raw) {
    scout <-raw$scout
    scout_count <- count(raw, scout, sort = TRUE, name = "number_of_times")
    
    still_graph <- ggplot(scout_count, aes(
        text = paste("Scout:", scout, "|| Count:", number_of_times),
        x = reorder(scout, number_of_times),
        y = number_of_times )) + 
        geom_col() +
        theme_bw() +
        labs(
            x = "Scout Initials",
            y = "Number of Times Scouted",
            title = "Scout and Their Number of Times Scouted")
    
    ggplotly(still_graph, tooltip = "text")
}

#CYCLES GRAPH
stacked_bar_chart <- function(raw, schedule, pridge, order, teams){
    data <- summary_stats(raw, schedule, pridge) |>
        select(Team, `Auto Fuel`, `Tele Fuel`, `ACP`, Climb, `Total Score`) |>
        rename(`Auto Climb` = ACP) |>
        arrange(desc(`Total Score`))|>
        filter(Team %in% teams)
    
    team_order <- data$Team
    
    data <- pivot_longer(
        data,
        cols = c('Auto Fuel', 'Tele Fuel', 'Auto Climb', 'Climb'),
        names_to = 'type',
        values_to = 'score',
    )
    
    data$Team <- factor(data$Team, levels = team_order, ordered = order)
    data$type <- factor(
        data$type, 
        c("Auto Fuel", "Auto Climb", "Tele Fuel", "Climb"), 
        ordered = TRUE)
    
    ggplot(data, aes(x = Team, y = score, fill = type)) +
        geom_bar(stat ="identity") + 
        labs(
            title = "Stacked Bar Chart", x = "Climb + PRidge Score", y = "Team"
        ) + 
        coord_flip() + 
        theme_bw()
}

#----------------------------------------------UI-----------------------------------------------------------------------

ui <- fluidPage(
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly"
    ),
    navbarPage(
        title = "2026 REBUILT 449 Data",
        tabPanel(
            title = "Event Summary",
            card(
                card_header("Event Summary"),
                plotOutput("event_summary")
            )
        ),
        tabPanel(
            title = "Single Team",
            layout_sidebar(
                sidebar = card(
                    virtualSelectInput("selected_team", label = "Select a Team", choices = NULL, 
                                       search = TRUE),
                    height = "500px"
                ),
                card(
                    card_header("Summary Fuel Points"),
                    plotOutput("summary_point")
                ),
                layout_columns(
                    card(
                        card_header("Trench Bump Relationship Boxplot"),
                        plotOutput("trench_bump")
                    ),
                    card(
                        card_header("Endgame Stacked Bar Chart"),
                        plotOutput("end_bar")
                    ),
                    card(
                        card_header("Driver Rating by Match"),
                        plotOutput("driver_rating")
                    )
                )
            )
        ),
        tabPanel(
            title = "Compare Teams",
            layout_sidebar(
                sidebar = card(
                    virtualSelectInput("selected_teams_comp", label = "Select Teams", choices = NULL, multiple = TRUE, search = TRUE
                    ),
                    height = "500px"
                ),
                layout_columns(
                    card(
                        card_header("Summary Fuel Points"),
                        plotOutput("summary_point_comp")
                    ),
                    card(
                        card_header("Endgame Stacked Bar Chart"),
                        plotOutput("end_bar_comp")
                    )
                ),
                layout_columns(
                    card(
                        card_header("Trench Bump Relationship Boxplot"),
                        plotOutput("trench_bump_comp")
                    ),
                    card(
                        card_header("Driver Rating by Match"),
                        plotOutput("driver_rating_comp")
                    )
                ),
            )
        ),
        tabPanel(
            title = "Match",
            layout_sidebar(
                sidebar = card(
                    virtualSelectInput("selected_match", label = "Select a Match", choices = NULL),
                    height = "500px"
                ),
                card(
                    card_header("Team Points Boxplot"),
                    plotOutput("boxplot_match")
                ),
                layout_columns(
                    card(
                        card_header("Autonomous/Teleop Stacked Bar Chart"),
                        plotOutput("fuel_bar_match")
                    ),
                    card(
                        card_header("Endgame Stacked Bar Chart"),
                        plotOutput("end_bar_match")
                    )
                )
            )
        ),
        tabPanel(
            title = "Scouts",
            layout_columns(
                card(
                    card_header("Average Yaps by Scout"),
                    plotlyOutput("scout_yaps")
                ),
                card(
                    card_header("Total Matches Scouted by Scout"),
                    plotlyOutput("matches_scouted")
                )
            )
        ),
        tabPanel(
            title = "Settings",
            layout_columns(
                card(
                    card_header("Refresh Data"),
                    actionButton("pridge_button", "Reload Data")
                ),
                card(
                    card_header("Custom Theme Color"),
                    ColorPicker("theme_color")
                )
            )
        )
    )
)

#-------------------------------------------SERVER----------------------------------------------------------------------

server <- function(input, output, session) {
    #UPDATE PICKERS
    observe({
        updateVirtualSelect("selected_match", choices = schedule$match)
        updateVirtualSelect("selected_team", choices = sort(unique(data$team)))
        updateVirtualSelect("selected_teams_comp", choices = sort(unique(data$team)))
    })
    
    #MATCHES SCOUTED
    output$matches_scouted <- renderPlotly({
        plot_scouting_graph(data)
    })
    
    #EVENT SUMMARY
    output$event_summary <- renderPlot({
        teams <- unique(data$team)
        stacked_bar_chart(data, schedule, pridge, TRUE, teams)
    })
    
    #POINT SUMMARY BAR PLOT
    output$summary_point <- renderPlot({
        team <- input$selected_team
        stacked_bar_chart(data, schedule, pridge, TRUE, team)
    })
    
    #ENDGAME BAR GRAPH
    output$end_bar <- renderPlot({
        team <- input$selected_team
        endgame_graph(data, team)
    })
    
    #DRIVER RATING OVER TIME
    output$driver_rating <- renderPlot({
        team <- input$selected_team
        plot_driver_rating_graph(data, team)
    })
    
    #TRENCH BUMP BOXPLOT
    output$trench_bump <- renderPlot({
        team <- input$selected_team
        bump_trench_boxplot(data, team)
    })
    
    #COMPARE POINT SUMMARY
    output$summary_point_comp <- renderPlot({
        team <- input$selected_teams_comp
        stacked_bar_chart(data, schedule, pridge, TRUE, team)
    })
    
    #COMPARE ENDGAME BAR
    output$end_bar_comp <- renderPlot({
        team <- input$selected_teams_comp
        endgame_graph(data, team)
    })
    
    #COMPARE DRIVER RATING
    output$driver_rating_comp <- renderPlot({
        team <- input$selected_teams_comp
        plot_driver_rating_graph(data, team)
    })
    
    #COMPARE TRENCH BUMP
    output$trench_bump_comp <- renderPlot({
        team <- input$selected_teams_comp
        bump_trench_boxplot(data, team)
    })
}

#--------------------------------------------INITIALIZE-----------------------------------------------------------------

shinyApp(ui = ui, server = server)