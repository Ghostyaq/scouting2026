library(shiny)
library(DT)
library(ggplot2)
library(httr2)
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

default_linear_weights <- data.frame(
    Team = 0,
    `Auto Fuel` = 1, `Tele Fuel` = 1, `Total Fuel` = 1, `Total Score` = 0,
    `Auto Cycles` = 0, `Tele Cycles` = 0, `Total Cycles` = 0,
    `Auto Bump` = 10, `Tele Bump` = 10, `Tele Trench` = 5, 
    `Auto Climb` = 15, Climb = 15, `Quick Climb` = 15,
    Driver = 10, `Solo Shot` = 0, Died = 0, Card = -20, `Matches Played` = 0,
    `ACP` = 0
) #temp, remove later

raw <- read.csv("data/week0/data.csv")
pridge <- read.csv("data/week0/pridge.csv")
tba_data <- read.csv("data/week0/tba_data.csv")
schedule <- read.csv("data/week0/schedule.csv")
alliances <- read.csv("data/week0/alliances.csv")
weights <- reactiveVal(default_linear_weights)
team <- reactiveVal(sort(unique(raw$team)))

addResourcePath("images_d", "data/test_data/images")
addResourcePath("heatmaps", "../subjective_scouting/pathImages/finals")
in_rstudio <- rstudioapi::isAvailable()

server <- function(input, output, session) {
    #UPDATE PICKERS
    observe({
        unique_teams <- sort(unique(raw$team))
        updateVirtualSelect("selected_match", choices = schedule$match)
        updateVirtualSelect("selected_teams_comp", choices = unique_teams)
        updateVirtualSelect("selected_red", choices = alliances$alliance)
        updateVirtualSelect("selected_blue", choices = alliances$alliance)
    })
    
    #Password logic
    correct_password <- "0322"
    user_logged_in <- reactiveVal(in_rstudio)
    
    #EVENT SUMMARY
    output$event_summary <- renderPlot({
        teams <- unique(raw$team)
        stacked_bar_chart(raw, schedule, pridge, TRUE, teams, TRUE)
    })
    
    output$summary_stats <- renderDT({
        dataframe <- summary_stats(raw, pridge)
        datatable(
            dataframe,
            options = list(
                pageLength = nrow(dataframe)
            )
        )
    })
    
    #AUTO PICKLISTING
    observeEvent(input$open_weights, {
        showModal(weights_modal(weights()))
    })
    
    observeEvent(input$apply_weights, {
        new_weights <- data.frame(
            Team = 0,
            `Auto Fuel` = input$weight_auto_fuel,
            `Tele Fuel` = input$weight_tele_fuel,
            `Total Fuel` = input$weight_total_fuel,
            `Total Score` = input$weight_total_score,
            `Auto Cycles` = input$weight_auto_cycle,
            `Tele Cycles` = input$weight_tele_cycle,
            `Total Cycles` = input$weight_total_cycle,
            `Auto Bump` = input$weight_auto_bump,
            `Tele Bump` = input$weight_tele_bump,
            `Tele Trench` = input$weight_tele_trench,
            `Auto Climb` = input$weight_auto_climb, Climb = input$weight_climb,
            `Quick Climb` = input$weight_quick_climb,
            Driver = input$weight_driver, Died = input$weight_died,
            Card = input$weight_card, `Matches Played` = 0,
            `ACP` = 0
        )
        weights(new_weights)
        removeModal()
    })
    
    output$auto_picklist <- renderDT({
        data <- summary_stats(raw, pridge)
        team_scores <- calculate_team_scores(weights(), data)
        team_scores$Rank <- 1:nrow(team_scores)
        
        #reorder columns to show rank and score first
        cols_order <- c("Rank", "Team", "Team Score")
        remaining_cols <- setdiff(names(team_scores), cols_order)
        team_scores <- team_scores[, c(cols_order, remaining_cols)]
        
        #datatable
        datatable(team_scores, 
                  options = list(
                      pageLength = length(team_scores$Team),
                      dom = 'ftip',
                      scrollX = TRUE
                  ),
                  rownames = FALSE) |>
            formatStyle('Team Score',
                        background = styleColorBar(
                            c(0, max(team_scores$`Team Score`)), 'lightblue'),
                        backgroundSize = '100% 90%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center')    
    })
    
    # UPDATE SELECTED TEAMS
    observeEvent(input$selected_teams_comp, {
        team(input$selected_teams_comp)
    })
    
    observeEvent(c(input$selected_red, input$selected_blue), {
        red_alliance <- alliances[alliances$alliance == input$selected_red,]
        blue_alliance <- alliances[alliances$alliance == input$selected_blue,]
        red_alliance <- c(red_alliance$C, red_alliance$FP, red_alliance$SP)
        blue_alliance <- c(blue_alliance$C, blue_alliance$FP, blue_alliance$SP)
        team(c(red_alliance, blue_alliance))
    })
    
    #COMPARE POINT SUMMARY
    output$summary_point_comp <- renderPlot({
        stacked_bar_chart(raw, schedule, pridge, FALSE, team(), FALSE)
    })
    
    #COMPARE ENDGAME BAR
    output$end_bar_comp <- renderPlot({
        endgame_graph(raw, team())
    })
    
    #COMPARE DRIVER RATING
    output$driver_rating_comp <- renderPlot({
        plot_driver_rating_graph(raw, team())
    })
    
    # COMPARE INACTIVE STRATEGY
    output$inactive_strategy_comp <- renderPlot({
        inactive_stategy_summary(raw, team(), FALSE, FALSE)
    })
    
    #COMPARE TRENCH BUMP
    output$trench_bump_comp <- renderPlot({
        bump_trench_boxplot(raw, team())
    })
    
    # COMPARE AUTO TYPE
    output$auto_type_comp <- renderPlot({
        auto_type_graph(raw, FALSE, team(), FALSE)
    })
    
    output$comments_df_comp <- renderDT({
        req(user_logged_in())
        comments_df(raw, team())
    })
    
    observeEvent(input$selected_match, {
        teams <- schedule |>
            filter(match == input$selected_match) |>
            pivot_longer(
                cols = c(R1, R2, R3, B1, B2, B3), 
                names_to = "position", 
                values_to = "tnum") |>
            pull(tnum)
        
        team(teams)
    })
    
    #SUMMARY POINT MATCH
    output$summary_point_match <- renderPlot({
        stacked_bar_chart(raw, schedule, pridge, FALSE, team(), FALSE)
    })
    
    output$summary_stats_comp <- renderDT({
        summary_stats(raw, pridge, teams = team())
    })
    
    output$login_ui <- renderUI({
        if (!user_logged_in()) {
            tagList(
                passwordInput("password", "Enter password to access comments:"),
                actionButton("login", "Login")
            )
        }
    })
    
    observeEvent(input$login, {
        if (input$password == correct_password) {
            user_logged_in(TRUE)
        } else {
            user_logged_in(FALSE)
        }
    })
    
    output$login_status <- renderUI({
        req(input$login)
        if (user_logged_in()) {
            tags$p(style = "color: green;", "Access granted.")
        } else {
            tags$p(style = "color: red;", "Incorrect password.")
        }
    })
    
    output$end_bar_match <- renderPlot({
        endgame_graph(raw, team())
    })
    
    output$trench_bump_match <- renderPlot({
        bump_trench_boxplot(raw, team())
    })
    
    output$driver_rating_match <- renderPlot({
        plot_driver_rating_graph(raw, team())
    })
    
    output$inactive_strategy_match <- renderPlot({
        inactive_stategy_summary(raw, team(), FALSE, FALSE)
    })
    
    output$auto_type_match <- renderPlot({
        auto_type_graph(raw, FALSE, team(), FALSE)
    })
    
    output$summary_stats_match <- renderDT({
        summary_stats(raw, pridge, team())
    })
    
    output$comments_df_match <- renderDT({
        req(user_logged_in())
        comments_df(raw, team())
    })
    
    output$matches_scouted <- renderPlotly({
        plot_scouting_graph(raw)
    })
    
    output$scout_yaps <- renderPlotly({
        yap_graph(raw)
    })
    
    output$scouter_streak <- renderPlot({
        high_streak(raw)
    })
    
    output$images_comp <- renderUI({
        tags <- lapply(team(), function(teamnum) {
            img_src <- paste0("images_d/", teamnum,".png")
            tag_temp <- tags$img(
                src = img_src, 
                alt = paste("Robot Image for Team", teamnum), 
                style = "height: 90%; width: auto; object-fit: cover;")
            
            cap_tag <- tags$p(
                paste("Team:", teamnum), 
                style = "text-align: center;")
            
            full <- tags$div(
            tag_temp, cap_tag, 
            style = "display: flex; flex-direction: column; 
            align-items: center; height: 300px; padding: 5px; 
            border: 1px solid #555; overflow: hidden;")
            
            column(4, full, style = "padding: 5px;")
        })
        
        fluidRow(tags)
    })
    
    output$images_match <- renderUI({
        tags_m <- lapply(team(), function(team) {
            img_src_m <- paste0("images_d/", team,".png")
            tag_temp_m <- tags$img(
                src = img_src_m, 
                alt = paste("Robot Image for Team", team), 
                style = "height: 90%; width: auto; object-fit: cover;")
            
            cap_tag_m <- tags$p(
                paste("Team:", team), 
                style = "text-align: center;")
            
            full_m <- tags$div(
                tag_temp_m, cap_tag_m, 
                style = "display: flex; flex-direction: column; 
                align-items: center; height: 300px; padding: 5px; 
                border: 1px solid #555; overflow: hidden;")
            
            column(4, full_m, style = "padding: 5px;")
        })
        
        fluidRow(tags_m)
    })
    
    output$auto_heatmap_comp <- renderUI({
        tags <- lapply(team(), function(teamnum) {
            img_src <- paste0("heatmaps/", teamnum,".png")
            tag_temp <- tags$img(
                src = img_src, 
                alt = paste("Robot Auto Heatmap for Team", teamnum), 
                style = "height: auto; width: 90%; object-fit: cover;")
            
            cap_tag <- tags$p(
                paste("Team:", teamnum), 
                style = "text-align: center;")
            
            full <- tags$div(
            tag_temp, cap_tag, 
            style = "display: flex; flex-direction: column; 
            align-items: center; height: 250px; padding: 5px; 
            border: 1px solid #555; overflow: hidden;")
            
            column(6, full, style = "padding: 5px;")
        })
        
        fluidRow(tags)
    })
    
    output$auto_heatmap_match <- renderUI({
        tags_m <- lapply(team(), function(team) {
            img_src_m <- paste0("heatmaps/", team,".png")
            tag_temp_m <- tags$img(
                src = img_src_m, 
                alt = paste("Robot Auto Heatmap for Team", team), 
                style = "height: auto; width: 90%; object-fit: cover;")
            
            cap_tag_m <- tags$p(
                paste("Team:", team), 
                style = "text-align: center;")
            
            full_m <- tags$div(
                tag_temp_m, cap_tag_m, 
                style = "display: flex; flex-direction: column; 
                align-items: center; height: 250px; padding: 5px; 
                border: 1px solid #555; overflow: hidden;")
            
            column(6, full_m, style = "padding: 5px;")
        })
        
        fluidRow(tags_m)
    })
}
