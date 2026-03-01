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

default_linear_weights <- data.frame(
    Team = 0, `ACP` = 0,
    `Auto Fuel` = 1, `Tele Fuel` = 1, `Total Fuel` = 1, `Total Score` = 0,
    `Auto Cycles` = 0, `Tele Cycles` = 0, `Total Cycles` = 0,
    `Auto Bump` = 10, `Tele Bump` = 10, `Tele Trench` = 5, 
    `Auto Climb` = 15, Climb = 15, `Quick Climb` = 15,
    Driver = 10, `Solo Shot` = 0, Died = 0, Card = -20, `Matches Played` = 0
) #temp, remove later

raw <- read.csv("data/week0/data.csv")
pridge <- read.csv("data/week0/pridge.csv")
tba_data <- read.csv("data/week0/tba_data.csv")
schedule <- read.csv("data/week0/schedule.csv")
weights <- reactiveVal(default_linear_weights)

addResourcePath("images_d", "data/test_data/images")

server <- function(input, output, session) {
    #UPDATE PICKERS
    observe({
        updateVirtualSelect("selected_match", choices = schedule$match)
        updateVirtualSelect("selected_team", choices = sort(unique(raw$team)))
        updateVirtualSelect("selected_teams_comp", choices = c(449, 422))
    })
    
    #MATCHES SCOUTED
    output$matches_scouted <- renderPlotly({
        plot_scouting_graph(raw)
    })
    
    #EVENT SUMMARY
    output$event_summary <- renderPlot({
        teams <- unique(raw$team)
        stacked_bar_chart(raw, schedule, pridge, TRUE, teams, TRUE)
    })
    
    output$summary_stats <- renderDT({
        summary_stats(raw, pridge)
    })
    
    #AUTO PICKLISTING
    observeEvent(input$open_weights, {
        showModal(weights_modal(weights()))
    })
    
    observeEvent(input$apply_weights, {
        new_weights <- data.frame(
            Team = 0, `ACP` = 0,
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
            Card = input$weight_card, `Matches Played` = 0
        )
        weights(new_weights)
        removeModal()
    })
    
    output$auto_picklist <- renderDT({
        data <- summary_stats(raw, pridge)
        team_scores <- calculate_team_scores(weights, data)
        team_scores$Rank <- 1:nrow(team_scores)
        
        #reorder columns to show rank and score first
        cols_order <- c("Rank", "Team", "Team Score")
        remaining_cols <- setdiff(names(team_scores), cols_order)
        team_scores <- team_scores[, c(cols_order, remaining_cols)]
        
        #datatable
        datatable(team_scores, 
                  options = list(
                      pageLength = 15,
                      dom = 'ftip',
                      scrollX = TRUE
                  ),
                  rownames = FALSE)# |>
        #formatStyle('Team Score',
        #            background = styleColorBar(
        #                c(0, max(team_scores$`Team Score`)), 'lightblue'),
        #            backgroundSize = '100% 90%',
        #            backgroundRepeat = 'no-repeat',
        #            backgroundPosition = 'center')    
    })
    
    #COMPARE POINT SUMMARY
    output$summary_point_comp <- renderPlot({
        team <- input$selected_teams_comp
        stacked_bar_chart(raw, schedule, pridge, FALSE, team, FALSE)
    })
    
    #COMPARE ENDGAME BAR
    output$end_bar_comp <- renderPlot({
        team <- input$selected_teams_comp
        endgame_graph(raw, team)
    })
    
    #COMPARE DRIVER RATING
    output$driver_rating_comp <- renderPlot({
        team <- input$selected_teams_comp
        plot_driver_rating_graph(raw, team)
    })
    
    #COMPARE TRENCH BUMP
    output$trench_bump_comp <- renderPlot({
        team <- input$selected_teams_comp
        bump_trench_boxplot(raw, team)
    })
    
    #SUMMARY POINT MATCH
    output$summary_point_match <- renderPlot({
        #Make sure sure that the input is within data manipulation pipeline
        teams <- schedule |>
            filter(match == input$selected_match) |>
            pivot_longer(
                cols = c(R1, R2, R3, B1, B2, B3), 
                names_to = "position", 
                values_to = "tnum") |>
            pull(tnum)
        
        stacked_bar_chart(raw, schedule, pridge, FALSE, teams, FALSE)
    })
    
    output$end_bar_match <- renderPlot({
        teams <- schedule |>
            filter(match == input$selected_match) |>
            pivot_longer(
                cols = c(R1, R2, R3, B1, B2, B3), 
                names_to = "position", 
                values_to = "tnum") |>
            pull(tnum)
        
        endgame_graph(raw, teams)
    })
    
    output$trench_bump_match <- renderPlot({
        teams <- schedule |>
            filter(match == input$selected_match) |>
            pivot_longer(
                cols = c(R1, R2, R3, B1, B2, B3), 
                names_to = "position", 
                values_to = "tnum") |>
            pull(tnum)
        
        bump_trench_boxplot(raw, teams)
    })
    
    output$driver_rating_match <- renderPlot({
        teams <- schedule |>
            filter(match == input$selected_match) |>
            pivot_longer(
                cols = c(R1, R2, R3, B1, B2, B3), 
                names_to = "position", 
                values_to = "tnum") |>
            pull(tnum)
        
        plot_driver_rating_graph(raw, teams)
    })
    
    output$summary_stats_comp <- renderDT({
        summary_stats(raw, pridge, input$selected_teams_comp)
    })
    
    output$summary_stats_match <- renderDT({
        teams <- schedule |>
            filter(match == input$selected_match) |>
            pivot_longer(
                cols = c(R1, R2, R3, B1, B2, B3), 
                names_to = "position", 
                values_to = "tnum") |>
            pull(tnum)
        
        summary_stats(raw, pridge, teams)
    })
    
    output$scout_yaps <- renderPlotly({
        yap_graph(raw)
    })
    
    output$images <- renderUI({
        teamnum <- input$selected_teams_comp
        teamnum1 = 422
        img_src <- paste0("images_d/", teamnum,".png")
        img_src1 <- paste0("images_d/", teamnum1,".png")
        tags$img(src = img_src, alt = paste("Robot Image for Team", teamnum), style = "width: 100%; height: auto; display: block; margin: 0 auto;")
        tags$img(src = img_src1, alt = paste("Robot Image for Team", teamnum1), style = "width: 100%; height: auto; display: block; margin: 0 auto;")
    })
}
