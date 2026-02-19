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

data <- read.csv("data/test_data/data.csv")
pridge <- read.csv("data/test_data/pridge.csv")
tba_data <- read.csv("data/test_data/tba_data.csv")
schedule <- read.csv("data/test_data/schedule.csv")

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
