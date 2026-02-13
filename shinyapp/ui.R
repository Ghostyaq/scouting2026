library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
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

scheduled_matches = tibble(matches = 1:78)
played_matches = tibble(matches = 1:40)

#--------------------------------------------FUNCTION-------------------------------------------------------------------

boxplot = function(curr_match, selected_match) {
    data = tibble(x = 0, y = 1)
    is_played = ggplot(data = NULL, aes(x = x, y = y)) +
        geom_point()
    is_played = if(curr_match > selected_match) {
        ggplot(data = data, aes(x = x, y = y)) +
            geom_point()
    }
    
    return(is_played)
}

#----------------------------------------------UI-----------------------------------------------------------------------


ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    navbarPage(
        title = "2026 REBUILT 449 Data",
        tabPanel(
            title = "Event Summary",
            card(
                card_header("Event Summary"),
                plotlyOutput("event_summary")
            )
        ),
        tabPanel(
            title = "Single Team",
            layout_sidebar(
                sidebar = card(
                    virtualSelectInput("selected_team", label = "Select a Team", choices = NULL, search = TRUE),
                    height = "500px"
                )
            )
        ),
        tabPanel(
            title = "Compare Teams",
            layout_sidebar(
                sidebar = card(
                    virtualSelectInput("selected_teams_compare", label = "Select Teams", choices = NULL, multiple = TRUE, search = TRUE)
                )
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
                )
            )
        ),
        tabPanel(
            title = "Scouts",
            card(
                plotlyOutput("Average Yaps"),
                plotlyOutput("Total Matches Scouted")
            )
        )
    )
)

#-------------------------------------------SERVER----------------------------------------------------------------------

server <- function(input, output, session) {
    observe({
        updateVirtualSelect("selected_match", choices = scheduled_matches$matches)
    })
    
    output$boxplot_match <- renderPlot({
        curr_match = max(played_matches$matches)
        selected_match = input$selected_match
        
        boxplot(curr_match, selected_match)
    })
    
}

#--------------------------------------------INITIALIZE-----------------------------------------------------------------

shinyApp(ui = ui, server = server)