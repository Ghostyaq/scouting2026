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

source("viz/helper_functions.R")
source("server.R")

data <- read.csv("data/test_data/data.csv")
pridge <- read.csv("data/test_data/pridge.csv")
tba_data <- read.csv("data/test_data/tba_data.csv")
schedule <- read.csv("data/test_data/schedule.csv")

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

#--------------------------------------------INITIALIZE-----------------------------------------------------------------

shinyApp(
    ui = ui, 
    server = server, 
    options = list(height = 1000))