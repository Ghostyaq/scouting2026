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

source("viz/helper_functions.R")
source("server.R")

options(sass.cache = FALSE)

ui <- fluidPage(
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly"
    ),
    navbarPage(
        title = "2026 REBUILT 449 Data",
        tabPanel(
            title = "Introductory Page",
            layout_columns(
                card(
                    card_header("Introduction"),
                    textOutput("intro_paragraph")
                )
            ),
            layout_columns(
                card(
                    card_header("Explore Data"),
                    plotOutput("event_summary_display")
                ),
                card(
                    card_header("Presented by FRC Team 449"),
                    uiOutput("team_logo2")
                ),
            ),
            card(),
            layout_columns(
                card(
                    card_header("Event Summary"),
                    textOutput("event_summary_summary")
                ),
                card(
                    card_header("Auto Picklisting"),
                    textOutput("auto_picklisting_summary")
                ),
                card(
                    card_header("Compare Teams"),
                    textOutput("compare_teams_summary")
                )
            ),
            layout_columns(
                card(
                    card_header("Match"),
                    textOutput("match_tab_summary")
                ),
                card(
                    card_header("Scouts"),
                    textOutput("scouts_tab_summary")
                ),
                card(
                    card_header("Settings"),
                    textOutput("settings_summary")
                )
            )
        ),
        tabPanel(
            title = "Event Summary",
            card(
                card_header("Event Summary"),
                plotOutput("event_summary")
            ),
            card(
                card_header("Summary Stats"),
                DTOutput("summary_stats"),
                height = 1100
            )
        ),
        tabPanel(
            title = "Auto-Picklisting (WIP)",
            card(
                card_header("Auto Picklisting"),
                DTOutput("auto_picklist"),
                height = 1100
            ),
            actionButton(
                "open_weights", "Adjust Weights", class = "btn btn-primary")
        ),
        tabPanel(
            title = "Compare Teams",
            layout_sidebar(
                sidebar = card(
                    virtualSelectInput(
                        "selected_teams_comp", 
                        label = "Select Teams", 
                        choices = NULL, multiple = TRUE, search = TRUE
                    ),
                    virtualSelectInput(
                        "selected_red", 
                        label = "Select Red Alliance", 
                        choices = NULL, multiple = FALSE, search = TRUE
                    ),
                    virtualSelectInput(
                        "selected_blue", 
                        label = "Select Blue Alliance", 
                        choices = NULL, multiple = FALSE, search = TRUE
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
                        card_header("Trench Bump Relationship Ratioplot"),
                        plotOutput("trench_bump_comp")
                    ),
                    card(
                        card_header("Driver Rating by Match"),
                        plotOutput("driver_rating_comp")
                    )
                ),
                layout_columns(
                    card(
                        card_header("Inactive Strategy Summary"),
                        plotOutput("inactive_strategy_comp")
                    ),
                    card(
                        card_header("Auto Type"),
                        plotOutput("auto_type_comp")
                    )
                ),
                card(
                    card_header("Robot Images"),
                    uiOutput("images_comp")
                ),
                #card(
                #    card_header("Auto Heatmaps"),
                #    uiOutput("auto_heatmap_comp")
                #),
                card(
                    card_header("Summary Stats"), 
                    DTOutput("summary_stats_comp")
                ),
                card(
                    card_header("Match History"),
                    DTOutput("match_history")
                ),
                card(
                    card_header("Comments Data"),
                    DTOutput("comments_df_comp")
                )
            )
        ),
        tabPanel(
            title = "Match",
            layout_sidebar(
                sidebar = card(
                    virtualSelectInput(
                        "selected_match", 
                        label = "Select a Match", 
                        choices = NULL, selected = 1, search = TRUE),
                    uiOutput("score_prediction"),
                    height = "500px"
                ),
                layout_columns(
                    card(
                        card_header("Summary Fuel Points"),
                        plotOutput("summary_point_match")
                    ),
                    card(
                        card_header("Endgame Stacked Bar Chart"),
                        plotOutput("end_bar_match")
                    )
                ),
                layout_columns(
                    card(
                        card_header("Trench Bump Relationship Ratioplot"),
                        plotOutput("trench_bump_match")
                    ),
                    card(
                        card_header("Driver Rating by Match"),
                        plotOutput("driver_rating_match")
                    )
                ),
                layout_columns(
                    card(
                        card_header("Inactive Strategy Summary"),
                        plotOutput("inactive_strategy_match")
                    ),
                    card(
                        card_header("Auto Type"),
                        plotOutput("auto_type_match")
                    ),
                ),
                card(
                    card_header("Robot Images in Match"),
                    uiOutput("images_match")
                ),
                #card(
                #    card_header("Auto Heatmaps"),
                #    uiOutput("auto_heatmap_match")
                #),
                card(
                    card_header("Summary Stats"),
                    DTOutput("summary_stats_match")
                ),
                card(
                    card_header("Comments Data"),
                    DTOutput("comments_df_match")
                )
            )
        ),
        tabPanel(
            title = "Scouts",
            card(
                card_header("Total Matches Scouted by Scout"),
                plotlyOutput("matches_scouted")
            ),
            card(
                card_header("Average Yaps by Scout"),
                plotlyOutput("scout_yaps")
            ),
            card(
                card_header("Scout Yap Streak"),
                plotOutput("scouter_streak")
            )
        ),
        tabPanel(
            title = "Settings",
            layout_sidebar(
                sidebar = card(
                    title = "Event Data Switching",
                    actionButton("week0", "Week0 (Test)"),
                    actionButton("vaale", "Alexandria"),
                    actionButton("mdpas", "Pasadena (Only pRidge)"),
                    actionButton("mdbet", "Bethesda"),
                ),
                layout_columns(
                    #card(
                    #    card_header("Refresh Data"),
                    #    actionButton("pridge_button", "Reload Data")
                    #),
                    card(
                        card_header("Scout Comments Login"),
                        uiOutput("login_ui"),
                        uiOutput("login_status")
                    )
                    #card(
                    #    card_header("Custom Theme Color"),
                    #    ColorPicker("theme_color")
                    #)
                )
            )
        )
    )
)

shinyApp(
    ui = ui, 
    server = server, 
    options = list(height = 1000))