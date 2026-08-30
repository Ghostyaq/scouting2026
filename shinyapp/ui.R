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
library(shiny.pwa)

source("viz/helper_functions.R")
source("viz/introduction_page_text.R")
source("server.R")

options(sass.cache = FALSE)
addResourcePath("assets", "www")

ui <- navbarPage(
    title = "2026 REBUILT 449 Shinyapp",
    theme = bs_theme(
        version = 5,
        preset = "flatly"
    ),
    collapsible = TRUE,
    header = tagList(
        tags$link(rel = "stylesheet", type = "text/css", href = "assets/styles.css"),
        tags$head(tags$script(src = "assets/script.js", type = "text/javascript")),
    ),
    tabPanel(#--------------------------INTRODUCTION------------------------
             title = "Introductory Page",
             card(
                 card_header(h3(
                     "2026 REBUILT 449 Shinyapp", 
                     align = "center",
                     style = "color: #a7000a; font-weight: bold;"))
             ),
             div(class = "row",
                 div(class = "col-lg-3",
                     card(uiOutput("frc_logo"))
                 ),
                 div(class = "col-lg-9",
                     card(
                         card_header("Introduction"),
                         htmlOutput("intro_paragraph")
                     )
                 )
             ),
             card(),
             div(class = "row",
                 div(class = "col-12 col-lg-4",
                     card(
                         class = "graph-card",
                         card_header("Explore Data"),
                         plotOutput("event_summary_display")
                     )
                 ),
                 div(class = "col-6 col-lg-4",
                     card(
                         card_header("Presented by FRC Team 449"),
                         uiOutput("team_logo")
                     )
                 ),
                 div(class = "col-6 col-lg-4",
                     card(
                         card_header("All data on FRC REBUILT"),
                         uiOutput("rebuilt_logo")
                     )
                 )
             ),
             card(),
             card(
                 card_header(h3(
                     "Description of Tabs", 
                     align = "center",
                     style = "color: #a7000a; font-weight: bold;"))
             ),
             div(class = "row",
                 div(class = "col-lg-6",
                     card(
                         card_header("Event Summary"),
                         htmlOutput("event_summary_summary")
                     )
                 ),
                 div(class = "col-lg-6",
                     card(
                         card_header("Compare Teams"),
                         htmlOutput("compare_teams_summary")
                     )
                 )
             ),
             div(class = "row",
                 div(class = "col-lg-4",
                     card(
                         card_header("Match"),
                         htmlOutput("match_tab_summary")
                     )
                 ),
                 div(class = "col-lg-4",
                     card(
                         card_header("Scouts"),
                         htmlOutput("scouts_tab_summary")
                     )
                 ),
                 div(class = "col-lg-4",
                     card(
                         card_header("Settings"),
                         htmlOutput("settings_summary")
                     )
                 )
             ),
             card(),
             card(
                 card_header(h3(
                     "Description of Features", 
                     align = "center",
                     style = "color: #a7000a; font-weight: bold;"))
             ),
             div(class = "row",
                 div(class = "col-lg-6",
                     card(
                         card_header("pRidge"),
                         htmlOutput("pridge_summary")
                     )
                 ),
                 div(class = "col-lg-6",
                     card(
                         card_header("Metric Switcher"),
                         htmlOutput("metric_swap_summary")
                     )
                 )
             ),
             div(class = "row",
                 div(class = "col-lg-6",
                     card(
                         card_header("Event Switcher"),
                         htmlOutput("event_swap_summary")
                     )
                 ),
                 div(class = "col-lg-6",
                     card(
                         card_header("Password-Locked Features"),
                         htmlOutput("password_summary")
                     )
                 )
             )
    ),
    tabPanel(#-----------------------EVENT SUMMARY--------------------------
             title = "Event Summary",
             card(
                 class = "graph-card",
                 card_header("Event Summary"),
                 plotOutput("event_summary", height = "600px")
             ),
             card(
                 card_header("Event Summary Stats"),
                 fill = FALSE,
                 card_body(
                     fillable = FALSE,
                     DTOutput("summary_stats")
                 )
             )
    ),
    tabPanel(#------------------------COMPARE TEAMS-------------------------
             title = "Compare Teams",
             div(class = "container-fluid",
                 div(class = "row",
                     div(class = "col-12 col-lg-3",
                         div(
                             style = "background-color: #f8f9fa; padding: 15px; 
                            border-radius: 5px; min-height: 100%;",
                             virtualSelectInput(
                                 "selected_teams_comp", 
                                 label = "Select Teams", 
                                 choices = NULL, multiple = TRUE, search = TRUE
                             )
                         )
                     ),
                     div(class = "col-12 col-lg-9",
                         div(class = "row",
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Summary Fuel Points"),
                                     plotOutput("summary_point_comp") |> 
                                         withSpinner()
                                 )
                             ),
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Trench Bump Ratioplot"),
                                     plotOutput("trench_bump_comp") |> 
                                         withSpinner()
                                 )
                             )
                         ),
                         div(class = "row",
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Driver Rating by Match"),
                                     plotOutput("driver_rating_comp") |> 
                                         withSpinner()
                                 )
                             ),
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Auto Type"),
                                     plotOutput("auto_type_comp") |> 
                                         withSpinner()
                                 )
                             )
                         ),
                         div(class = "row",
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Inactive Strategy Summary"),
                                     plotOutput("inactive_strategy_comp")|>withSpinner()
                                 )
                             ),
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Problems Encountered"),
                                     plotOutput("problem_type_comp")|>withSpinner()
                                 )
                             )
                         ),
                         card(
                             card_header("Robot Images"),
                             uiOutput("images_comp") |> withSpinner()
                         ),
                         card(
                             card_header("Stats"),
                             fill = FALSE,
                             card_body(
                                 fillable = FALSE,
                                 DTOutput("summary_stats_comp") |> withSpinner()
                             )
                         ),
                         card(
                             card_header("Match History"),
                             fill = FALSE,
                             card_body(
                                 fillable = FALSE,
                                 DTOutput("match_history") |> withSpinner()
                             )
                         ),
                         card(
                             card_header("Comments"),
                             fill = FALSE,
                             card_body(
                                 fillable = FALSE,
                                 DTOutput("comments_df_comp") |> withSpinner()
                             )
                         )
                     )
                 )
             )
    ),
    tabPanel(#---------------------------MATCH------------------------------
             title = "Match",
             div(class = "container-fluid",
                 div(class = "row",
                     div(class = "col-12 col-lg-3",
                         div(
                             style = "background-color: #f8f9fa; padding: 15px; 
                            border-radius: 5px; min-height: 100%;",
                             virtualSelectInput(
                                 "selected_match", 
                                 label = "Select a Match", 
                                 choices = NULL, selected = 1, search = TRUE),
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
                             uiOutput("score_prediction")
                         )
                     ),
                     div(class = "col-12 col-lg-9",
                         div(class = "row",
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Summary Fuel Points"),
                                     plotOutput("summary_point_match") |> 
                                         withSpinner()
                                 )
                             ),
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Trench Bump Ratioplot"),
                                     plotOutput("trench_bump_match") |> 
                                         withSpinner()
                                 )
                             )
                         ),
                         div(class = "row",
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Driver Rating by Match"),
                                     plotOutput("driver_rating_match") |> 
                                         withSpinner()
                                 )
                             ),
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Auto Type"),
                                     plotOutput("auto_type_match") |> 
                                         withSpinner()
                                 )
                             )
                         ),
                         div(class = "row",
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Inactive Strategy Summary"),
                                     plotOutput("inactive_strategy_match") |> 
                                         withSpinner()
                                 )
                             ),
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Problems Encountered"),
                                     plotOutput("problem_type_match") |> 
                                         withSpinner()
                                 )
                             )
                         ),
                         card(
                             card_header("Robot Images in Match"),
                             uiOutput("images_match") |> withSpinner()
                         ),
                         card(
                             card_header("Stats"),
                             fill = FALSE,
                             card_body(
                                 fillable = FALSE,
                                 DTOutput("summary_stats_match") |> 
                                     withSpinner()
                             )
                         ),
                         card(
                             card_header("Comments"),
                             fill = FALSE,
                             card_body(
                                 fillable = FALSE,
                                 DTOutput("comments_df_match") |> withSpinner()
                             )
                         )
                     )
                 )
             )
    ),
    tabPanel(#-----------------------QUALITATIVE----------------------------
             title = "Qualitative",
             div(class = "container-fluid",
                 div(class = "row",
                     div(class = "col-12 col-lg-3",
                         div(
                             style = "background-color: #f8f9fa; padding: 15px; 
                                    border-radius: 5px; min-height: 100%;",
                             virtualSelectInput(
                                 "selected_teams_qual", 
                                 label = "Select Teams (max 3)", 
                                 choices = NULL, multiple = TRUE, search = TRUE
                             )
                         )
                     ),
                     div(class = "col-12 col-lg-9",
                         div(class = "row",
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Radar Chart"),
                                     plotOutput("qual_radar_chart")
                                 )
                             ),
                             div(class = "col-lg-6",
                                 card(
                                     class = "graph-card",
                                     card_header("Images"),
                                     uiOutput("images_qual")
                                 )
                             )
                         ),
                         uiOutput("qual_comments_ui")
                     )
                 )
             )
    ),
    tabPanel(#----------------------------SCOUTS----------------------------
             title = "Scouts",
             card(
                 class = "graph-card",
                 card_header("Total Matches Scouted by Scout"),
                 plotlyOutput("matches_scouted")
             ),
             card(
                 class = "graph-card",
                 card_header("Average Yaps by Scout"),
                 plotlyOutput("scout_yaps")
             ),
             card(
                 class = "graph-card",
                 card_header("Scout Yap Streak"),
                 plotlyOutput("scouter_streak")
             )
    ),
    tabPanel(#-------------------------SETTINGS-----------------------------
             title = "Settings",
             div(class = "row",
                 div(class = "col-lg-6",
                     card(
                         card_header("Metric Selection"),
                         uiOutput("data_up_till"),
                         uiOutput("metric_current_selection"),
                         actionBttn(
                             inputId = "pRidge",
                             label = "pRidge",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "EPA",
                             label = "EPA",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "OPR",
                             label = "OPR",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         )
                     )
                 ),
                 div(class = "col-lg-6",
                     card(
                         card_header("Chesapeake Data Switching"),
                         actionBttn(
                             inputId = "week0",
                             label = "Week 0 (Test)",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "vaale",
                             label = "Alexandria",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "mdpas",
                             label = "Pasadena (Only pRidge)",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "mdbet",
                             label = "Bethesda",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "chcmp",
                             label = "DChamps",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "all_data",
                             label = "All Data",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                     )
                 )
             ),
             div(class = "row",
                 div(class = "col-lg-6",
                     card(
                         card_header("Worlds Data Switching"),
                         uiOutput("event_current_selection"),
                         actionBttn(
                             inputId = "arc",
                             label = "Archimedes",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "cur",
                             label = "Curie",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "dal",
                             label = "Daly",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "gal",
                             label = "Galileo",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "hop",
                             label = "Hopper",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "joh",
                             label = "Johnson",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "mil",
                             label = "Milstein",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         ),
                         actionBttn(
                             inputId = "new",
                             label = "Newton",
                             style = "unite",
                             color = "success",
                             size = "md",
                             block = TRUE
                         )
                     )
                 ),
                 div(class = "col-lg-6",
                     card(
                         card_header("Scout Comments Login"),
                         uiOutput("login_ui"),
                         uiOutput("login_status")
                     )
                 )
             )
    )
)

shinyApp(
    ui = ui, 
    server = server)