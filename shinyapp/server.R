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

addResourcePath("images_d", "data/mdbet/images")
#addResourcePath("heatmaps", "../subjective_scouting/pathImages/finals")

server <- function(input, output, session) {
    raw <- reactiveVal()
    pridge <- reactiveVal()
    tba_data <- reactiveVal()
    schedule <- reactiveVal()
    alliances <- reactiveVal()
    weights <- reactiveVal(default_linear_weights)
    teams_selected <- reactiveVal(NULL)
    summary_stat <- reactiveVal(NULL)
    
    user_logged_in <- reactiveVal(rstudioapi::isAvailable())
    correct_password = "0322"
    
    load_event_data <- function(event) {
        raw(read.csv(file.path("data", event, "data.csv")))
        schedule(read.csv(file.path("data", event, "schedule.csv")))
        tba_data(read.csv(file.path("data", event, "tba_data.csv")))
        pridge(read.csv(file.path("data", event, "pridge.csv")))
        alliances(read.csv(file.path("data", event, "alliances.csv")))
    }
    load_event_data("mdbet")
    
    #UPDATE PICKERS
    observe({
        unique_teams <- sort(unique(raw()$team))
        updateVirtualSelect("selected_match", choices = schedule()$match)
        updateVirtualSelect("selected_teams_comp", choices = unique_teams)
        updateVirtualSelect("selected_red", choices = alliances()$alliance)
        updateVirtualSelect("selected_blue", choices = alliances()$alliance)
    })
    
    observeEvent(input$week0, {
        load_event_data("week0")
    })
    
    observeEvent(input$vaale, {
        load_event_data("vaale")
    })
    
    observeEvent(input$mdpas, {
        load_event_data("mdpas")
    })
    
    observeEvent(input$mdbet, {
        load_event_data("mdbet")
    })
    
    #UPDATE MATCH TEAMS SELECTED
    observeEvent(input$selected_match, {
        req(input$selected_match)
        teams <- schedule() |>
            filter(match == input$selected_match) |>
            pivot_longer(
                cols = c(R1, R2, R3, B1, B2, B3),
                names_to = "position",
                values_to = "tnum") |>
            pull(tnum)
        
        teams_selected(teams)
    })
    
    #UPDATE COMP TEAMS SELECTED
    observeEvent(input$selected_teams_comp, {
        teams_selected(input$selected_teams_comp)
    })
    
    #UPDATE ALLIANCE TEAMS SELECTED
    observeEvent(c(input$selected_red, input$selected_blue),{
        red <- alliances()[alliances()$alliance == input$selected_red,]
        blue <- alliances()[alliances()$alliance == input$selected_blue,]
        red <- c(red$C, red$FP, red$SP)
        blue <- c(blue$C, blue$FP, blue$SP)
        teams <- c(red, blue)
        
        teams_selected(teams)
    })
    
    #UPDATE SUMMARY STAT
    observeEvent(teams_selected(), {
        summary_stat(summary_stats(raw(), pridge(), teams_selected()))
    })
    
    #EVENT SUMMARY
    output$event_summary <- renderPlot({
        teams <- unique(raw()$team)
        stacked_bar_chart(raw(), schedule(), pridge(), TRUE, teams, TRUE)
    })
    
    output$event_summary_display <- renderPlot({
        teams <- unique(raw()$team)
        stacked_bar_chart(raw(), schedule(), pridge(), TRUE, teams, TRUE)
    })
    
    output$summary_stats <- renderDT({
        dataframe <- summary_stats(raw(), pridge())
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
        data <- summary_stats(raw(), pridge())
        team_scores <- calculate_team_scores(weights(), data)
        team_scores$Rank <- 1:nrow(team_scores)
        
        #reorder columns to show rank and score first
        cols_order <- c("Rank", "Team", "Team Score")
        remaining_cols <- setdiff(names(team_scores), cols_order)
        team_scores <- team_scores[, c(cols_order, remaining_cols)]
        
        #datatable
        datatable(
            team_scores, 
            options = list(
                pageLength = length(team_scores$Team),
                dom = 'ftip',
                scrollX = TRUE
            ),
            rownames = FALSE) |>
            formatStyle(
                'Team Score',
                background = styleColorBar(
                    c(0, max(team_scores$`Team Score`)), 'lightblue'),
                backgroundSize = '100% 90%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')    
    }) 
    
    #COMPARE POINT SUMMARY
    output$summary_point_comp <- renderPlot({
        stacked_bar_chart(
            raw(), schedule(), pridge(), 
            FALSE, teams_selected(), FALSE)
    })
    
    #COMPARE ENDGAME BAR
    output$end_bar_comp <- renderPlot({
        endgame_graph(raw(), teams_selected())
    })
    
    #COMPARE DRIVER RATING
    output$driver_rating_comp <- renderPlot({
        plot_driver_rating_graph(raw(), teams_selected())
    })
    
    # COMPARE INACTIVE STRATEGY
    output$inactive_strategy_comp <- renderPlot({
        inactive_stategy_summary(raw(), teams_selected(), FALSE, FALSE)
    })
    
    #COMPARE TRENCH BUMP
    output$trench_bump_comp <- renderPlot({
        bump_trench_ratioplot(raw(), teams_selected())
    })
    
    # COMPARE AUTO TYPE
    output$auto_type_comp <- renderPlot({
        auto_type_graph(raw(), FALSE, teams_selected(), FALSE)
    })
    
    output$comments_df_comp <- renderDT({
        if (user_logged_in()){
            df <- comments_df(raw(), teams_selected())
        } else {
            df <- data.frame(
                Message ="Please Login in the Settings Tab to access comments!"
            )
        }
        
        datatable(
            df,
            options = list(
                dom = 't', 
                pageLength = nrow(df)
            )
        )
    })
    
    #SCORE PREDICTION
    output$score_prediction <- renderText({
        data <- summary_stat()
        score_pred(data, teams_selected()[1:3], teams_selected()[4:6])
    })
    
    #SUMMARY POINT MATCH
    output$summary_point_match <- renderPlot({
        stacked_bar_chart(
            raw(), schedule(), pridge(), FALSE, teams_selected(), FALSE)
    })
    
    output$summary_stats_comp <- renderDT({
        summary_stats(raw(), pridge(), teams = teams_selected())
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
        endgame_graph(raw(), teams_selected())
    })
    
    output$trench_bump_match <- renderPlot({
        bump_trench_ratioplot(raw(), teams_selected())
    })
    
    output$driver_rating_match <- renderPlot({
        plot_driver_rating_graph(raw(), teams_selected())
    })
    
    output$inactive_strategy_match <- renderPlot({
        inactive_stategy_summary(raw(), teams_selected(), FALSE, FALSE)
    })
    
    output$auto_type_match <- renderPlot({
        auto_type_graph(raw(), FALSE, teams_selected(), FALSE)
    })
    
    output$summary_stats_match <- renderDT({
        summary_stat()
    })
    
    output$comments_df_match <- renderDT({
        if (user_logged_in()){
            df <- comments_df(raw(), teams_selected())
        } else {
            df <- data.frame(
                Message ="Please Login in the Settings Tab to access comments!"
            )
        }
        
        datatable(
            df,
            options = list(
                dom = 't', 
                pageLength = nrow(df)
            )
        )
    })
    output$matches_scouted <- renderPlotly({
        plot_scouting_graph(raw())
    })
    
    output$scout_yaps <- renderPlotly({
        yap_graph(raw())
    })
    
    output$scouter_streak <- renderPlot({
        high_streak(raw())
    })
    
    output$images_comp <- renderUI({
        tags <- lapply(teams_selected(), function(teamnum) {
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
        tags_m <- lapply(teams_selected(), function(team) {
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
        tags <- lapply(teams_selected(), function(teamnum) {
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
        tags_m <- lapply(teams_selected(), function(team) {
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
    
    output$match_history <- renderDT({
        matches_hist <- raw()|>
            filter(team %in% teams_selected())|>
            select(-scout, -comments)
        datatable(
            matches_hist,
            options = list(
                dom = "t",
                pageLength = nrow(matches_hist),
                height = 1000
            )
        )
    })
    
    output$intro_paragraph <- renderUI({
        HTML("This is the 449 Shinyapp for 2026 REBUILT. On this app we visualize 
              data in order to strategize for matches, picklist, and appreciate
              beautiful data :).<br><br><b>If you are reading this, and intend on 
              using our app, or even just checking it out, please fill out this 
              form linked here:</b><br>Form Link<br>It won't take longer than a 
              minute of your time.<br><br>Remember that everything on this 
              shinyapp is free for your benefit... Except for our password 
              locked comments database ;).<br><br>Everything was developed by
              the 449 data science subteam, including a brand new stat, PRidge,
              see below for more detail.<br><br>Data is inputted with our amazing
              scouts, and TBA data to get actual scores for pRidge")
    })
    
    output$event_summary_summary <- renderText({
        paste("The event summary tab provides an overview of the selected event 
              (see settings for how to switch events), showing a general event 
              graph and a event datatable showing average event stats.")
    })
    
    output$auto_picklisting_summary <- renderText({
        paste("The auto picklisting tab automatically grades each team and sorts
              them into a list based off default weights for each category,
              also shown. There is a customization option at the bottom of the
              page which allows you to change the weights, which is recommended
              to prioritize your preferences.")
    })
    
    output$compare_teams_summary <- renderText({
        paste("The compare teams tab allows you to select any team from the
              event, and the shinyapp will generate visualizations and fill out
              dataframes for the selected teams, as many teams can be selected,
              but more than 6 is not recommended. The tab includes: Trench vs.
              Bump, robot images, auto heatmaps, auto tendencies, driver rating
              trends, and overall summaries as graphs and tables. Once playoffs
              start, alliances become available to compare.")
    })
    
    output$match_tab_summary <- renderText({
        paste("The match tab can select any match from the selected event, and 
              much like the compare teams tab, the shinyapp will create graphs
              and tables to describe the match, with all the same graphs as
              compare teams, and scoring predictions found on the sidebar.")
    })
    
    output$scouts_tab_summary <- renderText({
        paste("The scouts tab displays all the people who have scouted for us
              this year, and their amount of matches scouted, average characters
              commented, and their streak of scouted matches. Thanks scouts!")
    })
    
    output$settings_summary <- renderText({
        paste("The settings control the shinyapp and add accesibilities,
              including metric switching, event switching and passcode entry.
              More info on the features in the settings can be found below.")
    })
    
    output$pridge_summary <- renderText({
        paste("PRigde is a debut advanced metric developed by team 449, used
              this year in REBUILT to estimate the amount of fuel a team 
              scores. It is especially important this year where a scout cannot
              quantitatively count fuel real time, and pRidge very accurately 
              does this. PRidge is essentially a cross between OPR and EPA.
              PRidge creates a matrix and solves accurately after about 20 
              matches, very similar to OPR, but biased toward EPA
              rather than 0. A link to a team 449 whitepaper for PRidge can be
              found here: Link here")
    })
    
    output$metric_swap_summary <- renderText({
        paste("In the settings tab, there is a section for metric swapping.
              These metrics include OPR, EPA, pRidge, and hOPpeR. Switching 
              between metrics only requires one click and will change the 
              calculation method for estimated fuel scored per match.")
    })
    
    output$event_swap_summary <- renderText({
        paste("Located on the sidebar of the settings tab, there are event 
              buttons, which when clicked, change the data that the shinyapp
              uses to visualize and show. The only events showed are the ones
              that 449 has participated in, plus extra test events. Once an 
              event is chosen, all the inputs will switch to the proper matches 
              and teams, and graphs and tables will be based off the chosen 
              event.")
    })
    
    output$password_summary <- renderText({
        paste("Some features are password locked, namely the comments tables
              in the compare teams tab and the matches tab. If you have the 
              password, you can go to the settings tab, input it, unlock the 
              password blocked features.")
    })
    
    output$team_logo <- renderUI({
        image_src <- "https://avatars.githubusercontent.com/u/1393583?s=280&v=4"
        tags$img(src = image_src, height = "100%px", width = "100%px")
    })
    
    output$rebuilt_logo <- renderUI({
        image_src <- "
Zoom https---www.studica.ca-images-thumbs-0013543_first-robotics-competition-rebuilt-game-piece-kop-quantity_550.webp
https://www.studica.ca/images/thumbs/0013543_first-robotics-competition-rebuilt-game-piece-kop-quantity_550.webp
"
        tags$img(src = image_src, height = "100%px", width = "100%px")
    })
    
    output$frc_logo <- renderUI({
        image_src <- "
Zoom https---www.nicepng.com-png-full-44-442571_first-robotics-logo-first-robotics-logo-png.png
https://www.nicepng.com/png/full/44-442571_first-robotics-logo-first-robotics-logo-png.png
"
        tags$img(src = image_src, height = "100%px", width = "100%px")
    })
}