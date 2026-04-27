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

addResourcePath("images_d", "data/chcmp/images")
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
    metric_selected <- reactiveVal("pRidge")
    event_selected <- reactiveVal("DChamps")
    
    user_logged_in <- reactiveVal(rstudioapi::isAvailable())
    correct_password = "0322"
    
    load_event_data <- function(event) {
        raw(read.csv(file.path("data", event, "data.csv")))
        schedule(read.csv(file.path("data", event, "schedule.csv")))
        tba_data(read.csv(file.path("data", event, "tba_data.csv")))
        pridge(read.csv(file.path("data", event, "pridge.csv")))
        alliances(read.csv(file.path("data", event, "alliances.csv")))
    }
    load_event_data("chcmp")
    
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
        event_selected("Week 0")
    })
    
    observeEvent(input$vaale, {
        load_event_data("vaale")
        event_selected("Alexandria")
    })
    
    observeEvent(input$mdpas, {
        load_event_data("mdpas")
        event_selected("Pasadena")
    })
    
    observeEvent(input$mdbet, {
        load_event_data("mdbet")
        event_selected("Bethesda")
    })
    
    observeEvent(input$chcmp, {
        load_event_data("chcmp")
        event_selected("Dchamps")
    })
    
    observeEvent(input$pRidge, {
        metric_selected("pRidge")
    })
    
    observeEvent(input$EPA, {
        metric_selected("EPA")
    })
    
    observeEvent(input$OPR, {
        metric_selected("OPR")
    })
    
    #UPDATE MATCH TEAMS SELECTED
    observeEvent(input$selected_match, {
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
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
        summary_stat(
            summary_stats(raw(), pridge(), teams_selected(), metric_selected())
        )
    })
    
    #EVENT SUMMARY
    output$event_summary <- renderPlot({
        teams <- unique(raw()$team)
        stacked_bar_chart(raw(), schedule(), pridge(), teams, metric_selected())
    })
    
    output$event_summary_display <- renderPlot({
        teams <- unique(raw()$team)
        stacked_bar_chart(raw(), schedule(), pridge(), teams, metric_selected())
    })
    
    output$summary_stats <- renderDT({
        dataframe <- summary_stats(raw(), pridge(), metric = metric_selected())
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
        data <- summary_stats(raw(), pridge(), metric = metric_selected())
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
        req(input$selected_teams_comp)
        stacked_bar_chart(
            raw(), schedule(), pridge(), teams_selected(), metric_selected(), 
            order = FALSE, flip = FALSE)
    })
    
    #COMPARE ENDGAME BAR
    output$end_bar_comp <- renderPlot({
        req(input$selected_teams_comp)
        endgame_graph(raw(), teams_selected())
    })
    
    #COMPARE DRIVER RATING
    output$driver_rating_comp <- renderPlot({
        req(input$selected_teams_comp)
        plot_driver_rating_graph(raw(), teams_selected())
    })
    
    # COMPARE INACTIVE STRATEGY
    output$inactive_strategy_comp <- renderPlot({
        req(input$selected_teams_comp)
        inactive_stategy_summary(raw(), teams_selected())
    })
    
    # COMPARE PROBLEM TYPE
    output$problem_type_comp <- renderPlot({
        req(input$selected_teams_comp)
        problems_graph(raw(), teams_selected())
    })
    
    #COMPARE TRENCH BUMP
    output$trench_bump_comp <- renderPlot({
        req(input$selected_teams_comp)
        bump_trench_ratioplot(raw(), teams_selected())
    })
    
    # COMPARE AUTO TYPE
    output$auto_type_comp <- renderPlot({
        req(input$selected_teams_comp)
        auto_type_graph(raw(), FALSE, teams_selected())
    })
    
    output$comments_df_comp <- renderDT({
        req(input$selected_teams_comp)
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
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        data <- summary_stat()
        score_pred(data, teams_selected()[1:3], teams_selected()[4:6])
    })
    
    #SUMMARY POINT MATCH
    output$summary_point_match <- renderPlot({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        stacked_bar_chart(
            raw(), schedule(), pridge(), teams_selected(), metric_selected(), 
            order = FALSE, flip = FALSE, alliance_color = TRUE)
    })
    
    output$summary_stats_comp <- renderDT({
        req(input$selected_teams_comp)
        summary_stats(raw(), pridge(), teams_selected(), metric_selected())
    })
    
    output$end_bar_match <- renderPlot({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        endgame_graph(raw(), teams_selected(), alliance_color = TRUE)
    })
    
    output$trench_bump_match <- renderPlot({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        bump_trench_ratioplot(raw(), teams_selected(), alliance_color = TRUE)
    })
    
    output$driver_rating_match <- renderPlot({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        driver_rating_match(raw(), teams_selected())
    })
    
    output$inactive_strategy_match <- renderPlot({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        inactive_stategy_summary(raw(), teams_selected(), alliance_color = TRUE)
    })
    
    output$problem_type_match <- renderPlot({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        problems_graph(raw(), teams_selected(), TRUE)
    })
    
    output$auto_type_match <- renderPlot({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        auto_type_graph(
            raw(), teams_selected(), 
            flip = FALSE, order = FALSE, alliance_color = TRUE)
    })
    
    output$summary_stats_match <- renderDT({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
        summary_stat()
    })
    
    output$comments_df_match <- renderDT({
        req(isTruthy(input$selected_match) || 
                isTruthy(input$selected_red) || 
                isTruthy(input$selected_blue))
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
    
    output$scouter_streak <- renderPlotly({
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
    
    output$data_up_till <- renderUI({
        text <- paste("Data Up To: Match", max(raw()$match))
        HTML(text)
    })
    
    output$metric_current_selection <- renderUI({
        text <- paste("Currently Selected:", metric_selected())
        HTML(text)
    })
    
    output$event_current_selection <- renderUI({
        text <- paste("Currently Selected:", event_selected())
        HTML(text)
    })
    
    output$intro_paragraph <- renderUI({
        intro_paragraph_text()
    })
    
    output$event_summary_summary <- renderUI({
        event_summary_summary_text()
    })
    
    output$auto_picklisting_summary <- renderUI({
        auto_picklisting_summary_text()
    })
    
    output$compare_teams_summary <- renderUI({
        compare_teams_summary_text()
    })
    
    output$match_tab_summary <- renderUI({
        match_tab_summary_text()
    })
    
    output$scouts_tab_summary <- renderUI({
        scouts_tab_summary_text()
    })
    
    output$settings_summary <- renderUI({
        settings_summary_text()
    })
    
    output$pridge_summary <- renderUI({
        pridge_summary_text()
    })
    
    output$metric_swap_summary <- renderUI({
        metric_swap_summary_text()
    })
    
    output$event_swap_summary <- renderUI({
        event_swap_summary_text()
    })
    
    output$password_summary <- renderUI({
        password_summary_text()
    })
    
    output$team_logo <- renderUI({
        image_src <- "https://avatars.githubusercontent.com/u/1393583?s=280&v=4"
        tags$img(src = image_src, height = "100%px", width = "100%px")
    })
    
    output$rebuilt_logo <- renderUI({
        image_src <- paste0(
            "https://www.studica.ca/images/thumbs/0013543_first-robotics-compe",
            "tition-rebuilt-game-piece-kop-quantity_550.webp")
        tags$img(src = image_src, height = "100%px", width = "100%px")
    })
    
    output$frc_logo <- renderUI({
        image_src <- paste0(
            "https://yt3.googleusercontent.com/yLQ-DmaEu2MHV5MRVFL3Qp7A61x8qRg6X8laL8XAG6a-ZpaaEps_0WwIxjtxoyoUDL2RBral7g=s900-c-k-c0x00ffffff-no-rj")
        tags$img(src = image_src, height = "100%px", width = "100%px")
    })
}


