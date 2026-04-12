intro_paragraph_text <- function(){
    HTML(
        "This is the 449 Shinyapp for 2026 REBUILT. On this app, we are able to 
        visualize to strategize for matches, picklist, and appreciate 
        beautiful graphs :)<br><br><b>If you are reading this, and intend on 
        using our app, or even just checking it out, please fill out this 
        form linked here:</b> <a href='https://forms.gle/GLTHyD5tREEJ7tdn8' 
        target='_blank'>Form Link</a><br>It won't take longer than 
        15 seconds of your time.<br><br>Remember that everything on the 
        shinyapp is free for your benefit... Except for our password 
        locked comments database ;)<br><br>Everything was developed by 
        the 449 Data Science 'Subteam', including a brand new stat, pRidge: 
        see below for more detail.<br><br>Data is inputted by our amazing 
        scouts, with some assistance from TBA data (for pRidge). Our code is 
        available on the 449 Github: 
        <a href='https://github.com/blair-robot-project/scouting2026'
        target='_blank'>Github Link</a>")
}

event_summary_summary_text <- function(){
    HTML(
        "The Event Summary tab provides an overview of the selected event 
        (see below for how to switch events), showing a general event 
        graph and an event datatable showing interesting stats.")
}

auto_picklisting_summary_text <- function(){
    HTML(
        "The Auto-Picklisting tab (WIP) automatically grades each team and 
        sorts them into a list based off default weights for each category, 
        also shown. There is a customization option at the bottom of the 
        page, allowing you to change the weights — which is recommended 
        to prioritize your preferences.")
}

compare_teams_summary_text <- function(){
    HTML(
        "The Compare Teams tab allows you to select any team from the 
        event, and the shinyapp will generate visualizations and fill out 
        dataframes for the selected teams. All teams can be selected, 
        but we recommend no more than six be selected at a time. Our team 
        usually uses this tab during our Picklisting meetings. Once playoffs 
        start, alliances may become available to choose.")
}

match_tab_summary_text <- function(){
    HTML(
        "The Match tab can select any match from the event, and much like the 
        Compare Teams tab, the shinyapp will create graphs and tables to 
        describe the match, with scoring predictions found on the sidebar.")
}

scouts_tab_summary_text <- function(){
    HTML(
        "The Scouts tab displays all the people who have scouted for us 
        this year, their amount of matches scouted, average characters 
        commented, and their streak of scouted matches. Thanks Scouts!")
}

settings_summary_text <- function(){
    HTML(
        "The Settings tab controls the shinyapp and adds accessibilities 
        including: metric switching, event switching, and passcode entry. 
        More info on the features in the settings can be found below.")
}

pridge_summary_text <- function(){
    HTML(
        "pRidge is a debut advanced metric developed by our team, used in 
        REBUILT to estimate the amount of fuel a team scores. pRidge is 
        essentially a cross between OPR and EPA, creating a matrix and solving 
        it with bias towards a team's EPA. For proof of validity, 
        running Leave-One-Out-Cross-Validation and then computing the Mean 
        Squared Error for match score predictions of events between 2016-2025 
        results in 16.2% improvement upon OPR, and 11.7% improvement upon EPA,
        on average. A Whitepaper is in progress.
")
}

metric_swap_summary_text <- function(){
    HTML(
        "In the Settings tab, there is a section for metric swapping. 
        These metrics include OPR, EPA, pRidge, and hOPpeR. Switching 
        between metrics only requires one click and will change the 
        calculation method for estimated fuel scored per match. This 
        will also alter the graphs displaying estimated fuel count.")
}

event_swap_summary_text <- function(){
    HTML(
        "Located on the sidebar of the Settings tab, there are event 
        buttons, which when clicked, change the data between events which the 
        shinyapp displays. The only events showed are the ones that 449 has 
        participated in, plus extra test events. Once an event is chosen, all 
        the inputs will switch to the proper matches and teams, and graphs + 
        tables will be based off the chosen event.")
}

password_summary_text <- function(){
    HTML(
        "Some features are password-locked, namely the comments tables in the 
        Compare Teams tab and the Match tab. If you have the password, you 
        can go to the settings tab to login and unlock the password-locked 
        features.")
}

