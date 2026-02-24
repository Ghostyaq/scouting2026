
yap_graph <- function(raw) {
    spliting <- strsplit(raw$comments, split = " ")
    
    raw$number_of_yaps <- sapply(spliting, length)
    
    scout_comments <- raw |>
        group_by(scout) |>
        summarize(
            mean_yaps = round(mean(number_of_yaps), digits = 2),
            count = n()
        ) |>
        filter(count > 10) |>
        mutate(
            scout_name = reorder(scout, mean_yaps, decreasing = TRUE)
        )
    
    
    plot <- ggplot(scout_comments, aes(x = scout_name, y = mean_yaps)) +
        geom_bar(stat = "identity", position = position_dodge(), 
                 fill = "rosybrown1", colour = "black") +
        labs(title = "Comments Summary: Mean Yappage per Scout", 
             x = "Scouts", y = "Mean yappage") +
        theme_bw()
    
    ggplotly(plot)
    
}

raw <- read.csv("shinyapp/data_files/all_data/data.csv")
yap_graph(raw)