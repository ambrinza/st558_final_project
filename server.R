library(shiny)
library(caret)
library(tidyverse)

df <- read.csv('data/Austin_Animal_Center_Outcomes.csv')

# Cleaning the data before it will be used in the shiny app

df <- df %>% 
  mutate(Name = gsub("\\*", "",Name), 
         Outcome.Type = ifelse(Outcome.Type == "Rto-Adopt", "Return to Owner", Outcome.Type),
         Age = ifelse(grepl("month",Age.upon.Outcome), 0,as.numeric(sub(" .*", "", Age.upon.Outcome)))) %>%
  separate(MonthYear, c("Month", "Year"))  %>%  filter(Name != "", Age >= 0, Outcome.Type != "",
                                                       Sex.upon.Outcome != "NULL")

shinyServer(function(input, output) {
  # Create bar plot based on selection
  output$bar_plot <- renderPlot({
    # First, we want to filter to show only the n most popular for each group
    df_counts <- df %>%
          count(get(input$graph_options)) %>%
            arrange(desc(n))
    restricted_list <- df_counts[1:input$category_count,][1]
    df_filtered <- df %>% filter(get(input$graph_options) %in% unlist(restricted_list))
    if(input$grouping_options == "None"){
      # Create  plain plot
      g <- ggplot(df_filtered, aes_string(x = input$graph_options))
      g + geom_bar()
    } else{
      # Create plot with grouping
      # First need to filter the grouping var in the same way as above
      df_counts <- df %>%
        count(get(input$grouping_options)) %>%
        arrange(desc(n))
      restricted_list <- df_counts[1:input$category_count,][1]
      df_filtered_group <- df_filtered %>% filter(get(input$grouping_options) %in% unlist(restricted_list))
      g <- ggplot(df_filtered_group, aes_string(x = input$graph_options))
      g + geom_bar(aes_string(fill = input$grouping_options), position = "dodge")
    }
  })
  
  output$histogram <- renderPlot({
    g <- ggplot(df, aes(x = Age))
    g + geom_histogram(binwidth = 1)
  })
    
    
  output$cont_table <- renderTable({
    if(input$cont_table == 'One-Way'){
      # Selecting the relevant column for a one-way contingency table
      table(df %>% select(sym(input$one_way)))
    } else {
      first_col <-  df %>% select(sym(input$two_way_1))
      second_col <- df %>% select(sym(input$two_way_2))
      # table(first_col, second_col)
      with(df, table(get(input$two_way_1),get(input$two_way_2)))
    }
  })
  output$numeric_sum <- renderDataTable({
    if(input$numeric_sum == 'Overall'){
      df_numeric_sum <- df %>% summarise(mean = round(mean(Age), 2), median = median(Age), max = max(Age))
    } else {
      df_numeric_sum <- df %>% 
        group_by(get(input$numeric_sum))%>%
        summarise(mean = mean(Age), median = median(Age), max = max(Age))
    }
    
  })
    
  
})