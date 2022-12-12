library(shiny)
library(caret)
library(tidyverse)
library(gbm)

df <- read.csv('data/Austin_Animal_Center_Outcomes.csv')

# Cleaning the data before it will be used in the shiny app for EDA

df <- df %>% separate(MonthYear, c("Month", "Year")) %>%
  mutate(Name = gsub("\\*", "",Name), 
         Outcome.Type = ifelse(Outcome.Type == "Rto-Adopt", "Return to Owner", Outcome.Type),
         Age = ifelse(grepl("month",Age.upon.Outcome), 0,as.numeric(sub(" .*", "", Age.upon.Outcome))),
         Year = as.numeric(Year), Month = match(Month, month.abb)) %>%
      filter(Name != "", Age >= 0, Outcome.Type != "", Sex.upon.Outcome != "NULL")

# Cleaning the data for modelling
df_models <- df %>% 
  filter(Outcome.Type %in% c("Adoption", "Transfer")) %>%
  mutate(Is.Adopted = ifelse(Outcome.Type == "Adoption", 1, 0))

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
  
  # Only run model code when the button is clicked
  model_fits <- eventReactive(input$model_run, {
    # Finding the index for the training split based on user input
    train_index <- createDataPartition(df_models$Is.Adopted, times = 1, p = input$train_prop/100,
                                       list = FALSE)
    # Splitting the data
    df_train <- df_models[train_index,]
    df_test  <- df_models[-train_index,]
    df_train_select <- cbind(df_train[,input$predictors], df_train$Is.Adopted) %>%
      rename(Is.Adopted = `df_train$Is.Adopted`) %>%
      mutate(Is.Adopted = as.factor(Is.Adopted))
    df_test_select <- cbind(df_test[,input$predictors], df_test$Is.Adopted) %>%
      rename(Is.Adopted = `df_test$Is.Adopted`) %>%
      mutate(Is.Adopted = as.factor(Is.Adopted))
    # Fitting the relevant model
    withProgress(message = "Fitting models",{
      fit_lr <- train(Is.Adopted ~ ., data = df_train_select,
                   method = "glm", family = "binomial",
                   trControl = trainControl(method = "cv", number = 5),
                   preProcess = c("center", "scale"))
      # Increment the progress bar, and update the detail text.
      incProgress(1/3, detail = "Finished training Logistic Regression")
      fit_rf <- train(Is.Adopted ~ ., data = df_train_select, method
                   = "rf", trControl = trainControl(method = "cv", number = 5),
                   preProcess = c("center", "scale"))
      incProgress(1/2, detail = "Finished training Random Forest")
      fit_bt <- train(Is.Adopted ~ ., data = df_train_select, method
                   = "gbm", trControl = trainControl(method = "cv", number = 5),
                   preProcess = c("center", "scale"), verbose = FALSE)
    })
    list(fit_lr = fit_lr, fit_rf = fit_rf, fit_bt = fit_bt, df_test_select = df_test_select)
  })
  
  output$fit_statistics <- renderTable({
    .tmp <- model_fits()
    fit_lr <- .tmp$fit_lr
    fit_rf <- .tmp$fit_rf
    fit_bt <- .tmp$fit_bt
    fit_lr_res <- cbind(method = "Logistic Regression",fit_lr$results)
    fit_rf_res <- cbind(method ="Random Forest", fit_rf$results %>% slice_max(Accuracy, n=1) %>%
                            rename(parameter = mtry))
    fit_bt_res <- cbind(method = "Boosting", fit_bt$results %>% slice_max(Accuracy, n=1) %>%
                            select(n.trees,Accuracy, Kappa, AccuracySD, KappaSD) %>%
                            rename(parameter = n.trees))
    rbind(fit_lr_res, fit_rf_res, fit_bt_res)
  })

  output$var_imp_rf <- renderPlot({
    .tmp <- model_fits()
    fit_rf <- .tmp$fit_rf
    plot(varImp(fit_rf))
  })
  output$var_imp_bt <- renderPlot({
    .tmp <- model_fits()
    fit_bt <- .tmp$fit_bt
    plot(varImp(fit_bt))
  })

  output$fit_statistics_test <- renderTable({
    .tmp <- model_fits()
    fit_lr <- .tmp$fit_lr
    fit_rf <- .tmp$fit_rf
    fit_bt <- .tmp$fit_bt
    df_test_select <- .tmp$df_test_select
    cm_lr <-  confusionMatrix(data = df_test_select$Is.Adopted, 
                              reference = predict(fit_lr, newdata = df_test_select))
    cm_rf <-  confusionMatrix(data = df_test_select$Is.Adopted,
                              reference = predict(fit_rf, newdata = df_test_select))
    cm_bt <-  confusionMatrix(data = df_test_select$Is.Adopted, 
                              reference = predict(fit_bt, newdata = df_test_select))
    rbind(cbind(method = "Logistic Regression", t(round(cm_lr$byClass,2))),
          cbind(method = "Random Forest", t(round(cm_rf$byClass,2))),
          cbind(method = "Boosting Trees", t(round(cm_bt$byClass,2))))
  })
  
  output$prediction <- renderTable({
    .tmp <- model_fits()
    fit_lr <- .tmp$fit_lr
    fit_rf <- .tmp$fit_rf
    fit_bt <- .tmp$fit_bt
    pred_vals <- data.frame("Sex.upon.Outcome" = input$sex_val,
                            "Age" = input$age_value,
                            "Year" = input$year_val, 
                            "Month" = input$month_val, 
                            "Animal.Type" = input$animal_type)
    pred_vals <- pred_vals[,input$predictors]
    pred_lr <- predict(fit_lr,newdata = pred_vals)
    pred_rf <- predict(fit_rf, newdata = pred_vals)
    pred_bt <- predict(fit_bt, newdata = pred_vals)
    rbind(cbind(method = "Logistic Regression", prediction = ifelse(pred_lr ==1,"Transferred","Adopted")),
          cbind(method = "Random Forest", prediction = pred_rf),
          cbind(method = "Boosting Trees", prediction = ifelse(pred_bt ==1,"Transferred","Adopted")))
  })
  
  output$sidebar_output <- renderUI({
    sidebarPanel(                          
      h4("Select How To Subset Data"),
      checkboxGroupInput(inputId = 'column_select',
                  "Select Columns to Subset",
                  choices = colnames(df)),
      sliderInput("row_select", "Select number of rows to view", min = 0,
                  max = nrow(df), value = 50),
      downloadButton("download_data", "Download")
    )
  })
  
  filtered_df <- reactive({    
    validate({
      need(input$column_select != "", "No column(s) selected")
    })
    df[1:input$row_select,input$column_select]
  })
  
  output$dataframe <- renderTable({
    filtered_df()
  })

  
  # Downloadable csv of selected dataset
  output$download_data <- downloadHandler(
    filename = function(){"Animal_Outtake_Data.csv"},
    content = function(filename) {
      write.csv(filtered_df(), filename, row.names = FALSE)
    }
  )
    
  
})