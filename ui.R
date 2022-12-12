library(shiny)
library(mathjaxr)

shinyUI(fluidPage(
  navbarPage("Exploring Austin Animal Center Data",
             tabPanel(title = "About",
                      titlePanel("About this App"),
                        mainPanel(h3("Purpose of the App"),
                                  "The purpose of this app is to explore a 
                                   dataset from the Austin animal shelter, and to
                                   create several supervised learning models predicting
                                   the outcomes for those animals.",
                                  h4("The Data"),
                                  HTML("The data comes from the Austin government
                                       website, and can be found <a href='https://data.austintexas.gov/Health-and-Community-Services/Austin-Animal-Center-Outcomes/9t4d-g238'> here </a>. It includes outcomes from different types
                                       of animals taken into animal centers in Austin, Texas. The outcomes
                                       include adoption, return to owner, and unfortunately, sometimes
                                       euthanasia, but Austin is the largest 'No Kill' city. The data is from
                                       2013 to present, and it was pulled December 6th. There are 12 features,
                                       and 146,445 unique animals represented, but the dataset will be cleaned
                                       and reduced for this project."),
                                  h4("The Tabs"),
                                  "There are four different sections of this app. The first is the 'About'
                                  section, which you are currently on. The second is the 'Data Exploration'
                                  section, where you can explore the data, seeing relevant summaries and graphs.
                                  The third section is the 'Modelling' section, which consists of several tabs.
                                  In this section, three different supervised learning models will be fit, and
                                  you will be able to see how they perform and create some predictions from the 
                                  models. The last section is the 'Data' section, where you can scroll through
                                  the data set, subset it if you so desire, and export the data.",
                                  img(src='soup.JPEG', align = "right")
                        
                      )),
             tabPanel("Data Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Select the types of summaries you would like to display"),
                          selectInput("summary_choice", "Types of Summaries", 
                                      choices = list("Bar Plot", "Histogram")),
                          # Only show this panel if the user selects "Bar Plot"
                          conditionalPanel(condition = "input.summary_choice == 'Bar Plot'",
                                           selectInput("graph_options", "What variable would you like to see?",
                                                       choices = list("Outcome.Type", "Animal.Type",
                                                                      "Color", "Breed", "Sex.upon.Outcome")),
                                           sliderInput("category_count", "Choose how many categories to see",
                                                       value = 7, min = 3, max = 10),#FIXME to be dynamic UGH
                                           selectInput("grouping_options", "What variable would you
                                                       like to color code by?",
                                                       choices = list("None","Outcome.Type", "Animal.Type",
                                                                       "Color", "Breed", "Sex.upon.Outcome"))),
                          selectInput("table_choice", "Types of Tables",
                                      choices = list("Contingency", "Numeric Summary")),
                          conditionalPanel(condition = "input.table_choice == 'Numeric Summary'",
                                           selectInput("numeric_sum","How would you like to view 
                                                       the Age variable?",
                                                       choices = list("Overall", "Animal.Type", "Outcome.Type", 
                                                                      "Outcome.Subtype", "Sex.upon.Outcome",
                                                                      "Color", "Breed"))),
                          conditionalPanel(condition = "input.table_choice == 'Contingency'",
                                           radioButtons("cont_table", "What type of contingency table?",
                                                       choices = list("One-Way", "Two-Way")),
                          conditionalPanel(condition = "input.cont_table == 'One-Way'",
                                           selectInput("one_way", "What variable would you
                                                       like to summarize?",
                                                       choices = list("Animal.Type", "Outcome.Type",
                                                                      "Outcome.Subtype", "Sex.upon.Outcome",
                                                                      "Color", "Breed"))
                                           ),
                          conditionalPanel(condition = "input.cont_table == 'Two-Way'",
                                           selectInput("two_way_1", "What is the first variable you
                                                       would like to summarize?",
                                                       choices = list("Outcome.Type", "Outcome.Subtype",
                                                                      "Animal.Type", "Sex.upon.Outcome",
                                                                      "Color", "Breed")),
                                           selectInput("two_way_2", "What is the second variable you
                                                       would like to summarize?",
                                                       choices = list("Animal.Type", "Outcome.Type",
                                                                      "Outcome.Subtype", "Sex.upon.Outcome",
                                                                      "Color", "Breed"))))
                          ),
                        mainPanel(
                          # Plot the graph
                          conditionalPanel(condition = "input.summary_choice == 'Bar Plot'",
                                             plotOutput("bar_plot")),
                          conditionalPanel(condition = "input.summary_choice == 'Histogram'",
                                           plotOutput("histogram")),
                          # Plot summary table
                          conditionalPanel(condition = "input.table_choice == 'Contingency'",
                                          tableOutput("cont_table")),
                          conditionalPanel(condition = "input.table_choice == 'Numeric Summary'",
                                           dataTableOutput("numeric_sum"))
                        ))
                      ),
             tabPanel("Modelling",
                      sidebarLayout(
                        sidebarPanel(
                            h4("Model Specifications"),
                            numericInput("train_prop", "Training Proportion", min = 0, max = 99,
                                         value = 80),
                            checkboxGroupInput("predictors","Select the predictor variables",
                                                choices = list("Animal.Type", "Month", "Year",
                                                               "Sex.upon.Outcome", "Age")),
                            actionButton("model_run", "Run Model"),
                            conditionalPanel(condition = "input.main_panels_models == 3",
                            h4("Select Predictor Values for Prediction"),
                            conditionalPanel(condition = "input.predictors.includes('Age')",
                                  numericInput("age_value", "Insert Age", min = 0, max = 30,
                                             value = 1)),
                            conditionalPanel(condition = "input.predictors.includes('Animal.Type')",
                                   selectInput("animal_type", "Choose an animal",
                                    choices = list("Cat", "Dog", "Other", "Bird"))),
                            conditionalPanel(condition = "input.predictors.includes('Month')",
                                    numericInput("month_val", "Input a month", min = 1, max = 12,
                                                 value = 1)),
                            conditionalPanel(condition = "input.predictors.includes('Year')",
                                    numericInput("year_val", "Input a year", min = 2013, max = 2022,
                                                 val = 2013)),
                            conditionalPanel(condition = "input.predictors.includes('Sex.upon.Outcome')",
                                    selectInput("sex_val", "Choose a sex type", 
                                                choices = list("Neutered Male", "Spayed Female", "Intact Female"
                                                               ,"Intact Male", "Unknown")))
                            ))
                        ,
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Modeling Info",
                                               withMathJax(),
                                               h3("Modelling Info"),
                                               "In this section, we will explore three different
                                               supervised learning models.",
                                               h4("Logistic Regression"),
                                               "The first model will be a classic logistic regression
                                               model inorder to predict the adoption outcome. In order 
                                               to make the model more manageable, it will focus on only two
                                               outcomes - adoption or transfer, where adoption will
                                               be considered to be the 'success'. Logistic regression works 
                                               by finding the probability of success, found by the formula below",
                                               helpText("$$\\frac{e^{\\beta_0+\\beta_1x}}{1 + e^{\\beta_0
                                                        +\\beta_1x}}$$"),
                                               "The benefits of a logistic regression model is that it
                                               is low-cost for running, easier to interpret than some of the other
                                               classification models. The drawbacks are that logistic regression
                                               requires a lot of assumptions about the data and its distribution,
                                               which might not necessarily be met.",
                                               h4("Random Forest"),
                                               "The next model will be a random forest model. Random forest is an
                                               extension of a bagged classification model, where it creates
                                               multiple trees from bootstrap samples,but it only uses a subset of
                                               predictors for each tree, typically",
                                               helpText("m = \\(\\sqrt{p}\\) predictors for classification."),
                                               "The benefits of random forest is that it does not rely on the
                                               assumptions that logistic regression does, and if there is a 
                                               feature that has a high correlation with the outcome variable, 
                                               the random forest method accounts for that using the predictor
                                               subsetting method, meaning the resulting trees will a higher
                                               reduction in variance overall, resulting in better predictions. The
                                               drawbacks of random forest are that it is less interpretable than
                                               logistic regression or a single tree, since trees are aggregated
                                               together, and it takes a higher computing power.",
                                               h4("Boosting Trees"),
                                               "The last classification model considered is a boosting tree model.
                                               Boosting trees work by first creating one classification tree, and
                                               then grow trees sequentially from that, updating predictions as the
                                               trees grow. This often works better than random forest and bagging
                                               since the trees are grown in a sequence. The drawbacks of boosting
                                               are, similar to random forest, the results are less interpretable
                                               and it requires a higher computing power.", value = 1),
                                      tabPanel("Model Fitting",
                                               h4("Fit Statistics From Training Data"),
                                               tableOutput("fit_statistics"),
                                               fluidRow(h4("Variable Importance Plots"),
                                                 column(width = 6,
                                                        "Random Forest",
                                                        plotOutput("var_imp_rf")),
                                                 column(width = 6,
                                                        "Boosting Trees",
                                                        plotOutput("var_imp_bt"))
                                               ),
                                               h4("Test Fit Statistics"),
                                               tableOutput("fit_statistics_test")
                                               ),
                                      tabPanel("Prediction",
                                               h4("Predicted Outcome using User Selections"),
                                               tableOutput("prediction"),
                                               value = 3),
                                      id = "main_panels_models")
                      )
                      )
             )
                      ,
             tabPanel("Data",
                      sidebarLayout(
                        uiOutput("sidebar_output"),
                        mainPanel(
                          tableOutput("dataframe")
                        )
                      )
                      
             )
  )
  # Application title
  # titlePanel("Exploring Austin Animal Shelter Data"),
  # sidebarLayout(
  #   sidebarPanel(
  #   ),
  #   mainPanel(
  #   )
  # )
  
))