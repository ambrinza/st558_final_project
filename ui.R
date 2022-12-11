library(caret)
library(shiny)

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
                                  img(src='images/soup.JPEG', align = "right")
                        
                      )),
             tabPanel("Data Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Select the types of summaries you would like to display"),
                          selectInput("summary_choice", "Types of Summaries", 
                                      choices = list("Bar Plot", "Histogram")),
                          selectInput("table_choice", "Types of Tables",
                                      choices = list("Contingency", "Numeric Summary")),
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
                          conditionalPanel(condition = "input.table_choice == 'Numeric Summary",
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
                                                                      "Color", "Breed")),
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
                          # Plot bar plot
                          plotOutput("bar_plot"),
                          # Plot summary table
                          conditionalPanel(condition = "input.table_choice == 'Contingency'",
                                          tableOutput("cont_table")),
                          conditionalPanel(condition = "input.table_choice == 'Numeric Summary'",
                                           dataTableOutput("numeric_sum"))
                        )
                      ) ),
             tabPanel("Modelling"),
             tabPanel("Data")
  ),
  # Application title
  # titlePanel("Exploring Austin Animal Shelter Data"),
  # sidebarLayout(
  #   sidebarPanel(
  #   ),
  #   mainPanel(
  #   )
  # )
  
))