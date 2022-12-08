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
                        ),
                        mainPanel(
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