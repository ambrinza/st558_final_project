library(shiny)
library(caret)
library(tidyverse)

df <- read.csv('data/Austin_Animal_Center_Outcomes.csv')

# Cleaning the data before it will be used in the shiny app
df <- df %>% 
  filter(Name != "") %>% 
  mutate(Name = gsub("\\*", "",Name)) %>%
  separate(MonthYear, c("Month", "Year"))

shinyServer(function(input, output) {
})