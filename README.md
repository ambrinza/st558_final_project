# Purpose
This app was made to explore and predict the data taken from the Austin Animal Centers, focusing on the outtake data.
There are several types of outtakes: Adoption, Transfer, Return to Owner, etc. There are also different animal types,
ages, and genders in the data, as well as time of entry into the shelter. There are several tabs - an About tab to discuss
what the app is for, a Data Exploration tab to do some EDA, a Modelling tab to fit models and make predictions, and finally a Data
tab to output the dataset.

# Packages
Below are the packages needed:
```
library(shiny)
library(mathjaxr)
library(caret)
library(tidyverse)
library(gbm)
```
You can use this code to install all those packages:
```
pkgs <- c("shiny","mathjaxr","caret","tidyverse","gbm")
install.packages(pkgs)
```

# Code to Run
The code necessary to run this app successfully:
```shiny::runGitHub(repo = "ambrinza/st558_final_project")```

