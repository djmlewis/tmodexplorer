#
# (C) DJMLEWIS.EU 2018
#
library(tools)
library(shiny)
library(readr)
library(dplyr) 
library(purrr)
library(tidyr)
library(tmod)
library(grid)
library(gridExtra)
library(ggplot2)
library(utf8)
library(stringr)
library(shinythemes)
library(forcats)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(cowplot)

####--------------------------
options(shiny.maxRequestSize=30*1024^2) 


# Define serverside functions 
source('serverFunctions.R')
source('serverFunctions_Mods.R')

# Define plotting functions 
source('plotFunctions.R')
source('plotFunctions_Mods.R')
source('plotFunctions_Cyts.R')
source('plotFunctions_Cells.R')

# Define UI 
source('GUI.R')

# Define server 
source('serverSide.R')

# Run the application
shinyApp(ui = ui, server = server)
