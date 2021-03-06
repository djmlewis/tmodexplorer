#
# (C) DJMLEWIS.EU 2018
#
library(tools)
library(shiny)
library(readr)
library(tibble) 
library(tidyr)
library(tmod)
library(grid)
library(gridExtra)
library(ggplot2)
library(utf8)
library(stringr)
library(shiny)
library(shinythemes)
library(forcats)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(cowplot)
library(xtable)
library(qgraph)
library(VennDiagram)
library(eulerr)
library(UpSetR)
library(openxlsx)
library(ggpubr)
library(gridGraphics)
library(dplyr) 
library(purrr)


####--------------------------
options(shiny.maxRequestSize=30*1024^2) 

# refresh the makedayPatterns
source('makeVaccineColours.R')
# Define serverside functions 
source('serverFunctions.R')
source('serverFunctions_Mods.R')
source('serverFunctions_muscle.R')

# Define plotting functions 
source('plotFunctions.R')
source('plotFunctions_Mods.R')
source('plotFunctions_Cyts.R')
source('plotFunctions_Cells.R')
source('network.R')

# Define UI 
source('GUI.R')

# Define server 
source('serverSide.R')

# Run the application
shinyApp(ui = ui, server = server)
