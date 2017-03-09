# INFO 201 Final Project
# Kianna Hales, Emily Nuri, Yulong Tan, Ali Salahuddin

# Required libraries
library(scales)
library(dplyr)
library(shiny)
library(rsconnect)
library(ggplot2)
library(tidyr)
library(shinythemes)
library(plotly)

source('ui.R') 
source('server.R')

shinyApp(ui = ui, server = server)
