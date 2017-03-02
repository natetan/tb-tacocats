# INFO 201 Final Project

# Required libraries
library(dplyr)
library(shiny)
library(rsconnect)
library(ggplot2)

incidence <- read.csv("data/tb-country-incidence.csv", stringsAsFactors = F)
mortality <- read.csv("data/tb-country-mortality.csv", stringsAsFactors = F)
drug.resistant <- read.csv("data/tb-country-drug-resistant.csv", stringsAsFactors = F)

ui <- fluidPage(
  titlePanel(""),
  
  mainPanel(
    tabsetPanel(type = "tabs",tabPanel("Introduction"), 
                tabPanel("Map",fluidRow(column(width = 10, (map) )),
                               fluidRow(column(width = 10, plotOutput("plot1")))),
                tabPanel("Plot")
    
    )
  )
  
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)