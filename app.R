# INFO 201 Final Project

# Required libraries
library(dplyr)
library(shiny)
library(rsconnect)
library(ggplot2)

tb.incidence <- read.csv('data/tb-country-incidence.csv', stringsAsFactors = FALSE)
tb.mortality <- read.csv('data/tb-country-mortality.csv', stringsAsFactors = FALSE)
tb.resistance <- read.csv('data/tb-country-drug-resistant.csv', stringsAsFactors = FALSE)

combined.data <- left_join(tb.incidence, tb.mortality) %>% 
  left_join(tb.resistance)
View(combined.data)

countries <- map_data('world') %>% 
  View()
