# SERVER MAP SCRATCH

library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
library(rsconnect)

incidence <- read.csv("data/tb-country-incidence.csv", stringsAsFactors = F)
mortality <- read.csv("data/tb-country-mortality.csv", stringsAsFactors = F)
drug.resistant <- read.csv("data/tb-country-drug-resistant.csv", stringsAsFactors = F)

tb.data <- read.csv("data/main-data.csv", stringsAsFactors = F)
Country.edited <- gsub("United States of America", "USA",tb.data$Country)
altered.tb.data <- mutate(tb.data, Country.edited)
tb.data.iso <-   mutate(altered.tb.data, ISO3 = iso.alpha(Country.edited, n=3))


shinyServer(
  function(input, output) {
    
    # Take in world data and add ISO3 for later sorting
    # Take in world data and add ISO3 for later sorting
  world_data <- map_data("world") %>% 
    mutate(ISO3 = iso.alpha(region, n=3))
  View(world_data)
  
  tb_world_data <- `colnames<-`(world_data, c('long','lat', 'group', 'order', 'name', 'subregion', 'ISO3'))
  map_tb <- left_join(tb_world_data, tb.data.iso, by = 'ISO3')
  
  filtered <- reactive({
    data <- map_tb %>%
      filter(Year == input$map.year)
    
    return(data)
  })
  
    
    
    
    
    
    
  }
  
)
