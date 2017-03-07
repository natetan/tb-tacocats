# USER INTERFACE MAP SCRATCH
library(shiny)
library(plotly)

pageWithSidebar(
  headerPanel('Iris'),
  sidebarPanel(
    # Radio buttons to choose what you want the map to represent
    # returns string specifying actual column in the data table tb.data.iso the category relates to
    radioButtons("tab2category", label = h3("Map"),
                 choices = list("Tuberculosis Infection" = "Incidence.per.100.000.people", "Tuberculosis Mortality" = "Death.by.TB.per.100.people", 
                                "Drug Resistand Tuberculosis Infection" = "Confirmed.cases.of.RR.MDR.TB", 
                                "Tuberculosis Infection Given HIV Positive" = "Incidence.per.100.000.people..HIV.positive"), 
                 selected = 1)
  ),

  
  mainPanel(
    tabsetPanel(type= "tabs",
                tabPanel(
                  # Play button goes through years automatically
                  "Sample Plot", plotOutput('tab2mapplot'),
                  # Slider chooses year to represent in map
                  sliderInput("tab2year", label = h3("Slider"), min = 2000, 
                              max = 2015, value = 2000))
    )
  )
)