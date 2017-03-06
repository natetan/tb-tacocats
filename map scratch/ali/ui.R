# USER INTERFACE MAP SCRATCH
library(shiny)
library(plotly)

pageWithSidebar(
  headerPanel('Iris'),
  sidebarPanel(
    # Radio buttons to choose what you want the map to represent
    # returns string specifying actual column in the data table tb.data.iso the category relates to
    radioButtons("radio", label = h3("Map"),
                 choices = list("Tuberculosis Infection" = "Incidence.per.100.000.people", "Tuberculosis Mortality" = "Death.by.TB.per.100.people", 
                                "Drug Resistand Tuberculosis Infection" = "Confirmed.cases.of.RR.MDR.TB", 
                                "Tuberculosis Infection Given HIV Positive" = "Incidence.per.100.000.people..HIV.positive"), 
                 selected = 1)
    
    
    
  ),

  
  mainPanel(
    tabsetPanel(type= "tabs",
                tabPanel(
      'Scatter Plot',
      sidebarLayout(
        # WIDGET STUFF GOES HERE (inside sidebarPanel)
        sidebarPanel(
          selectInput(
            'tab3.y.axis',
            label = 'Y Axis',
            choices = c('Incidence', 'Mortality', 'Drug Resistant')
          )
        ),
        # WIDGET STUFF END
        
        # VISUAL STUFF HERE (inside mainPanel)
        
      )
     )
    )
   )
