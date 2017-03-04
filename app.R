# INFO 201 Final Project

# Required libraries
library(dplyr)
library(shiny)
library(rsconnect)
library(ggplot2)

incidence <- read.csv("data/tb-country-incidence.csv", stringsAsFactors = F)
mortality <- read.csv("data/tb-country-mortality.csv", stringsAsFactors = F)
drug.resistant <- read.csv("data/tb-country-drug-resistant.csv", stringsAsFactors = F)

combined.data <- left_join(incidence, mortality) %>% 
  left_join(drug.resistant)
View(combined.data)

countries <- map_data('world') %>% 
  View()

ui <- fluidPage(
  titlePanel(""),
  
  tabsetPanel(type = 'tabs',
    # TAB 1
    tabPanel(
      'Panel 1',
      sidebarLayout(
        # WIDGET STUFF GOES HERE (inside sidebarPanel)
        sidebarPanel(
          selectInput(
            'testing',
            label = 'Testing',
            choices = c('Yes', 'No')
          )
        ),
        # WIDGET STUFF END
        
        # VISUAL STUFF HERE (inside mainPanel)
        mainPanel(
          selectInput(
            'testing4',
            label = 'Testing first panel',
            choices = c('Yes', 'No')
          ),
          tableOutput('table1')
        )
        # VISUAL STUFF END
      )
    ),
    
    # TAB 2
    tabPanel(
      'Panel 2',
      sidebarLayout(
        # WIDGET STUFF GOES HERE (inside sidebarPanel)
        sidebarPanel(
          selectInput(
            'testing2',
            label = 'Testing 2',
            choices = c('Yes', 'No')
          ),
          selectInput(
            'testing5',
            label = 'Second Slider!',
            choices = c('Yes', 'No')
          )
        ),
        # WIDGET STUFF END
        
        # VISUAL STUFF HERE (inside mainPanel)
        mainPanel(
          selectInput(
            'testing3',
            label = 'Testing second panel',
            choices = c('Yes', 'No')
          ),
          tableOutput('table2')
        )
        # VISUAL STUFF END
      )
    ),
    
    # TAB 3
    tabPanel(
      'Panel 3',
      sidebarLayout(
        # WIDGET STUFF GOES HERE (inside sidebarPanel)
        sidebarPanel(
          selectInput(
            'testing2',
            label = 'Testing 2',
            choices = c('Yes', 'No')
          ),
          selectInput(
            'testing5',
            label = 'Second Slider!',
            choices = c('Yes', 'No')
          )
        ),
        # WIDGET STUFF END
        
        # VISUAL STUFF HERE (inside mainPanel)
        mainPanel(
          selectInput(
            'testing3',
            label = 'Testing second panel',
            choices = c('Yes', 'No')
          ),
          tableOutput('table2')
        )
        # VISUAL STUFF END
      )
    ),
    
    # TAB 4
    tabPanel(
      'Panel 4',
      sidebarLayout(
        # WIDGET STUFF GOES HERE (inside sidebarPanel)
        sidebarPanel(
          selectInput(
            'testing2',
            label = 'Testing 2',
            choices = c('Yes', 'No')
          ),
          selectInput(
            'testing5',
            label = 'Second Slider!',
            choices = c('Yes', 'No')
          )
        ),
        # WIDGET STUFF END
        
        # VISUAL STUFF HERE (inside mainPanel)
        mainPanel(
          selectInput(
            'testing3',
            label = 'Testing second panel',
            choices = c('Yes', 'No')
          ),
          tableOutput('table2')
        )
        # VISUAL STUFF END
      )
    ),
    
    # TAB 5
    tabPanel(
      'Panel 5',
      sidebarLayout(
        # WIDGET STUFF GOES HERE (inside sidebarPanel)
        sidebarPanel(
          selectInput(
            'testing2',
            label = 'Testing 2',
            choices = c('Yes', 'No')
          ),
          selectInput(
            'testing5',
            label = 'Second Slider!',
            choices = c('Yes', 'No')
          )
        ),
        # WIDGET STUFF END
        
        # VISUAL STUFF HERE (inside mainPanel)
        mainPanel(
          selectInput(
            'testing3',
            label = 'Testing second panel',
            choices = c('Yes', 'No')
          ),
          tableOutput('table2')
        )
        # VISUAL STUFF END
      )
    )
  )
)

# Ali's code
#ui <- fluidPage(
#titlePanel(""),

#mainPanel(
#  tabsetPanel(type = "tabs",tabPanel("Introduction"), 
#              tabPanel("Map",fluidRow(column(width = 10, (map) )),
#                       fluidRow(column(width = 10, plotOutput("plot1")))),
#              tabPanel("Plot")
              
#  )
#)

#)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
