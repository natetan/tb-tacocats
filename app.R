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
      titlePanel("Tuberculosis"),
      #strong(textOutput('hi')),
      #sidebarLayout(
        # WIDGET STUFF GOES HERE (inside sidebarPanel)
       # sidebarPanel(
          #selectInput(
            #'testing',
            #label = 'Testing',
            #choices = c('Yes', 'No')
         # )
       #),
        # WIDGET STUFF END
        
        # VISUAL STUFF HERE (inside mainPanel)
        mainPanel(
          #img(src='img/bacteria.png', align = "center"),
          h3("What Is It?", align = "center"),
          p("Tuberculosis or TB, as it’s commonly called is a contagious infection that usually attacks the lungs. 
            It can also spread to other parts of the body, like the brain as well as the spine. A bacteria called", 
            em("Mycobacterium tuberculosis"),"causes it."),
          h3("Why Is It Important?", align = "center"),
          p("Tuberculosis (TB) is one of the world’s deadliest diseases:"),
          p("1. One third of the world’s population is infected with TB."),
          p("2. In 2015, 10.4 million people around the world became sick with TB disease. There were 1.8 million TB-related deaths worldwide."),
          p("3.TB is a leading killer of people who are HIV infected."),
          h3("How Does It Spread?", align = "center"),
          p("TB spreads from person to person through the air. When people with TB cough, sneeze or spit, they propel the 
            TB germs into the air. A person needs to inhale only a few of these germs to become infected."),
          h3("What Are The Symptoms?", align = "center"),
          p("1. A cough that lasts more than 3 weeks"),
          p("2. Chest pain"),
          p("3. Coughing up blood"),
          p("4. Feeling tired all the time"),
          p("5. Night sweats"),
          p("6. Chills"),
          p("7. Fever"),
          p("8. Loss of appetite"),
          p("9. Weight loss"),
          #selectInput(
            #'testing4',
            #label = 'Testing first panel',
            #choices = c('Yes', 'No')
          #),
          tableOutput('table1')
        )
        # VISUAL STUFF END
      #)
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
      verbatimTextOutput("summary"),
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
  #TAB 1 ABOUT TB
  
  #TAB 2 
  
  #TAB 3 
  
  #TAB 4
  
  #TAB 5 SUMMARY
  #doesnt work 
  output$summary <- renderPrint({
    summary(combined.data)
  })
}

shinyApp(ui = ui, server = server)
