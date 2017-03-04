# INFO 201 Final Project

# Required libraries
library(dplyr)
library(shiny)
library(rsconnect)
library(ggplot2)

incidence <- read.csv("data/tb-country-incidence.csv", stringsAsFactors = F)
incidence.region <- read.csv("data/tb-region-incidence.csv", stringsAsFactors = F)
mortality <- read.csv("data/tb-country-mortality.csv", stringsAsFactors = F)
drug.resistant <- read.csv("data/tb-country-drug-resistant.csv", stringsAsFactors = F)
gdp <- read.csv("data/GDPcap_NOClimateChange_RCP85_SSP5.csv", stringsAsFactors = F)


combined.data <- left_join(incidence, mortality) %>% 
  left_join(drug.resistant)

ModifyColumn <- function(col) {
  col <- combined.data$Number.of.incident.tuberculosis.cases
  col <- gsub("\\[.*?\\]", "",col)
  col <- gsub(" ", "", col)
  return(col)
}

better.data <- combined.data %>% 
  mutate(`Incidence` = ModifyColumn(Number.of.incident.tuberculosis.cases),
         `Incidence per 100,000 people` = ModifyColumn(Incidence.of.tuberculosis..per.100.000.population.per.year.),
         `Incidence (HIV positive)` = ModifyColumn(Number.of.incident.tuberculosis.cases....HIV.positive.cases.),
         `Incidence per 100,000 people (HIV positive` = ModifyColumn(Incidence.of.tuberculosis..per.100.000.population...HIV.positive.cases.),
         `Death by TB` = ModifyColumn(Number.of.deaths.due.to.tuberculosis..excluding.HIV),
         `Death by TB per 100,000 people` = ModifyColumn(Deaths.due.to.tuberculosis.among.HIV.negative.people..per.100.000.population.),
         `Drug Resistant TB cases among TB cases` = ModifyColumn(Estimated.MDR.RR.TB.cases.among.notified.pulmonary.TB.cases),
         `New cases tested for RR-/MDR-TB` = ModifyColumn(New.cases.tested.for.RR..MDR.TB....),
         `Previously treated cases of RR-/MDR-TB` = ModifyColumn(Previously.treated.cases.tested.for.RR..MDR.TB....),
         `Confirmed cases of RR-/MDR-TB` = ModifyColumn(Confirmed.cases.of.RR..MDR.TB),
         `Cases started on MDR-TB treatment` = ModifyColumn(Cases.started.on.MDR.TB.treatment)) %>% 
  select(Country,
         Year,
         `Incidence`,
         `Incidence per 100,000 people`,
         `Incidence (HIV positive)`,
         `Incidence per 100,000 people (HIV positive`,
         `Death by TB`,
         `Death by TB per 100,000 people`,
         `Drug Resistant TB cases among TB cases`,
         `New cases tested for RR-/MDR-TB`,
         `Previously treated cases of RR-/MDR-TB`,
         `Confirmed cases of RR-/MDR-TB`,
         `Cases started on MDR-TB treatment`)

write.csv(better.data, file = 'data/main-data.csv')

countries <- map_data('world')

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
    
    # TAB 3 (GDP stuff)
    tabPanel(
      'Scatter Plot',
      sidebarLayout(
        # WIDGET STUFF GOES HERE (inside sidebarPanel)
        sidebarPanel(
          selectInput(
            'tab3.y.axis',
            label = 'Y Axis',
            choices = c('Incidence', 'Mortality', 'Drug Resistant')
          ),
          selectInput(
            'tab3.region',
            label = 'Choose by region',
            choices = c('Africa', 'Asia')
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
