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
main.data <- read.csv("data/main-data.csv", stringsAsFactors = F)


combined.data <- left_join(incidence, mortality) %>% 
  left_join(drug.resistant)

# Removes brackets and everything inside brackets in the data of a given column
ModifyColumn <- function(col) {
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




countries <- map_data('world')

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
          img(src='www/bacteria.png', align = "center"),
          h3("What Is It?", align = "center"),
          p("Tuberculosis or TB, as it’s commonly called is a contagious infection that usually attacks the lungs. 
            It can also spread to other parts of the body, like the brain as well as the spine. A bacteria called", 
            em("Mycobacterium tuberculosis"),"causes it."),
          h3("Why Is It Important?", align = "center"),
          p("Tuberculosis (TB) is one of the world’s deadliest diseases:"),
          p("1. One third of the world’s population is infected with TB."),
          p("2. In 2015, 10.4 million people around the world became sick with TB disease. There were 1.8 million TB-related deaths worldwide."),
          p("3. TB is a leading killer of people who are HIV positive."),
          p("4. Drug resistant TB has evolved and is spreading."),
          h3("How Does It Spread?", align = "center"),
          p("TB spreads from person to person through the air. When people with TB cough, sneeze or spit, they propel the 
            TB bacteria into the air. A person needs to inhale only a few of these germs to become infected."),
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
    
    # TAB 2 MAP STUFF
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
          plotOutput('tab2mapplot')
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
          )
        ),
        # WIDGET STUFF END
        
        # VISUAL STUFF HERE (inside mainPanel)
        mainPanel(
          plotOutput('tab3.plot')
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
          tableOutput('table31')
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
  
  #TAB 2 MAP
  tb.data <- read.csv("data/main-data.csv", stringsAsFactors = F)
  Country.edited <- gsub("United States of America", "USA",tb.data$Country)
  altered.tb.data <- mutate(tb.data, Country.edited)
  tb.data.iso <-   mutate(altered.tb.data, ISO3 = iso.alpha(Country.edited, n=3))
  
  # Take in world data and add ISO3 for later sorting
  world_data <- map_data("world") %>% 
    mutate(ISO3 = iso.alpha(region, n=3))
  View(world_data)
  
  tb_world_data <- `colnames<-`(world_data, c('long','lat', 'group', 'order', 'name', 'subregion', 'ISO3'))
  map_tb <- left_join(tb_world_data, tb.data.iso, by = 'ISO3')
  
  
  # Map plot
  output$tab2mapplot <- renderPlot({
    ggplot(map_tb, aes(long,lat,group=group, fill = Incidence.per.100.000.people)) + geom_polygon() +
      facet_wrap(~ Year)+
      ggtitle("Tuberculosis Over Time")
  })
  
  
  #TAB 3 
  test.data <- reactive({
    data <- main.data %>% 
      filter(Year == 2015)
    return(data)
  })
  # Plot not displaying
  output$tab3.plot <- renderPlot({
    plot <- ggplot(data = test.data()) +
      geom_point(mapping = aes(x = Incidence, y = Death.by.TB, color = Confirmed.cases.of.RR..MDR.TB))
    return(plot)
  })
  #TAB 4
  
  #TAB 5 SUMMARY
  #doesnt work 
  output$summary <- renderPrint({
    summary(combined.data)
  })
}

shinyApp(ui = ui, server = server)
