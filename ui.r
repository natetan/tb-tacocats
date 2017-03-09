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

incidence <- read.csv("data/tb-country-incidence.csv", stringsAsFactors = F)
incidence.region <- read.csv("data/tb-region-incidence.csv", stringsAsFactors = F)
mortality <- read.csv("data/tb-country-mortality.csv", stringsAsFactors = F)
drug.resistant <- read.csv("data/tb-country-drug-resistant.csv", stringsAsFactors = F)
gdp <- read.csv("data/GDPcap_NOClimateChange_RCP85_SSP5.csv", stringsAsFactors = F)
main.data <- read.csv("data/main-data.csv", stringsAsFactors = F)


country.names <- (main.data) %>% 
  filter(Year == 2015) %>% 
  select(2)

drug.resistant.years <- (main.data) %>% 
  select(Country,Year, Confirmed.cases.of.RR..MDR.TB) %>% 
  spread(Year, Confirmed.cases.of.RR..MDR.TB) %>% 
  select(2:17)
sum.columns <- data.frame(colSums(drug.resistant.years,na.rm=TRUE)) %>% 
  mutate(Year = 2000:2015)
colnames(sum.columns)<- c("Total Drug Resistant", "Year")

combined.data <- left_join(incidence, mortality) %>% 
  left_join(drug.resistant)

# Removes brackets and everything inside brackets in the data of a given column
ModifyColumn <- function(col) {
  col <- gsub("\\[.*?\\]", "",col)
  col <- gsub(" ", "", col)
  return(col)
}

# This makes data without periods in the column names
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

ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel(""),
                
                tabsetPanel(type = 'tabs',
                            # TAB 1: Introduction
                            tabPanel(
                              'About',
                              titlePanel("Tuberculosis"),
                              p("Emily Nuri, Ali Salahuddin, Yulong Tan, Kianna Hales", align = "left"),
                              h4("Citation: The data for this application was taken from World Health Organization", a("Link", href="http://www.who.int/mediacentre/factsheets/fs104/en/", align ='center')),
                              
                              # VISUAL STUFF HERE (inside mainPanel)
                              # Use colums to make flexible front page
                              fluidRow(
                                # contains all pictures in left column and who cares
                                column(5,
                                       img(src='bacteria.png', align = "left", width=450, height=320),
                                       img(src='whitebox.png', align = "left", width=450, height= 60),
                                       img(src='symptoms.png', align = "left", width=450, height=270),
                                       img(src='whitebox.png', align = "left", width=450, height= 60),
                                       img(src='xray.jpg', align = "left", width=450, height=270),
                                       img(src='whitebox.png', align = "left", width=450, height= 60),
                                       h3("Who Cares?", align = "center"),
                                       p("This application was built for individuals minorly interested in
                                         public health, research groups, , medical professionals, potential funding sources, 
                                         and the general public in order to spark awareness regarding this expanding issue.",
                                         align = "center")
                                       
                                ),
                                # contains project background and description
                                column(5, offset = 1,
                                       h3("What Is It? How is it Changing?", align = "center"),
                                       p("Tuberculosis or TB, as it’s commonly called is a contagious infection that usually attacks the lungs. 
                                         It can also spread to other parts of the body, like the brain as well as the spine. A bacteria called", 
                                         em("Mycobacterium tuberculosis"),"causes it. MDR/RR Tuberculosis is Multi Drug Resistant Tuberculosis
                                         that is immune to the most common TB treatment drugs including isoniazid (INH) and rifampicin (RMP).
                                         Humans and Tuberculosis are effectively in an arms race to find cures and evolve, respectively. ", align = "center"),
                                       h3("Why Is It Important?", align = "center"),
                                       p("Tuberculosis (TB) is one of the world’s deadliest diseases:", align = "center"),
                                       p("1. TB is one of the top 10 causes of death worldwide.", align = "center"),
                                       p("2. TB is a leading killer of people who are HIV positive.", align = "center"),
                                       p("3. In 2015, 10.4 million people around the world became sick with TB disease. There were 1.8 million TB-related deaths worldwide.", align = "center"),
                                       p("4. Drug resistant TB has evolved and is spreading. It was first recorded globally by the World Health Organization in 2005, and has since spread across continents.", align = "center"),
                                       h3("How Does It Spread?", align = "center"),
                                       p("TB spreads from person to person through the air.", align="center"), 
                                       p("When people with TB cough, sneeze or spit, they propel the TB bacteria into the air.", align="center"), 
                                       p("A person needs to inhale only a few of these germs to become infected.", align = "center"),
                                       h3("What Are The Symptoms?", align = "center"),
                                       p("1. A cough that lasts more than 3 weeks", align = "center"),
                                       p("2. Chest pain",align = "center"),
                                       p("3. Coughing up blood", align = "center"),
                                       p("4. Feeling tired all the time",align = "center"),
                                       p("5. Night sweats",align = "center"),
                                       p("6. Chills",align = "center"),
                                       p("7. Fever",align = "center"),
                                       p("8. Loss of appetite",align = "center"),
                                       p("9. Weight loss",align = "center"),
                                       h3("What's Happening Within The Body?",align = "center"),
                                       img(src='progression.png', align = "center")
                                       ))
                                ),
                            
                            # TAB 2 MAP
                            tabPanel(
                              'Map',
                              sidebarLayout(
                                # WIDGET STUFF GOES HERE (inside sidebarPanel)
                                sidebarPanel(
                                  radioButtons('map.type',label = 'Map Test',choices = c('Incidence', 'Mortality', 'Drug Resistance', 'HIV'), selected = 'Incidence'),
                                  selectInput('map.year',label = "Year",choices = 2000:2015),
                                  textOutput('tab2text')
                                ),
                                # WIDGET STUFF END
                                
                                # VISUAL STUFF HERE (inside mainPanel)
                                mainPanel(
                                  plotOutput('tab2mapplot'),
                                  p(),
                                  p("This map provides the ability to view incidence of TB, Mortality from TB, occurrences of Drug Resistant TB, and
                                    TB occurrence given HIV positive cases across a global map. It is also possible to select a specific year for each
                                    of these categories in order to observe trends in the data over time. The description on the left side of the 
                                    application can show you which data you are currently looking at. Because  Drug Resistant strains of TB are new and 
                                    rising, there are no recorded data on them before 2005. Countries with no data collected are represented by empy spaces
                                    here to allow for focus on data available", align = "center")
                                  )
                                # VISUAL STUFF END
                                  )
                                ),
                            
                            # TAB 3 (Scatter Plot)
                            tabPanel(
                              'Scatter Plot',
                              p("The Scatter Plot below presents a plotted point for each country. The Y axis can change into 
                                recording Mortality from TB, Treated for Drug Resistance TB cases and HIV cases while the X 
                                axis records the number of incidents. This plot can be observed by a specific year so that it 
                                is easy to see the TB trends in each country over the years or look at a specific year of interest. 
                                There is a hovering feature which allows one to view the specific statistics of a particular country 
                                as well as zoom in on any plotted point, or group of plotted points, of the graph.",
                                align = "center" ),
                              sidebarLayout(
                                # WIDGET STUFF GOES HERE (inside sidebarPanel)
                                
                                sidebarPanel(
                                  selectInput(
                                    'tab3.y.axis',
                                    label = 'Y Axis',
                                    choices = c('Mortality', 
                                                'Treated For Drug Resistance', 
                                                'HIV')
                                  ),
                                  selectInput(
                                    'tab3.year',
                                    label = 'Year',
                                    choices = c(2000:2015))
                                ),
                                # WIDGET STUFF END
                                
                                # VISUAL STUFF HERE (inside mainPanel)
                                mainPanel(
                                  p(),
                                  plotlyOutput('tab3.plot'),
                                  img(src='scatterlegend.png', align = "center", width=585, height=85),
                                  p(),
                                  p("The color reflects the number of confirmed RR MDR cases which is useful for determining
                                    where the drug resistant strains originated, under what previous conditions, where they thrive outside
                                    of origin, and where we should focus our research as this disease rises. We see that drug resistant cases 
                                    are not recorded until 2005, after which the color corresponds to the amount of drug resistant cases.", align = "center")
                                  )
                                # VISUAL STUFF END
                                  )
                                ),
                            
                            # TAB 4 Bar Graph
                            tabPanel(
                              'Bar Graph',
                              sidebarLayout(
                                # WIDGET STUFF GOES HERE (inside sidebarPanel)
                                sidebarPanel(
                                  selectInput('bar.year',label = "Year",choices = 2000:2015),
                                  
                                  selectInput("country1", label = "Country 1",choices = country.names[,1]),
                                  selectInput("country2", label = "Country 2",choices = country.names[,1]),
                                  selectInput("country3", label = "Country 3",choices = country.names[,1])
                                ),
                                # WIDGET STUFF END
                                
                                # VISUAL STUFF HERE (inside mainPanel)
                                mainPanel(
                                  plotOutput('tab4.plot'),
                                  p(),
                                  p("This Bar graph compares one, two or three country`s Incidence of TB and Death by TB rates. 
                                    This is useful for comparing specific countries of interest. It is also useful for understanding, 
                                    the difference between Tuberculosis infection and mortality include other factors possessed by each
                                    country including economic status, access to healthcare, location, and hygienic resources. This very 
                                    directly shows that there is variance among countries that may possess similar incidence rates, 
                                    but have vastly different mortality rates. The user gets to pick which countries they would like 
                                    to compare to explore this idea for themselves.", align = "center")
                                  )
                                # VISUAL STUFF END
                                  )
                                ),
                            
                            # TAB 5 Summary Table
                            tabPanel(
                              'Summary',
                              tableOutput("table"),
                              #verbatimTextOutput("summary"),
                              p("These statistics report summary statistics on TB incidences, mortality, and TB incidence given someone is HIV positive.
                                Here we explore the relationship between HIV and TB infection. HIV suppresses the immune system, and although it is not
                                the primary cause for death, it is responsible for breaking the immune system down to the point where a person cannot 
                                fight off the common cold. Therefore, it is very common in countries that host both diseases for individuals who have 
                                HIV to also get TB. Here, we evaluate this occurrence on a global scale. It is detrimental to the fight against these 
                                diseases to understand how they interact with one another.", align = "center"),
                              sidebarLayout(
                                # WIDGET STUFF GOES HERE (inside sidebarPanel)
                                sidebarPanel(
                                  radioButtons(
                                    'tab4.radio',
                                    label = 'Data to display',
                                    choices = c(
                                      'Incidence',
                                      'Incidence (HIV)',
                                      'Mortality'
                                    )
                                  ),
                                  selectInput(
                                    'tab4.year',
                                    label = 'Year',
                                    choices = c(2000:2015)
                                  )
                                ),
                                # WIDGET STUFF END
                                
                                # VISUAL STUFF HERE (inside mainPanel)
                                mainPanel(
                                  h4("Summary Statistics:"),
                                  tableOutput('tab5.data')
                                )
                                # VISUAL STUFF END
                              )
                              ),
                            # TAB 6 Animated Line Graph
                            tabPanel(
                              'Line Graph',
                              sidebarLayout(
                                # WIDGET STUFF GOES HERE (inside sidebarPanel)
                                sidebarPanel(
                                  actionButton("goButton", "Go!"),
                                  actionButton("reset", "Reset")
                                ),
                                # WIDGET STUFF END
                                
                                # VISUAL STUFF HERE (inside mainPanel)
                                mainPanel(
                                  plotOutput('tsplot'),
                                  p(),
                                  p("The Line plot shows the number of recorded Drug Resistant TB cases from 2000 to 2015.
                                    By using the Go Button, the animation will play in order to demonstrate the rate of 
                                    growth of Drug Resistant TB. This demonstrates the strain's ability to propagate and 
                                    the serious danger it presents to global health.", align = "center")
                                )
                                # VISUAL STUFF END
                              )
                            )
                    )
                
            )

shinyUI(ui)