# INFO 201 Final Project

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
                              img(src='bacteria.png', align = "left", width=480, height=350),
                              h3("What Is It?", align = "center"),
                              p("Tuberculosis or TB, as it’s commonly called is a contagious infection that usually attacks the lungs. 
                                It can also spread to other parts of the body, like the brain as well as the spine. A bacteria called", 
                                em("Mycobacterium tuberculosis"),"causes it.", align = "center"),
                              h3("Why Is It Important?", align = "center"),
                              p("Tuberculosis (TB) is one of the world’s deadliest diseases:", align = "center"),
                              p("1. One third of the world’s population is infected with TB.", align = "center"),
                              p("2. In 2015, 10.4 million people around the world became sick with TB disease. There were 1.8 million TB-related deaths worldwide.", align = "center"),
                              p("3. TB is a leading killer of people who are HIV positive.", align = "center"),
                              p("4. Drug resistant TB has evolved and is spreading.", align = "center"),
                              h3("How Does It Spread?", align = "center"),
                              p("TB spreads from person to person through the air. When people with TB cough, sneeze or spit, they propel the 
                                TB bacteria into the air. A person needs to inhale only a few of these germs to become infected.", align = "center"),
                              img(src='symptoms.png', align = "right", width=475, height=310),
                              h3("What Are The Symptoms?", align = "center"),
                              p("1. A cough that lasts more than 3 weeks",align = "center"),
                              p("2. Chest pain",align = "center"),
                              p("3. Coughing up blood",align = "center"),
                              p("4. Feeling tired all the time",align = "center"),
                              p("5. Night sweats",align = "center"),
                              p("6. Chills",align = "center"),
                              p("7. Fever",align = "center"),
                              p("8. Loss of appetite",align = "center"),
                              p("9. Weight loss",align = "center"),
                              h3("What's Happening Within The Body?",align = "center"),
                              img(src='progression.png', align = "right"),
                              #selectInput(
                              #'testing4',
                              #label = 'Testing first panel',
                              #choices = c('Yes', 'No')
                              #),
                              tableOutput('table1')
                              # VISUAL STUFF END
                              #)
                              ),
                            
                            # TAB 2 MAP STUFF
                            tabPanel(
                              'Panel 2',
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
                                  plotlyOutput('tab3.plot')
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
                                  selectInput('bar.year',label = "Year",choices = 2000:2015),
                                  
                                  selectInput("country1", label = "Country 1",choices = country.names[,1]),
                                  selectInput("country2", label = "Country 2",choices = country.names[,1])
                                ),
                                # WIDGET STUFF END
                                
                                # VISUAL STUFF HERE (inside mainPanel)
                                mainPanel(
                                  plotOutput('tab4.plot')
                                )
                                # VISUAL STUFF END
                              )
                            ),
                            
                            # TAB 5
                            tabPanel(
                              'Panel 5',
                              tableOutput("table"),
                              #verbatimTextOutput("summary"),
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
                            # TAB 6
                            tabPanel(
                              'Panel 6',
                              sidebarLayout(
                                # WIDGET STUFF GOES HERE (inside sidebarPanel)
                                sidebarPanel(
                                  actionButton("goButton", "Go!"),
                                  actionButton("reset", "Reset")
                                ),
                                # WIDGET STUFF END
                                
                                # VISUAL STUFF HERE (inside mainPanel)
                                mainPanel(
                                  plotOutput('tab6.plot')
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
  
  tb_world_data <- `colnames<-`(world_data, c('long','lat', 'group', 'order', 'name', 'subregion', 'ISO3'))
  map_tb <- left_join(tb_world_data, tb.data.iso, by = 'ISO3')
  
  incidence1 <- select(map_tb, long,lat,group,Year,ISO3, Incidence.per.100.000.people)
  death1 <- select(map_tb, long,lat,group,Year,ISO3, Death.by.TB.per.100.000.people)
  mutated <- select(map_tb, long,lat,group,Year,ISO3, Confirmed.cases.of.RR..MDR.TB)
  hiv <- select(map_tb, long,lat,group,Year,ISO3, Incidence.per.100.000.people..HIV.positive)
  
  filtered <- reactive({
    data <- switch(input$map.type, Incidence = incidence1, Mortality = death1, `Drug Resistance` = mutated, HIV = hiv) %>%
      filter(Year == input$map.year)
    
    return(data)
    
    
  })
  
  # Map plot
  output$tab2mapplot <- renderPlot({
    ggplot(data = filtered(), aes(long,lat,group=group, fill = filtered()[,6])) + geom_polygon() +
      #facet_wrap(~ Year)+
      ggtitle("Tuberculosis Over Time")+ 
      scale_fill_gradientn(colours = c("yellow", "red"), na.value = "white")
  })
  
  # Text Output
  output$tab2text <-  renderText({
    type.result <- input$map.type
    if (type.result == 'HIV') {
      type.result <- 'cases of TB given HIV'
    }
    output <- paste0('This map shows ', type.result, ' in each country for the year ', input$map.year)
    return(output)
  })
  
  
  #TAB 3 
  tab3.better.data <- read.csv("data/better-data.csv", stringsAsFactors = F)
  colnames(tab3.better.data) <- c("X", "Country",
                                  "Year",
                                  "Incidence",
                                  "Incidence per 100,000 people",
                                  "Incidence (HIV positive)",
                                  "Incidence per 100,000 people (HIV positive",
                                  "Death by TB",
                                  "Death by TB per 100,000 people",
                                  "Drug Resistant TB cases among TB cases",
                                  "New cases tested for RR-/MDR-TB",
                                  "Previously treated cases of RR-/MDR-TB",
                                  "Confirmed cases of RR-/MDR-TB",
                                  "Cases started on MDR-TB treatment")
  
  values <- reactiveValues()
  values$yaxis <- ''
  
  tab3.data <- reactive({
    values$yaxis <- input$tab3.y.axis
    data <- tab3.better.data %>% 
      dplyr::mutate(`Mortality` = `Death by TB`) %>% 
      dplyr::mutate(`Treated For Drug Resistance` = `Cases started on MDR-TB treatment`) %>% 
      dplyr::mutate(`HIV` = tab3.better.data[, 5]) %>% 
      filter(Year == input$tab3.year)
    return(data)
  })
  # Help insert selected year!!!!!!!!!!!!!!!!!!!!!!
  # Help make widget work!!!!!!!!!!!!
  output$tab3.plot <- renderPlotly({
    y.axis <- input$tab3.y.axis
    
    
    plot <- ggplot(data = tab3.data()) 
    if (y.axis == 'Mortality') {
      plot <- plot + geom_point(mapping = aes(x = Incidence, y = Mortality, color = `Confirmed cases of RR-/MDR-TB`, fill = `Country`)) +
        ylim(0, max(tab3.data()$Mortality) + mean(tab3.data()$Mortality))
    }
    if (y.axis == 'Treated For Drug Resistance') {
      plot <- plot + geom_point(mapping = aes(x = Incidence, y = `Treated For Drug Resistance`, color = `Confirmed cases of RR-/MDR-TB`)) +
        ylim(0, max(tab3.data()$`Treated For Drug Resistance`) + mean(tab3.data()$`Treated For Drug Resistance`))
    }
    if (y.axis == 'HIV') {
      plot <- plot + geom_point(mapping = aes(x = Incidence, y = HIV, color = `Confirmed cases of RR-/MDR-TB`)) +
        ylim(0, max(tab3.data()$HIV) + mean(tab3.data()$HIV))
    }
      plot <- plot +
        ggtitle("Tuberculosis By Country") + 
        scale_color_gradientn(colours = c("blue", "red")) + 
        xlim(0, 1000000) +
        xlab("Incidence") +
        ylab(input$tab3.y.axis) +
        labs(colour ='Number of confirmed cases of RR-/MDR-TB') +
        # scales library removes scientific notation and adds commas to values
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme(legend.position="none")
    
    plot <- ggplotly(plot)
    return(plot)
  })
  
  #TAB 4
  bargraph.data <- reactive({
    data <- main.data %>% 
      select(Country,Year,Incidence,Death.by.TB) %>% 
      filter(Year == input$bar.year) %>% 
      select(Country, Incidence, Death.by.TB) %>% 
      gather(key, value, -Country) %>% 
      filter(Country == input$country1 | Country == input$country2 ) %>% 
      arrange(Country)
    return(data)
  })
  
  
  output$tab4.plot <- renderPlot({
    plot <- ggplot(data = bargraph.data() ) +
      #aes(factor(Country), key, fill = value) + 
      #geom_bar(stat="identity", position = "dodge")
      
      aes(x = Country ,y = value) +  
      geom_bar(aes(fill = key), position = "dodge", stat="identity")
    
    
    return(plot)
  })
  
  #TAB 5 SUMMARY
  #STILL NEEDS WORK
  tab5.better.data <- tab3.better.data[4:9]
  output$table <- renderTable({
    data.frame(summary(tab5.better.data))
  })
  
  
  
  #TAB 6 Go Button 
  
  # initialize reactive values
  
  ts <- reactiveValues( counter=1)
  
  output$tsplot <- renderPlot({
    plot(sum.columns$Year[1:ts$counter], sum.columns$`Total Drug Resistant`[1:ts$counter], xlim=c(2000,2016), ylim=c(0,130000), xlab="Year",
         ylab="DRTB", type="l", main="Forecasted Cost Time series")
    
  })
  
  observe({
    isolate({
      if (ts$counter > 1){
        sum.columns$Year[ts$counter]=sum.columns$Year[ts$counter-1]
      }
      ts$counter=ts$counter+1    
    })
    if (((isolate(ts$counter) < 100)) & (input$goButton > 0)){
      invalidateLater(200, session)
    }
  })
  
  observe({
    if (input$reset > 0){
      ts$counter <<- 1
      
    }
    
  })
}



shinyApp(ui = ui, server = server)