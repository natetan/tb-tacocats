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

server <- function(input, output, session) {
  
  #TAB 2
  # Data for the Map
  tb.data <- read.csv("data/main-data.csv", stringsAsFactors = F)
  Country.edited <- gsub("United States of America", "USA",tb.data$Country)
  altered.tb.data <- mutate(tb.data, Country.edited)
  tb.data.iso <-   mutate(altered.tb.data, ISO3 = iso.alpha(Country.edited, n=3))
  
  # Take in world data and add ISO3 for later sorting
  world_data <- map_data("world") %>% 
    mutate(ISO3 = iso.alpha(region, n=3))
  
  # Joins the tb data into maps
  tb_world_data <- `colnames<-`(world_data, c('long','lat', 'group', 'order', 'name', 'subregion', 'ISO3'))
  map_tb <- left_join(tb_world_data, tb.data.iso, by = 'ISO3')
  
  # Filtering the data by choices of the select input
  incidence1 <- select(map_tb, long,lat,group,Year,ISO3, Incidence.per.100.000.people)
  death1 <- select(map_tb, long,lat,group,Year,ISO3, Death.by.TB.per.100.000.people)
  mutated <- select(map_tb, long,lat,group,Year,ISO3, Confirmed.cases.of.RR..MDR.TB)
  hiv <- select(map_tb, long,lat,group,Year,ISO3, Incidence.per.100.000.people..HIV.positive)
  
  filtered <- reactive({
    data <- switch(input$map.type, Incidence = incidence1, Mortality = death1, `Drug Resistance` = mutated, HIV = hiv) %>%
      filter(Year == input$map.year)
    
    return(data)
  })
  
  # Plots map data for tb in the given year
  output$tab2mapplot <- renderPlot({
    ggplot(data = filtered(), aes(long,lat,group=group, fill = filtered()[,6])) + geom_polygon() +
      #facet_wrap(~ Year)+
      ggtitle("Tuberculosis Over Time")+ 
      scale_fill_gradientn(colours = c("yellow", "red"), na.value = "white")
  })
  
  # Outputs text that matches what the user selected
  output$tab2text <-  renderText({
    type.result <- input$map.type
    if (type.result == 'HIV') {
      type.result <- 'cases of TB given HIV'
    }
    output <- paste0('This map shows ', type.result, ' in each country for the year ', input$map.year)
    return(output)
  })
  
  #TAB 3
  # Renames the main data table to have better table names
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
  
  # Reactive values for the yaxis variable
  values <- reactiveValues()
  values$yaxis <- ''
  
  # Gets a reactive data set for the scatter plot
  tab3.data <- reactive({
    values$yaxis <- input$tab3.y.axis
    data <- tab3.better.data %>% 
      dplyr::mutate(`Mortality` = `Death by TB`) %>% 
      dplyr::mutate(`Treated For Drug Resistance` = `Cases started on MDR-TB treatment`) %>% 
      dplyr::mutate(`HIV` = tab3.better.data[, 5]) %>% 
      filter(Year == input$tab3.year)
    return(data)
  })
  
  # Renders a scatter plot using plotly
  output$tab3.plot <- renderPlotly({
    y.axis <- input$tab3.y.axis
    
    # Plots and changes the y axis limits using the max and mean of the chosen column
    plot <- ggplot(data = tab3.data()) 
    if (y.axis == 'Mortality') {
      plot <- plot + geom_point(mapping = aes(x = Incidence, y = Mortality, color = `Confirmed cases of RR-/MDR-TB`, fill = `Country`)) +
        ylim(0, max(tab3.data()$Mortality) + mean(tab3.data()$Mortality))
    }
    if (y.axis == 'Treated For Drug Resistance') {
      plot <- plot + geom_point(mapping = aes(x = Incidence, y = `Treated For Drug Resistance`, color = `Confirmed cases of RR-/MDR-TB`, fill = `Country`)) +
        ylim(0, max(tab3.data()$`Treated For Drug Resistance`) + mean(tab3.data()$`Treated For Drug Resistance`))
    }
    if (y.axis == 'HIV') {
      plot <- plot + geom_point(mapping = aes(x = Incidence, y = HIV, color = `Confirmed cases of RR-/MDR-TB`, fill = `Country`)) +
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
      scale_y_continuous(labels = comma)+
      theme(legend.position="none")
    
    #sets ggplot as plotly
    plot <- ggplotly(plot)
    return(plot)
  })
  
  #TAB 4
  #set reactive data for bar graph 
  bargraph.data <- reactive({
    data <- tab3.better.data %>% 
      select(Country,Year,Incidence, `Death by TB`) %>% 
      filter(Year == input$bar.year) %>% 
      select(Country, Incidence, `Death by TB`) %>% 
      gather(key, value, -Country) %>% 
      filter(Country == input$country1 | Country == input$country2 | Country == input$country3 ) %>% 
      arrange(Country)
    return(data)
  })
  
  #renders bar graph 
  output$tab4.plot <- renderPlot({
    plot <- ggplot(data = bargraph.data() ) +
      aes(x = Country ,y = value) +  
      geom_bar(aes(fill = key), position = "dodge", stat="identity") +
      ggtitle("TB Incidence & Mortality by Selected Countries")
    return(plot)
  })
  
  #TAB 5 SUMMARY
  #set reactive data for summary statistics 
  tab5.data <- reactive({
    data <- tab3.better.data %>% 
      filter(Year == input$tab4.year)
    data <- data[4:9]
    #if user chooses Incidence data 
    if (input$tab4.radio == 'Incidence') {
      data <- data %>% 
        drop_na() %>% 
        #get stats 
        summarize(
          Min = min(Incidence),
          Max = max(Incidence),
          Mean = mean(Incidence),
          Median = median(Incidence),
          `Min per 100,000` = min(`Incidence per 100,000 people`),
          `Max per 100,000` = max(`Incidence per 100,000 people`),
          `Mean per 100,000` = mean(`Incidence per 100,000 people`),
          `Median per 100,000` = median(`Incidence per 100,000 people`)
        )
      #if user chooses Incidence(HIV) data 
    } else if (input$tab4.radio == 'Incidence (HIV)') {
      data <- data %>% 
        drop_na() %>% 
        #get stats
        summarize(
          Min = min(`Incidence (HIV positive)`),
          Max = max(`Incidence (HIV positive)`),
          Mean = mean(`Incidence (HIV positive)`),
          Median = median(`Incidence (HIV positive)`),
          `Min per 100,000` = min(`Incidence per 100,000 people (HIV positive`),
          `Max per 100,000` = max(`Incidence per 100,000 people (HIV positive`),
          `Mean per 100,000` = mean(`Incidence per 100,000 people (HIV positive`),
          `Median per 100,000` = median(`Incidence per 100,000 people (HIV positive`)
        )
      #if user chooses Mortality data 
    } else {
      data <- data %>% 
        drop_na() %>% 
        #get stats
        summarize(
          Min = min(`Death by TB`),
          Max = max(`Death by TB`),
          Mean = mean(`Death by TB`),
          Median = median(`Death by TB`),
          `Min per 100,000` = min(`Death by TB per 100,000 people`),
          `Max per 100,000` = max(`Death by TB per 100,000 people`),
          `Mean per 100,000` = mean(`Death by TB per 100,000 people`),
          `Median per 100,000` = median(`Death by TB per 100,000 people`)
        )
    }
    #set up data (gather data and change column names)
    data <- gather(data)
    names(data)[1] <- paste("Statistic")
    names(data)[2] <- paste("Value")
    return(data)
  })
  
  #renders table for summary statistics 
  output$tab5.data <- renderTable({
    return(tab5.data())
  })
  
  
  #TAB 6 
  #initialize reactive values
  ts <- reactiveValues( counter=1)
  
  #renders line graph and sets up variables 
  output$tsplot <- renderPlot({
    plot(sum.columns$Year[1:ts$counter], sum.columns$`Total Drug Resistant`[1:ts$counter], xlim=c(2000,2016), ylim=c(0,130000), xlab="Year",
         ylab="DRTB", type="l", main="Number of Drug Resistant TB Cases Over Time")
  })
  
  #sets up Go Button for line graph animation by sequencially plotting points
  observe({
    isolate({
      if (ts$counter > 1){
        sum.columns$Year[ts$counter]=sum.columns$Year[ts$counter-1]
      }
      ts$counter=ts$counter+1    
    })
    if (((isolate(ts$counter) < 17)) & (input$goButton > 0)){
      invalidateLater(200, session)
    }
  })
  
  #sets up Reset Button by resetting the counter 
  observe({
    if (input$reset > 0){
      ts$counter <<- 1
    }
  })
}

shinyServer(server)