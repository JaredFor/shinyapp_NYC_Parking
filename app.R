# df <- read_csv(file = "Parking_Violations_Issued_-_Fiscal_Year_2019_short_better.csv", col_names = TRUE)
violationCodes <- read_csv(file = 'DOF_Parking_Violation_Codes_cleaned.csv')
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(leaflet)
library(scales)
library(lattice)
library(htmlwidgets)
library(RColorBrewer)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("New York City Parking Violations"),
  verticalLayout(  
    flowLayout(
      checkboxGroupInput(inputId = "checkbox", label ="Select Locations", choices = unique(df$`Violation County`), selected = unique(df$`Violation County`)[],inline = FALSE),
      
      pickerInput(inputId = "pickerinput", label = "Violation Code", choices =sort(unique(df$`Violation Code`)),selected = unique(df$`Violation Code`)[], multiple = TRUE, options = list(`actions-box` = TRUE, dropupAuto = TRUE, header = "Nothing selected, nothing gained",  liveSearch = TRUE,  liveSearchNormalize = TRUE,  title = NULL )),
      
      dateRangeInput(inputId = "date", label = "Date range", start = "2018-1-1", end = "2018-11-11", min = "2018-01-01", max = "2018-11-11"))),
  
  tabsetPanel(type = "tabs",
              tabPanel("Tickets by Location", (verticalLayout(plotlyOutput("histogram1"), plotlyOutput("histogram1a")))  ),
              tabPanel("Violation Codes", plotlyOutput("histogram2")),
              tabPanel("Tickets by Time of Day", (verticalLayout(
                textOutput("revenue1"), 
                plotlyOutput("histogram3")))),
              tabPanel("Table", dataTableOutput("table")),
              tabPanel("Violation Code Definitions", dataTableOutput("violationtable")),
              tabPanel("Map", leafletOutput("map")),
              tabPanel("Revenue", textOutput("revenue")),
              tabPanel("ReadMe", (verticalLayout(helpText(a("Click Here For Description of NYC Parking", href="https://www.youtube.com/watch?v=UeHcZnvUIeE")
              ),textOutput("readme"))))
  ))

server <- function(input, output, session){
  filtered_data <- reactive({df %>% filter(`Violation County` %in% input$checkbox) %>% filter(`Violation Code` %in% input$pickerinput) %>% filter(`Issue Date` %inrange% input$date)})
  unfiltered_data <- reactive({df})
  
  violation_Codes <- reactive({violationCodes})
  output$histogram1   <-  renderPlotly({
    p <- ggplot(filtered_data(), aes(x = `Violation County`))+geom_bar()
    ggplotly(p) })
  
  output$histogram1a   <-  renderPlotly({
    p <- ggplot(unfiltered_data(), aes(x = `Violation County`, fill =`Violation County`))+geom_bar()
    ggplotly(p) })
  
  output$histogram2   <-  renderPlotly({
    p <- ggplot(data = filtered_data(), aes(x = `Violation Code`))+geom_bar()+
      theme_bw()
    ggplotly(p) })
  
  output$table <- renderDataTable({filtered_data()})
  
  output$violationtable <- renderDataTable({violation_Codes()})
  
  output$histogram3   <-  renderPlotly({
    p <- ggplot(data = filtered_data(), aes(x = `Violation Time`))+geom_histogram(stat = "count", binwidth = "96")+
      theme_bw()
    ggplotly(p) })
  
  output$revenue <- renderText({sum(as.integer(filtered_data()$`All Other Areas`))})
  
  output$revenue1 <- renderText({sum(as.integer(filtered_data()$`All Other Areas`))})
  
  
  output$readme <- renderText({print("This dataset was obtained from the City of New York,   https://data.cityofnewyork.us/City-Government/Parking-Violations-Issued-Fiscal-Year-2019/pvqr-7yc4 on November 2, 2018. Its most recent update was October 16, 2018.  
                                     Parking Violation Codes were obtained from https://data.ny.gov/Transportation/DOF-Parking-Violation-Codes/ncbg-6agr and https://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page    Registration Class Codes can be found at https://dmv.ny.gov/registration/registration-class-codes")})
  
  output$map <- renderLeaflet({
    leaflet(height = 30) %>%
      addTiles() %>%
      #   urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      # ) %>%
      setView(lng = -74.05, lat = 40.70, zoom = 10)  
  })
  }

shinyApp(ui = ui, server = server)