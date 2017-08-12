#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source('global.R')
library(RColorBrewer)
stations <- getStationData()
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NYC Citibike availability at stations"),
   
   tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # value is always yyyy-mm-dd, even if the display format is different
        dateInput("date_journey", "Date:", value = Sys.Date(), max = Sys.Date() + 10, format = "mm/dd/yy"),
        
        sliderInput("slider_hours", "Time:", min=0, max=23, value=0, step = 1),
        
        # selectInput("state", "Choose a state:",
        #             list(`East Coast` = c("NY", "NJ", "CT"),
        #                  `West Coast` = c("WA", "OR", "CA"),
        #                  `Midwest` = c("MN", "WI", "IA"))
                    
        selectInput("station", "Choose a station:", stations
        ),
        sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
        actionButton("recalc", "New points")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         
         # tabsetPanel(        
         #   tabPanel("map", h3(textOutput("add")),
         #            tags$style(".leaflet {height: 400px;}"), 
         #            leafletOutput('myMap'))),
         
         leafletOutput("map"),
         
         sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                     value = range(quakes$mag), step = 0.1
         ),
         selectInput("colors", "Color Scheme",
                     rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
         ),
         checkboxInput("legend", "Show legend", TRUE),
         
         textOutput("dateOutput"),
         textOutput("timeOutput"),
         textOutput("stationOutput")
      )
   )
)

library(shiny)
library(rCharts)
library(leaflet)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$stationOutput <- renderText({
     paste("Station you chose", input$station)
   })
   
   output$dateOutput <- renderText({
     paste("Date you chose", input$date_journey)
   })
   
   output$timeOutput <- renderText({
     paste("Time you chose", input$slider_hours)
   })
   #c(40.750664, 40.750664, 40.750664, 40.750664, 40.750664, 40.750664), c(-74.001768, -74.001768, -74.001768)
   
   # Reactive expression for the data subsetted to what the user selected
   filteredData <- reactive({
     quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
   })
   
   # This reactive expression represents the palette function,
   # which changes as the user makes selections in UI.
   colorpal <- reactive({
     colorNumeric(input$colors, quakes$mag)
   })
   
   output$map <- renderLeaflet({
     # Use leaflet() here, and only include aspects of the map that
     # won't need to change dynamically (at least, not unless the
     # entire map is being torn down and recreated).
     leaflet(quakes) %>% addTiles() %>%
       fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
   })
   
   # Incremental changes to the map (in this case, replacing the
   # circles when a new color is chosen) should be performed in
   # an observer. Each independent set of things that can change
   # should be managed in its own observer.
   observe({
     pal <- colorpal()
     
     leafletProxy("map", data = filteredData()) %>%
       clearShapes() %>%
       addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                  fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
       )
   })
   
   # Use a separate observer to recreate the legend as needed.
   observe({
     proxy <- leafletProxy("map", data = quakes)
     
     # Remove any existing legend, and only if the legend is
     # enabled, create a new one.
     proxy %>% clearControls()
     if (input$legend) {
       pal <- colorpal()
       proxy %>% addLegend(position = "bottomright",
                           pal = pal, values = ~mag
       )
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

