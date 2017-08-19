library(shinydashboard)
library(leaflet)
library(shiny)
#source('./global.R')
library(RColorBrewer)

header <- dashboardHeader(
  title = "NYC Citibike availability at stations"
)
body <- dashboardBody(
  fluidRow(
    # Application title
    titlePanel("NYC Citibike availability at stations"),
    
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    column(width = 3,
           box(width = NULL, solidHeader = TRUE,
           
               # value is always yyyy-mm-dd, even if the display format is different
               dateInput("date_journey", "Date:", value = Sys.Date(), max = Sys.Date() + 10, format = "mm/dd/yy"),
               
               sliderInput("slider_hours", "Time:", min=0, max=23, value=7, step = 1),
               
               selectInput("station", "Station:", stations$name)
               
               #actionButton("zoomButton", "Zoom to fit")
          )
    ),
    column(width = 9,
           # adding the new div tag to the sidebar            
           box(width = NULL, status = "warning",
              tags$table(class = "table",
                         tags$thead(tags$tr(
                           tags$th("Date"),
                           tags$th("Station"),
                           tags$th("Temp"),
                           tags$th("# Bikes avail")
                         )),
                         tags$tbody(
                           tags$tr(
                             tags$td(textOutput("dateOutput")),
                             tags$td(textOutput("stationOutput")),
                             tags$td(textOutput("tempOutput")),
                             tags$td(textOutput("predictions_num"))
                           )
                         )
              )
           ),
           box(width = NULL, status = "warning",
               leafletOutput("map")
               
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
