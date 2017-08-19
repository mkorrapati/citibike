library(shiny)
library(rCharts)
library(leaflet)
library(RColorBrewer)
library(curl) # make the jsonlite suggested dependency explicit
library(httr)
library(jsonlite)
library(tidyverse)
library(htmltools)
source('global.R')

#cur_station <- stations[stations$name == '12 Ave & W 40 St',]
#weather <- getWeatherData(cur_station$longitude, cur_station$latitude)

getWeatherData <- function(lon, lat, start_date = '2017-08-01', end_date = Sys.Date(), forecast=TRUE) {
  if(forecast) {
    print('Loading weather forecast from weather.gov!')
    req <- httr::GET(paste0('https://api.weather.gov/points/', lat, ',', lon, '/forecast'), #"https://www.ncdc.noaa.gov/cdo-web/api/v2/datasets", 
                     query = list(
                       # datasetid = 'GHCND',
                     ),
                     content_type_json()
                     #httr::add_headers('token' =  ncdc_token)
    )
    json <- httr::content(req, as = "text", encoding = 'UTF-8')
    fromJSON(json)
  }
  else {
    hist_weather <- getWeatherDataFromDb(start_date, end_date)
  }
}

getPredictions <- function(station_name = NULL, date = "2017-08-21", hour = 23) {
  
  station_name
  if(is.null(station_name) || station_name  == '') {
    cur_station <- stations[stations$name == '12 Ave & W 40 St',]
  } else {
    cur_station <- stations[stations$name == station_name,]
  }
  weather <- getWeatherData(cur_station$longitude, cur_station$latitude)
  weather_df <- as.data.frame(weather$properties$periods)
  
  weather_df$startTime <- as.Date.character(weather_df$startTime, format="%Y-%m-%d")
  weather_df$isDaytime <- as.factor(weather_df$isDaytime)
  
  # weather_df <- weather_df %>%
  #   mutate(avg_wind = grep("\b[0-9]{2}\\smph" ,average_wind_speed, value = TRUE))
    
  # pattern <- "\\d{1}(?= mph*$)"
  # pattern <- "d{1}(?=[^[ mph])"
  # m <- regexpr(pattern, myStr)
  # regmatches(myStr, m)
  
  weather_hist <- weather_hist %>%
    select('date', 'precipitation', 'snow_depth', 'snowfall', 'max_temperature', 'min_temperature', 'average_wind_speed')
  
  head(weather_hist)
  
  if(nrow(weather_df) > 0) {
    weather_df <- weather_df %>%
      filter(isDaytime == TRUE | startTime == format(Sys.Date(), format="%Y-%m-%d")) %>%
      filter(isDaytime == TRUE) %>%
      select('startTime', 'temperature', 'windSpeed') %>%
      #mutate(avg_wind = grep("\b\d{2}\smph" ,windSpeed, value = TRUE)) %>%
      mutate(windSpeed = 8) %>%
      #mutate(station_id = cur_station$id) %>%
      #mutate(station_name = cur_station$name) %>%
      mutate(min_temperature = temperature) %>%
      mutate(precipitation = weather_hist[1:n(), 'precipitation']) %>% #Copy Percipitation from hist 8 elements
      mutate(snow_depth = weather_hist[1:n(), 'snow_depth']) %>%
      mutate(snowfall = weather_hist[1:n(), 'snowfall']) %>%
      rename(date = startTime, max_temperature = temperature, average_wind_speed = windSpeed)
    
    #combine both data
    head(weather_df)
    weather_df <- rbind(weather_hist, weather_df)
  } else {
    weather_df <- weather_hist
  }
  
  print('Joining weather_df and weather_hist!')
  if(!is.null(weather_df) && !is.null(weather_hist))  {
    #Predict bikes
    avail_bikes <- predict_bikes(weather_df, station_name, date, hour)
  }
}


# Define server logic required to draw a histogram
function(input, output) {
  num_bikes <- data.frame(matrix(ncol = 10, nrow = 0))
  x <- c('interval', 'name', 'arrive_count', 'end_bike_count_pred', 'depart_count', 
         'removed_count', 'snow_depth', 'max_temperature',
         'new_bike_count', 'precipitation')
  
  colnames(num_bikes) <- x
  
  #num_bikes <- getPredictions()
  
  on.exit(closeDbConnection())
  output$stationOutput <- renderText({
    input$station
  })
  
  output$dateOutput <- renderText({
    as.character.Date(as.POSIXct(input$date_journey, format ='%Y-%m-%d', tz='') + hours(input$slider_hours), format = '%m/%d/%Y %H:%M:%S', tz='EDT')
    
  })
  
  output$tempOutput <- renderText({
    predictions <- startPrediction()
    if (length(predictions) == 0 || nrow(predictions) == 0)
      return(NULL)
    
    paste0(predictions[1,]$max_temperature, ' c')
  })
  
  output$predictions_num <- renderText({
    predictions <- startPrediction()
    if (length(predictions) == 0 || nrow(predictions) == 0)
      return(NULL)
    
    predictions[1,]$end_bike_count_pred
    
  })
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    map <- leaflet(stations) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }
    
    map <- map %>% mapOptions(zoomToLimits = rezoom)
    
    map
  })
  
  observe({
    leafletProxy("map", data = loadPointData()) %>%
      clearShapes() %>%
      addMarkers(
        layerId = paste0("marker", 1:1), 
        label = ~htmltools::HTML(
          sprintf('Name: <span style="font-size:16px;font-weight:bold;color:red">%s</span>
                  <br/>
                  Avail bikes: <span style="font-size:16px;font-weight:bold;color:red">%s</span>', name, as.character(avaial_bike_count))),
        #label = ~cat(paste(name, '\n', 'Avail bikes: ', as.character(avaial_bike_count)), sep='\n'),
        labelOptions = labelOptions(noHide = T, direction = "bottom",
                                    style = list(
                                      "color" = "blue",
                                      "font-family" = "serif",
                                      "font-style" = "italic",
                                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                      "font-size" = "14px",
                                      "border-color" = "rgba(0,0,0,0.5)"
                                    )))
  })
  
  # Locations of vehicles for a particular route
  startPrediction <- reactive({
    date_sel <- input$date_journey
    input$slider_hours # simulate event
    input$station # simulate event
    
    predictions <- loadModelData()
    
    predictions
  })
  
  # Locations of all active vehicles
  loadModelData <- reactive({
    #input$refresh # Refresh if button clicked
    
    # Capture data from input fields
    station_name <- input$station
    date <- input$date_journey
    hour <- input$slider_hours
    
    date_time = as.POSIXct(date, format ='%Y-%m-%d') + hours(hour)
    
    if(is.null(num_bikes)) {
      bike_record_exists <- num_bikes %>%
        filter(interval == as.character.Date(date_time, format = '%Y-%m-%d %H:%M:%S')) %>%
        filter(name == station_name)
    } else {
      bike_record_exists = NULL
    }
    
    #[num_bikes$interval == as.character.Date(date_time, format = '%Y-%m-%d %H:%M:%S'),]
    if(is.null(bike_record_exists) || nrow(bike_record_exists) < 1) {
      if(nrow(num_bikes) > 1) {
        temp_date <- date(max(num_bikes$interval))
        temp_hour <- as.numeric(format(max(num_bikes$interval), '%H'))
      } else {
        temp_date <- date
        temp_hour <- hour
      }
      num_bikes <- getPredictions(station_name, temp_date, temp_hour)
      bike_record_exists <- num_bikes %>%
        filter(interval == as.character.Date(date_time, format = '%Y-%m-%d %H:%M:%S')) %>%
        filter(name == station_name)
      
    }
    #print(bike_record_exists)
    
    bike_record_exists
  })
  
  loadPointData <- reactive({
    input$station
    input$date_journey
    input$slider_hours
    
    cur_station <- stations[stations$name == input$station,]
    
    avail_bikes <- loadModelData()
    avaial_bike_count <- avail_bikes[1,]$end_bike_count_pred
    
    if(!'avaial_bike_count' %in% colnames(cur_station)) {
      cur_station <- cbind(cur_station, avaial_bike_count = c(avaial_bike_count))
      print(paste0('Adding count column to ', cur_station$avaial_bike_count))
    } else {
      cur_station$avaial_bike_count <- avaial_bike_count
      print(paste0('Updating count to ', cur_station$avaial_bike_count))
    }
    print(cur_station)
    cur_station
  })
}
