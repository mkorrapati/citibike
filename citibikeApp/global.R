#Some shared functions to get data
library(RPostgreSQL)
library(lubridate)
library(h2o)

localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '500m', nthreads = -1) 
drv <- dbDriver("PostgreSQL")

myPostgres<- dbConnect(drv = drv, 
                          dbname="nyc-citibike-data",
                          host="dsla.c0l5qodhdfzv.us-east-1.rds.amazonaws.com",#192.168.15.254
                          port=5432,
                          user="murali",
                          password="murali123")

getLastKnownEndBikeCountFromDb <- function(station_ids) {
  bike_end_count = dbGetQuery(myPostgres, 
                              'select * from top_20_station_july_2017'
  )
}
weather_hist <- dbGetQuery(myPostgres, 
                     paste0('SELECT * FROM central_park_weather_observations w
                            WHERE w.date >= \'2017-08-01\' AND w.date <= \'', 
                            Sys.Date() - 1,  
                            '\' ORDER BY w.date')
)
known_predictions <- dbGetQuery(myPostgres, 
                           paste0('SELECT * FROM predictions_since_july_2017 order by interval desc')
)
closeDbConnection <- function() {
  dbDisconnect(myPostgres)
  killDbConnections()
  #dbUnloadDriver(drv)
  h2o.shutdown()
}

killDbConnections <- function () {
  
  all_cons <- dbListConnections(drv = drv)
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}
stations = dbGetQuery(myPostgres, 
                      "SELECT DISTINCT ON (sta.name) sta.id, sta.name, sta.longitude, sta.latitude FROM stations sta
                            WHERE sta.id in (
                              SELECT end_station_id FROM
                              monthly_station_aggregates mthly
                              WHERE mthly.month >= '2017-01-01'
                              ORDER by total_drop_offs DESC LIMIT 50
                            )
                          ORDER BY sta.name LIMIT 5")

end_bike_count_list <- getLastKnownEndBikeCountFromDb(stations$id)

#closeDbConnection()

#Load models from disk
#Devide predictors(independant variables) and responses(dependant variables)
response <- c("depart_count", "new_bike_count", "arrive_count", "removed_count")

predictors <- c('day_of_week', 'hour_of_day', 'holiday', 'precipitation', 'snow_depth', 'max_temperature',
                'average_wind_speed', 'name', 'beginning_count')

models_disk <- list()

predict_bikes <- function(weather_df_local, station_name, date = "2017-08-12", hour) {
  if(is.null(station_name) || station_name  == '') {
    station_names <- stations$name
  } else {
    station_names = c(station_name)
  }
  
  last_known_pred_date <- list()
  #Drop row.names column
  if(length(known_predictions) > 0) {
    known_predictions <- known_predictions %>%
      select(-row.names)
  
    #Load last known(precalculated) prediction from db so we don't have to generate them again.
    last_known_pred_date <- known_predictions %>%
      filter(name %in% station_names) %>%
      group_by(name) %>%
      summarize(max_date = max(interval))
  }
  
  pred_start_date = "2017-07-31 23:00:00+00"
  if(length(last_known_pred_date) > 0) {
    #Some stations might have few predictions in db. Get the lowest date that has less predictions
    pred_start_date_row <- last_known_pred_date %>%
      summarize(min_date = min(max_date))
    
    if(nrow(pred_start_date_row) > 0) {
      pred_start_date <- pred_start_date_row$min_date
    }
  }
  if(is.infinite(pred_start_date)) {
    pred_start_date = "2017-07-31 23:00:00+00"
  }
  pred_start_date <- strftime(pred_start_date, tz='UTC', usetz = TRUE)
  print(as.POSIXct(pred_start_date, tz = 'UTC') + hours(1))
  print(as.POSIXct(date, format ='%Y-%m-%d', tz='UTC') + hours(hour))
  
  to_date <- as.POSIXct(date, format ='%Y-%m-%d', tz='UTC') + hours(hour)
  ## Time series in hours
  #hours_from_july <- seq.POSIXt(as.POSIXct("2017-07-31 23:00:00"), as.POSIXct(date) + hours(hour), by = "hour")
  hours_from_july <- list()
  if(pred_start_date < to_date) {
    hours_from_july <- seq.POSIXt(as.POSIXct(pred_start_date, tz = 'UTC') + hours(1), to_date, by = "hour")
  }
  
  n_hours <- length(hours_from_july)
  
  #Prepare data
  predict_var_df <- data.frame(matrix(ncol = 10, nrow = 0))
  x <- c('interval', 'name', 'day_of_week', 'hour_of_day', 'holiday', 
         'precipitation', 'snow_depth', 'max_temperature',
         'average_wind_speed', 'beginning_count')
  colnames(predict_var_df) <- x
  
  if(length(hours_from_july) > 0) {
    for (i in 1:length(hours_from_july)) {
      weather_i = weather_df_local[date(weather_df_local$date) == date(hours_from_july[i]),]
      #weather_i = weather_df_local[date(weather_df_local$date) == date(hours_from_july[1]),]
      
      #predict_var_df[i,'name'] = station_name
      predict_var_df[i,'interval'] = as.character.Date(hours_from_july[i], format = '%Y-%m-%d %H:%M:%S')
      predict_var_df[i,'day_of_week'] = factor(weekdays(hours_from_july[i]), 
                                               levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                                          'Thursday', 'Friday', 'Saturday'),
                                               labels = c(0, 1, 2, 3, 4, 5, 6))
      predict_var_df[i,'hour_of_day'] = factor(as.POSIXlt(hours_from_july[i])$hour)
      predict_var_df[i,'holiday'] = FALSE
      predict_var_df[i,'precipitation'] = weather_i$precipitation
      predict_var_df[i,'snow_depth'] = weather_i$snow_depth
      predict_var_df[i,'max_temperature'] = weather_i$max_temperature
      predict_var_df[i,'average_wind_speed'] = weather_i$average_wind_speed 
      predict_var_df[i,'beginning_count'] = 0
    }
  }
  predict_var_df <- predict_var_df %>%
    arrange(interval) %>%
    mutate(day_of_week = as.factor(day_of_week)) %>%
    mutate(hour_of_day = as.factor(hour_of_day)) %>%
    mutate(interval = as.POSIXct(interval, format = '%Y-%m-%d %H:%M:%S'))
  
  
  #apply Models
  new_predictions <- applyModels(predict_var_df, station_names)
  
  rbind(known_predictions, new_predictions)
}

applyModels <- function(predict_var_df, station_names) {
  
  predictors <- predictors[! predictors %in% c('C1', 'interval', 'ending_bike_count')]
  
  finalResults <- data.frame(matrix(ncol = 10, nrow = 0))
  x <- c('interval', 'name', 'arrive_count', 'end_bike_count_pred', 'depart_count', 
         'removed_count', 'snow_depth', 'max_temperature',
         'new_bike_count', 'precipitation')
  
  colnames(finalResults) <- x
  
  for (y in response){
    # the model will be saved as "./folder_for_myDRF/myDRF"
    models_disk[[y]] <- h2o.loadModel(path = paste0("models/dl_model_faster_", y))
  }
  
  
  for (station_name in station_names) {
    
    #get station id
    station_id = stations[stations$name == station_name,]$id
    #last known ending bike count
    last_ending_bike_count = end_bike_count_list[end_bike_count_list$station_id == station_id,]$ending_bike_count
    local_last_bike_count <- last_ending_bike_count
    
    nrows = nrow(predict_var_df)
    if(nrows > 0) {
      for(i in 1:nrows) {
        row <- predict_var_df[i,]
        print(paste0('Row Num ', i, ' out of ', nrows, ' for station: ', station_name))
        # do stuff with row
        row$beginning_count <- local_last_bike_count
        
        test_results = data.frame("name" = station_name,
                                  "interval" = row[['interval']],
                                  "'snow_depth" = row[['snow_depth']],
                                  "max_temperature" = row[['max_temperature']],
                                  "precipitation" = row[['precipitation']])
        
        row$interval <- as.character.Date(row$interval)
        h2o_df <- as.h2o(row, destination_frame = "appdata" )
        
        
        ## outsample 
        ## Converting H2O format into data frame 
        for (y in response){
          df_yhat_test <- as.data.frame(h2o.predict(models_disk[[y]], h2o_df[,predictors]) ) 
          predH2O<- df_yhat_test$predict 
          #results[[y]] <- data.frame('pred' = as.integer(predH2O), 'real' = as.data.frame(bikecount.hex.test[[y]]))
          test_results = cbind(test_results, y = as.integer(predH2O))
        }
        
        bikecount.df.test = as.data.frame(h2o_df)
        colnames(test_results) = c('name','interval','snow_depth','max_temperature', 'precipitation',  response)
        test_results = cbind(test_results, beginning_count = bikecount.df.test$beginning_count)
        
        test_results <- test_results %>%
          mutate(beginning_count = beginning_count) %>%
          mutate(arrive_count = arrive_count) %>%
          mutate(depart_count = depart_count) %>%
          mutate(removed_count = removed_count) %>%
          mutate(new_bike_count = new_bike_count)
        
        test_results = cbind(test_results, end_bike_count_pred = 
                               max(test_results$beginning_count + 
                              test_results$arrive_count - test_results$depart_count -
                             test_results$removed_count + test_results$new_bike_count,0))
        
        local_last_bike_count <- test_results$end_bike_count_pred
        
        finalResults <- rbind(finalResults, test_results)
      }
    }
  }
  
  if(nrow(finalResults) > 0) {
    dbWriteTable(myPostgres, 'predictions_since_july_2017', finalResults, append = TRUE)
  }
  finalResults
}

