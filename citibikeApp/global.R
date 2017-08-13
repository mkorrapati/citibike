#Some shared functions to get data
library(RPostgreSQL)
library(lubridate)
library(h2o)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '4g', nthreads = -1) 

myPostgres<- src_postgres(dbname="nyc-citibike-data",
                          host="dsla.c0l5qodhdfzv.us-east-1.rds.amazonaws.com",#192.168.15.254
                          port=5432,
                          user="murali",
                          password="murali123")

stations = dbGetQuery(myPostgres$con, 
                      "SELECT DISTINCT ON (sta.name) sta.id, sta.name, sta.longitude, sta.latitude FROM stations sta
                            WHERE sta.id in (
                              SELECT end_station_id FROM
                              monthly_station_aggregates mthly
                              WHERE mthly.month >= '2017-01-01'
                              ORDER by total_drop_offs DESC LIMIT 50
                            )
                          ORDER BY sta.name LIMIT 20")

end_bike_count_list <- getLastKnownEndBikeCountFromDb(stations$id)

#closeDbConnection()

#Load models from disk
#Devide predictors(independant variables) and responses(dependant variables)
response <- c("depart_count", "new_bike_count", "arrive_count", "removed_count")

predictors <- c('day_of_week', 'hour_of_day', 'holiday', 'precipitation', 'snow_depth', 'max_temperature',
                'average_wind_speed', 'name', 'beginning_count')

models_disk <- list()
for (y in response){
  # the model will be saved as "./folder_for_myDRF/myDRF"
  models_disk[[y]] <- h2o.loadModel(path = paste0("models/dl_model_faster_", y))
}

predict_bikes <- function(weather_df, station_name, date = "2017-08-12", hour) {
  ## Time series in hours
  #hours_from_july <- seq.POSIXt(as.POSIXct("2017-07-31 23:00:00"), as.POSIXct(date) + hours(hour), by = "hour")
  hours_from_july <- seq.POSIXt(as.POSIXct("2017-08-01 00:00:00"), as.POSIXct(date, format ='%Y-%m-%d') + hours(hour), by = "hour")
  
  n_hours <- length(hours_from_july)
  
  
  #Prepare data
  predict_var_df <- data.frame(matrix(ncol = 10, nrow = 0))
  x <- c('interval', 'name', 'day_of_week', 'hour_of_day', 'holiday', 
         'precipitation', 'snow_depth', 'max_temperature',
         'average_wind_speed', 'beginning_count')
  colnames(predict_var_df) <- x
  
  for (i in seq_along(hours_from_july)){
    weather_i = weather_df[date(weather_df$date) == date(hours_from_july[i]),]
    
    predict_var_df[i,'name'] = station_name
    predict_var_df[i,'interval'] = as.character.Date(hours_from_july[i], format = '%Y-%m-%d %H:%M:%S')
    predict_var_df[i,'day_of_week'] = factor(weekdays(hours_from_july[i]))
    predict_var_df[i,'hour_of_day'] = factor(as.POSIXlt(hours_from_july[i])$hour)
    predict_var_df[i,'holiday'] = FALSE
    predict_var_df[i,'precipitation'] = weather_i$precipitation
    predict_var_df[i,'snow_depth'] = weather_i$snow_depth
    predict_var_df[i,'max_temperature'] = weather_i$max_temperature
    predict_var_df[i,'average_wind_speed'] = weather_i$average_wind_speed 
    predict_var_df[i,'beginning_count'] = 0
  }
  
  predict_var_df <- predict_var_df %>%
    arrange(interval) %>%
    mutate(day_of_week = as.factor(day_of_week)) %>%
    mutate(hour_of_day = as.factor(hour_of_day)) %>%
    mutate(interval = as.POSIXct(interval, format = '%Y-%m-%d %H:%M:%S'))
  
  
  #get station id
  station_id = stations[stations$name == station_name,]$id
  #last known ending bike count
  last_ending_bike_count = end_bike_count_list[end_bike_count_list$station_id==station_id,]$ending_bike_count

  #apply Models
  applyModels(predict_var_df, last_ending_bike_count)
}

applyModels <- function(predict_var_df, last_ending_bike_count) {
  
  predictors <- predictors[! predictors %in% c('C1', 'interval', 'ending_bike_count')]
  local_last_bike_count <- last_ending_bike_count
  
  finalResults <- data.frame(matrix(ncol = 10, nrow = 0))
  x <- c('interval', 'name', 'arrive_count', 'end_bike_count_pred', 'depart_count', 
         'removed_count', 'snow_depth', 'max_temperature',
         'new_bike_count', 'precipitation')
  
  colnames(finalResults) <- x
  
  for(i in 1:nrow(predict_var_df)) {
    row <- predict_var_df[i,]
    # do stuff with row
    row$beginning_count <- local_last_bike_count
    
    test_results = data.frame("name" = row[['name']],
                              "interval" = row[['interval']],
                              "'snow_depth" = row[['snow_depth']],
                              "max_temperature" = row[['max_temperature']],
                              "precipitation" = row[['precipitation']])
    
    row$interval <- as.character.Date(row$interval)
    h2o_df <- as.h2o(row, destination_frame = "appdata" )
    
    
    ## outsample 
    ## Converting H2O format into data frame 
    for (y in response){
      df_yhat_test <- as.data.frame(h2o.predict(models[[y]], h2o_df[,predictors]) ) 
      predH2O<- df_yhat_test$predict 
      #results[[y]] <- data.frame('pred' = as.integer(predH2O), 'real' = as.data.frame(bikecount.hex.test[[y]]))
      test_results = cbind(test_results, y = as.integer(predH2O))
    }
    
    bikecount.df.test = as.data.frame(h2o_df)
    colnames(test_results) = c('name','interval','snow_depth','max_temperature', 'precipitation',  response)
    test_results = cbind(test_results, beginning_count = bikecount.df.test$beginning_count)
    
    test_results = cbind(test_results, end_bike_count_pred = 
                           test_results$beginning_count + 
                          test_results$arrive_count - test_results$depart_count -
                         test_results$removed_count + test_results$new_bike_count)
    
    local_last_bike_count <- test_results$end_bike_count_pred
    
    finalResults <- rbind(finalResults, test_results)
  }
  
  finalResults
}
getStationData<- function()
{
  station_names <- stations %>%
    select('name','longitude', 'latitude')
  
  as.data.frame(station_names)
}
getWeatherDataFromDb <- function(start_date, end_date) {
  weather = dbGetQuery(myPostgres$con, 
                        paste0('SELECT * FROM central_park_weather_observations w
                            WHERE w.date >= \'', start_date,
                              '\' AND w.date <= \'', end_date,  
                          '\' ORDER BY w.date')
                      )
}
getLastKnownEndBikeCountFromDb <- function(station_ids) {
  bike_end_count = dbGetQuery(myPostgres$con, 
                       sprintf('select bcs.ending_bike_count, bcs.station_id  
                          from bike_count_at_stations bcs
                          inner join 
                            (select max(interval) as interval, station_id
                              from bike_count_at_stations
                              where date(interval) = \'2017-07-31\'
                                and station_id in (%s)
                              group by station_id
                            ) sub
                            on bcs.station_id = sub.station_id
                              and sub.interval = bcs.interval', 
                          paste(unlist(station_ids), collapse = ','))
  )
}
closeDbConnection <- function() {
  dbDisconnect(myPostgres$con)
}
