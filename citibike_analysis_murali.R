# monthly_station_aggregates
library(h2o)
library(dplyr)
library(RPostgreSQL)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(scales)
library(grid)
library(ggmap)
library(rgdal)
library(maptools)
library(readr)
library(minpack.lm)
library(zoo)
library(lubridate)
gpclibPermit()

citi_hex = "#146EFF"
orange_hex = "#FF5910"
training_start_date = '2016-01-01'
training_end_date = '2016-02-1'
testing_start_date = '2017-01-01'

myPostgres<- src_postgres(dbname="nyc-citibike-data",
                          host="dsla.c0l5qodhdfzv.us-east-1.rds.amazonaws.com",#192.168.15.254
                          port=5432,
                          user="murali",
                          password="murali123")

dbListTables(myPostgres$con)

stations = dbGetQuery(myPostgres$con, 
                      "select bcs.station_id, ss.name, 
                            ss.latitude, ss.longitude, ss.dup_count,
                            min(date(bcs.interval)) as serv_start_date, 
                            max(date(bcs.interval)) as serv_end_date 
                      from bike_count_at_stations bcs, stations s 
                      inner join 
                        (select s.id, s.name, s.latitude, s.longitude, sub.dup_count 
                          from stations s 
                          inner join  
                            (select name, count(*) as dup_count 
                              from stations group by name) sub 
                          on s.name = sub.name and sub.dup_count > 1
                        ) ss
                      on s.id = ss.id
                      where s.id = bcs.station_id 
                      group by bcs.station_id, ss.name, ss.latitude, ss.longitude, ss.dup_count
                      order by ss.name, serv_start_date")


head(stations, 10)

#There are duplicate stations with different ids. We need to fix them to get complete history
station_id_map <- tribble(
  ~end_station_id,  ~station_id_new,
  317,          15447,
  18350,        34831,
  19261,        38517,
  294,          25955,
  14113,        25955,
  27719,        40924,
  33523,        40924,
  69,           16506,
  20136,        38461, 
  25,           21691,
  17930,        19279,
  16,           18858,
  251,          38736,
  185,          27883,
  20,           29552,
  5564,         19305,
  17508,        18389, 
  18352,        20192,
  62,           27461,
  45,           7950,
  22595,        41678,
  28338,        41678,
  25455,        29398,
  17483,        24845,
  23892,        24845,
  77,           24600,
  83,           34511,
  51,           28950,
  30631,        31854,
  159,          26912,
  103,          24043,
  17490,        42503,
  22613,        34885,
  28860,        42477,
  17512,        25954,
  268,          39988
)

#laod monthly data
monthly = dbGetQuery(myPostgres$con, 
                     "SELECT mthly.*, sta.name as station_name FROM monthly_station_aggregates mthly
                        INNER JOIN stations sta
                          ON mthly.end_station_id = sta.id
                      ORDER BY month")

by_station = monthly %>%
  select('month', 'end_station_id', 'station_name', 'total_drop_offs') %>%
  filter((month >= "2016-01-01" & month < "2016-08-01") | month >= "2017-01-01") %>%
  left_join(station_id_map) %>% 
  # copy unchanged items
  mutate(station_id_new = if_else(is.na(station_id_new), end_station_id, station_id_new)) %>% 
  #filter(station_id_new == 34511) %>%   #For testing
  mutate(year = factor(year(month))) %>%
  group_by(year, station_id_new,station_name) %>%
  summarise(total  = sum(total_drop_offs)) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(desc(total), .by_group = TRUE) %>%
  top_n(20) 
  
ggplot(data = by_station, 
       aes(x = station_name, y = total, fill=year)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  scale_x_discrete("By station") +
  scale_y_continuous("", labels = comma) +
  ggtitle("Compare monthly trips in 2016 and 2017", "Based on Citi Bike system data") +
  xlab('Trips') + 
  theme_wsj(base_size = 8) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.margin = unit(0, "lines"),
        legend.text = element_text(size = rel(1.1)),
        legend.key.height = unit(1.7, "lines"),
        legend.key.width = unit(2, "lines"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  
  by_month = monthly %>%
  select('month', 'end_station_id', 'station_name', 'total_drop_offs') %>%
  filter(month >= "2016-01-01") %>%
  group_by(end_station_id,station_name, month) %>%
  mutate(month_for_x_axis = month(month, label = TRUE)) %>%
  summarize(total = sum(total_drop_offs)) %>%
  arrange(desc(total)) 

top_5_stations = c(10162, 184, 272, 205, 213)

ggplot(data = filter(by_month, end_station_id %in% top_5_stations), 
       aes(x = month, y = total, group = station_name, color = station_name)) + 
  geom_line(size = 1) +
  scale_x_date("Monthly Breakdown") +
  scale_y_continuous("Citi Bike trips at top stations, monthly\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("NYC Monthly Citi Bike Trips", "Based on Citi Bike system data") + 
  theme_wsj(base_size = 8) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.margin = unit(0, "lines"),
        legend.text = element_text(size = rel(1.1)),
        legend.key.height = unit(1.7, "lines"),
        legend.key.width = unit(2, "lines"))

# Station #213(West ST & Chambers ST) seems to pick up lot of traffic recently
#laod data for this station
sta_213 = dbGetQuery(myPostgres$con, 
                     "SELECT
                        EXTRACT(DOW FROM start_time) BETWEEN 1 AND 5 AS weekday,
                        ss.name as start_name,
                        es.name as end_name,
                        date(start_time) AS date,
                        start_station_id,
                        end_station_id,
                        COUNT(*) AS trips
                      FROM trips t 
                      LEFT JOIN stations ss 
                        ON ss.id = t.start_station_id
                      LEFT JOIN stations es
                        ON es.id = t.end_station_id
                      WHERE date(start_time) BETWEEN to_date('2016-01-01','YYYY-MM-DD') 
                        AND to_date('2017-07-31','YYYY-MM-DD')
                        AND end_station_id = 213
                      GROUP BY weekday, date, start_station_id, end_station_id, ss.name, es.name;")

sta_213 %>%
  select('date', 'weekday', 'end_station_id', 'start_station_id',  'start_name', 'end_name', 'trips')

by_sta_213 = sta_213 %>%
  mutate(month = floor_date(date, unit='month')) %>%
  group_by(month, weekday) %>%
  summarize(total = sum(trips)) %>%
  arrange(desc(total))

ggplot(data = by_sta_213, 
       aes(x = month, y = total, group = weekday, color = weekday)) + 
  geom_line(size = 1) +
  scale_x_date("Monthly Breakdown") + 
  scale_y_continuous("Trips, monthly\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("Monthly Citi Bike Trips to W St & Chambers St", "Based on Citi Bike system data") + 
  theme_wsj(base_size = 12) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.margin = unit(0, "lines"),
        legend.text = element_text(size = rel(1.1)),
        legend.key.height = unit(1.7, "lines"),
        legend.key.width = unit(2, "lines"))

by_sta_213_weekday = sta_213 %>%
  filter(weekday == TRUE) %>%
  mutate(month = floor_date(date, unit='month')) %>%
  mutate(dayOfWeek = wday(date, label = TRUE)) %>%
  group_by(dayOfWeek, month) %>%
  summarize(total = sum(trips)) %>%
  arrange(desc(total))

ggplot(data = by_sta_213_weekday, 
       aes(x = month, y = total, group = as.factor(dayOfWeek), color = as.factor(dayOfWeek))) + 
  geom_line(size = 1) +
  scale_x_date("Weekly Breakdown") + 
  scale_y_continuous("Trips, weeklys\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("Weekly Citi Bike Trips to W St & Chambers St", "Based on Citi Bike system data") + 
  theme_wsj(base_size = 12) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.margin = unit(0, "lines"),
        legend.text = element_text(size = rel(1.1)),
        legend.key.height = unit(1.7, "lines"),
        legend.key.width = unit(2, "lines"))

by_sta_213_start_sta = sta_213 %>%
  mutate(month = floor_date(date, unit='month')) %>%
  mutate(dayOfWeek = wday(date, label = TRUE)) %>%
  group_by(start_station_id, start_name, month) %>%
  summarize(total = sum(trips)) %>%
  arrange(desc(total))

top_10_start_sta_to_213 = c(38736, 39987, 41246, 39988, 38735, 41247, 39984, 39982, 38734, 41160)
################
#station_id |           name           |         min         
#------------+--------------------------+---------------------
#  38734 | E 53 St & 3 Ave          | 2017-04-01 09:00:00
#  38735 | E 44 St & 2 Ave          | 2017-04-19 17:00:00
#  38736 | E 16 St & Irving Pl      | 2017-04-19 12:00:00
#  39982 | W 55 St & 6 Ave          | 2017-05-12 10:00:00
#  39984 | Murray St & Greenwich St | 2017-05-18 12:00:00
#  39987 | W 37 St & Broadway       | 2017-05-04 13:00:00
#  39988 | W 45 St & 6 Ave          | 2017-05-26 14:00:00
#  41160 | E 82 St & East End Ave   | 2017-06-15 16:00:00
#  41246 | W 15 St & 10 Ave         | 2017-06-27 16:00:00
#  41247 | 6 Ave & Spring St        | 2017-06-29 16:00:00

ggplot(data = filter(by_sta_213_start_sta, start_station_id %in% top_10_start_sta_to_213), 
       aes(x = month, y = total, group = as.factor(start_name), color = as.factor(start_name))) + 
  geom_line(size = 1) +
  scale_x_date("Date") + 
  scale_y_continuous("Trips\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("Citi Bike Trips to W St & Chambers St in 2017", "Based on Citi Bike system data") + 
  theme_wsj(base_size = 8) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.margin = unit(0, "lines"),
        legend.text = element_text(size = rel(1.1)),
        legend.key.height = unit(1.7, "lines"),
        legend.key.width = unit(2, "lines"))

sta_213_tues_wed = dbGetQuery(myPostgres$con, 
                     "SELECT
                        EXTRACT(HOUR FROM start_time) AS hour,
                        date(start_time) AS date,
                        EXTRACT(DOW FROM start_time) AS weekday,
                        start_station_id,
                        end_station_id,
                        COUNT(*) AS trips
                      FROM trips
                      WHERE date(start_time) >= '2017-01-01'
                        AND date(start_time) < '2017-06-01'
                        AND EXTRACT(DOW FROM start_time) IN (2, 3)
                        AND end_station_id = 213
                      GROUP BY hour, date, weekday, start_station_id, end_station_id;")

sta_213_tues_wed %>%
  select('hour', 'date', 'weekday', 'end_station_id', 'start_station_id', 'trips')

by_sta_213_tues_wed = sta_213_tues_wed %>%
  filter(hour %in% c(7, 8, 9, 17, 18, 19)) %>%
  group_by(date, hour) %>%
  summarize(total = sum(trips)) %>%
  arrange(desc(total))

ggplot(data = by_sta_213_tues_wed, 
       aes(x = date, y = total, group = as.factor(hour), color = as.factor(hour))) + 
  geom_line(size = 1) +
  scale_x_date("Hourly Breakdown") + 
  scale_y_continuous("Trips, hourly\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("Hourly Citi Bike Trips to W St & Chambers St", "Based on Citi Bike system data") + 
  theme_wsj(base_size = 8) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.margin = unit(0, "lines"),
        legend.text = element_text(size = rel(1.1)),
        legend.key.height = unit(0.5, "lines"),
        legend.key.width = unit(2, "lines"))

#Build model to predict number of bikes available at each station at any given time
bike_count = dbGetQuery(myPostgres$con, sprintf(
                                            'select bcs.*, s.name, 
                                                  w.precipitation, w.snow_depth, w.max_temperature, w.average_wind_speed,
                                                  s.latitude, s.longitude 
                                            from bike_count_at_stations bcs
                                            left join central_park_weather_observations w
                                              on w.date = date(bcs.interval)
                                            inner join stations s
                                              on s.id = bcs.station_id
                                            where
                                              date(interval) >= \'%s\' 
                                              and date(interval) < \'%s\'
                                            order by s.name, interval', training_start_date, training_end_date)
                                          )

bike_count <- bike_count %>%
  select('interval', 'day_of_week', 'holiday', 'name', 
         'precipitation', 'snow_depth', 'max_temperature', 'average_wind_speed',
         'depart_count', 'new_bike_count','arrive_count', 'removed_count', 
         'ending_bike_count')

lag_1 <- function(x) lag(x)

by_bike_count <- bike_count %>%
  mutate_at(funs(as.factor), .vars = c("holiday", "name")) %>%
  mutate(day_of_week = factor(weekdays(interval)))  %>%
  arrange(name, interval) %>%
  mutate(hour_of_day = factor(as.POSIXlt(interval)$hour))  %>%
  group_by(name) %>%
  mutate(beginning_count = lag_1(ending_bike_count))  %>%
  filter(!is.na(beginning_count)) %>%
  #select(-interval)  %>%
  select("day_of_week", "interval","hour_of_day", "holiday", 'precipitation', 'snow_depth', 'max_temperature', 
         'average_wind_speed',"depart_count", "new_bike_count", 
          "arrive_count", "removed_count", "name", "beginning_count", 
          "ending_bike_count")  

Ttrain = nrow(by_bike_count[format(by_bike_count$interval, '%Y-%m-%d') < testing_start_date,])
Total_rows <- nrow(by_bike_count)

setwd() #set current working directory
by_bike_count %>%
  write.csv(file = 'bike_count_data.csv') 
  
#temp <- by_bike_count %>%
#  filter(name == 'Lafayette St & E 8 St')

##Apply h2o deep learning
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '4g', nthreads = -1) 
#remoteH2O <- h2o.init(ip='34.200.247.252', startH2O=FALSE, port = 54321, nthreads = -1) #  Connection successful!

# Push the data into h2o
bikecount.hex = h2o.importFile(path = "bike_count_data.csv", destination_frame = "bikecount.hex")
class(bikecount.hex)
summary(bikecount.hex)

#RM ending_bike_count
#bikecount.hex = bikecount.hex[,-1]

# Create a training set from the 1st dataset in the split
bikecount.hex.train <- bikecount.hex[1:Ttrain,] # before 2017

# Create a testing set from the 2nd dataset in the split
bikecount.hex.test <- bikecount.hex[Ttrain:Total_rows,] # 2017

#Devide predictors(independant variables) and responses(dependant variables)
response <- c("depart_count", "new_bike_count", 
              "arrive_count", "removed_count")
predictors <- setdiff(names(bikecount.hex), response)

#Remove unnecessary predictors
predictors <- predictors[! predictors %in% c('C1', 'interval', 'ending_bike_count')]

#h2o deep net 
models <- list()


for (y in response){
  
  models[[y]] <- h2o.deeplearning(model_id="dl_model_faster", 
                                  training_frame=bikecount.hex.train,
                                  x=predictors,
                                  y=y,
                                  hidden=c(32,32,32),                  ## small network, runs faster
                                  epochs=1000000,                      ## hopefully converges earlier...
                                  score_validation_samples=10000,      ## sample the validation dataset (faster)
                                  stopping_rounds=2,
                                  stopping_metric="MSE",               ## could be "MSE","logloss","r2"
                                  stopping_tolerance=0.01,
                                  variable_importances=T 
                            )
  
}
lapply(models, summary)

test_results = data.frame("name" = as.data.frame(bikecount.hex.test)[['name']])

## outsample 
## Converting H2O format into data frame 
for (y in response){
  df_yhat_test <- as.data.frame(h2o.predict(models[[y]], bikecount.hex.test[,predictors]) ) 
  predH2O<- df_yhat_test$predict 
  #results[[y]] <- data.frame('pred' = as.integer(predH2O), 'real' = as.data.frame(bikecount.hex.test[[y]]))
  test_results = cbind(test_results, y = as.integer(predH2O))
}

bikecount.df.test = as.data.frame(bikecount.hex.test)
colnames(test_results) = c('name', response)
test_results = cbind(test_results, beginning_count = bikecount.df.test$beginning_count)

test_results = cbind(test_results, end_bike_count_pred = 
                                          test_results$beginning_count
                                          +test_results$arrive_count - test_results$depart_count 
                                          -test_results$removed_count + test_results$new_bike_count)
test_results = cbind(test_results, end_bike_count = bikecount.df.test$ending_bike_count)

#update beginning count based on ending count in previous hour
test_results$beginning_count = lag_1(test_results$end_bike_count_pred)
#Fill in NA in first beginning count from known values in test
test_results[1, 'beginning_count'] = bikecount.df.test[1, 'beginning_count']

#recalc ending count baased on new beginning count
test_results$end_bike_count_pred = test_results$beginning_count+
                                      test_results$arrive_count - test_results$depart_count -
                                      test_results$removed_count + test_results$new_bike_count

plot(test_results$end_bike_count_pred,test_results$end_bike_count,type='l',col="black", main="H2O Deep Learning")

test_error = sum(test_results$end_bike_count_pred == test_results$ending_bike_count) / nrow(test_results)

test_error

#Save models
for (y in response){
  # Save the DRF model to disk
  # the model will be saved as "./folder_for_myDRF/myDRF"
  h2o.saveModel(models[[y]], path = "models") # define your path here
}

#Load models from disk
models_disk <- list()
for (y in response){
  # the model will be saved as "./folder_for_myDRF/myDRF"
  models_disk[[y]] <- h2o.loadModel(path = paste0("models/dl_model_faster_", y))
}

#COmpare training set results
train_results = list()
## Converting H2O format into data frame 
for (y in response){
  df_yhat_train <- as.data.frame(h2o.predict(models[[y]], bikecount.hex.train[,predictors]) ) 
  predH2O<- df_yhat_train$predict 
  train_results[[y]] <- data.frame('pred' = as.integer(predH2O), 'real' = as.data.frame(bikecount.hex.train[[y]]))
  plot(train_results[[y]]$pred, train_results[[y]][[y]]) 
  #fit2<-lm(eval(y) ~ pred,data=results[[y]]) 
  #summary(fit2)
}

pdf("hist_h2o.pdf") 
dev.off() 