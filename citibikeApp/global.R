#Some shared functions to get data
library(RPostgreSQL)
getStationData<- function()
{
  myPostgres<- src_postgres(dbname="nyc-citibike-data",
                            host="dsla.c0l5qodhdfzv.us-east-1.rds.amazonaws.com",#192.168.15.254
                            port=5432,
                            user="murali",
                            password="murali123")
  
  stations = dbGetQuery(myPostgres$con, 
                        "SELECT DISTINCT ON (sta.name) sta.name, sta.longitude, sta.latitude FROM stations sta
                            WHERE sta.id in (
                              SELECT end_station_id FROM
                              monthly_station_aggregates mthly
                              WHERE mthly.month >= '2017-01-01'
                              ORDER by total_drop_offs DESC LIMIT 50
                            )
                          ORDER BY sta.name LIMIT 20")
  
  station_names <- stations %>%
    select('name')
}