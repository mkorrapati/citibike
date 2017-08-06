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
gpclibPermit()

citi_hex = "#146EFF"
orange_hex = "#FF5910"

myPostgres<- src_postgres(dbname="nyc-citibike-data",
                          host="dsla.c0l5qodhdfzv.us-east-1.rds.amazonaws.com",#192.168.15.254
                          port=5432,
                          user="murali",
                          password="murali123")

dbListTables(myPostgres$con)

# Imported the file as a table into database using the createTable.sql script

#daily<-tbl(src = myPostgres, from = "aggregate_data_with_weather" )
daily = dbGetQuery(myPostgres$con, "SELECT date, dow, trips FROM aggregate_data_with_weather ORDER BY date")

glimpse(daily)

daily = daily %>%
  select('date', 'dow', 'trips') %>%
  mutate(dow = factor(dow, labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
         monthly = rollsum(trips, k = 28, na.pad = TRUE, align = "right"))

getwd()
png(filename = "graphs/monthly_total_trips.png", width = 640, height = 480)
ggplot(data = daily, aes(x = date, y = monthly)) +
  geom_line(size = 1, color = citi_hex) +
  scale_x_date("") +
  scale_y_continuous("Citi Bike trips, trailing 28 days\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("NYC Monthly Citi Bike Trips", "Based on Citi Bike system data") + 
  theme_wsj()

dev.off()
ggsave("monthly_total_trips.pdf")

by_dow = daily %>%
  filter(date >= "2015-09-01") %>%
  group_by(dow) %>%
  summarize(avg = mean(trips))

png(filename = "graphs/trips_by_dow.png", width = 640, height = 420)
ggplot(data = by_dow, aes(x = dow, y = avg)) +
  geom_bar(stat = "identity", fill = citi_hex) +
  scale_x_discrete("") +
  scale_y_continuous("Avg daily Citi Bike trips\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("NYC Citi Bike Trips by Day of Week", "Based on Citi Bike system data 9/2015–11/2015")

by_hour = dbGetQuery(myPostgres$con,"SELECT * FROM aggregate_data_hourly ORDER BY weekday, hour")
by_hour = by_hour %>%
  mutate(timestamp_for_x_axis = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC"),
         weekday = factor(weekday, levels = c(TRUE, FALSE), labels = c("Weekdays", "Weekends")),
         avg = trips / number_of_days)

png(filename = "graphs/trips_by_hour.png", width = 640, height = 720)
ggplot(data = by_hour, aes(x = timestamp_for_x_axis, y = avg)) +
  geom_bar(stat = "identity", fill = citi_hex) +
  scale_x_datetime("", labels = date_format("%l %p")) +
  scale_y_continuous("Avg hourly Citi Bike trips\n", labels = comma) +
  ggtitle("NYC Citi Bike Trips by Hour of Day", "Based on Citi Bike system data 9/2015–11/2015") +
  facet_wrap(~weekday, ncol = 1) +
  theme_economist(base_size = 20) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1)),
        panel.margin = unit(1.2, "lines"))
dev.off()

# Weekday more active rush hour start stations - am and pm
rush_hour_data = dbGetQuery(myPostgres$con,"SELECT * FROM weekday_rush_hour_data")

miles_per_meter = 1 / (5280 * 12 * 2.54 / 100)
hours_per_second = 1 / (60 * 60)

rush_hour_data = rush_hour_data %>%
  mutate(age_bucket = cut(age, breaks = c(0, 22, 25, 30, 35, 40, 45, 50, 60, 100), right = FALSE))

# Drop the small number of records where gender/age are unknown,
# and there are a bunch of suspcious 19th century birthdays, so drop those too
rush_hour_data = rush_hour_data %>%
  filter(user_type == "Subscriber",
         gender != "unknown",
         !is.na(age),
         age <= 95)

bucketed_data = rush_hour_data %>%
  group_by(gender, age_bucket) %>%
  summarize(count = n(),
            mean_age = mean(age))

png(filename = "graphs/mean_by_age_gender.png", width = 640, height = 720)
ggplot(data = filter(bucketed_data, count > 1000),
       aes(x = mean_age, y = duration, color = gender)) +
  geom_line(size = 1) +
  scale_x_continuous("Age") +
  scale_y_continuous("Trip Duration\n") +
  scale_color_discrete("") +
  ggtitle("NYC Citi Bike Speed by Age, Gender, and Trip Distance",
                      "7/2013–11/2015, Citi Bike subscribers, weekday rush hour (7–10AM, 5–8PM)") +
  theme_tws(base_size = 18.5) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1)),
        panel.margin = unit(1.2, "lines"))
dev.off()

# number of bikes and stations in service
active_bikes = dbGetQuery(myPostgres$con,"SELECT * FROM monthly_active_bikes ORDER BY date")
active_stations = dbGetQuery(myPostgres$con,"SELECT * FROM monthly_active_stations ORDER BY date")

png(filename = "graphs/bikes_used.png", width = 640, height = 480)
ggplot(data = filter(active_bikes, date <= "2017-07-31"), aes(x = date, y = bikes)) +
  geom_line(size = 1, color = citi_hex) +
  scale_x_date("") +
  scale_y_continuous("Number of Citi Bikes Used\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("Unique Bikes Used Per Day", "Program expansion in August 2015 added nearly 2,000 bikes") +
  theme_wsj(base_size = 18)
dev.off()

png(filename = "graphs/stations_in_use.png", width = 640, height = 480)
ggplot(data = filter(active_stations, date <= "2017-07-31"), aes(x = date, y = stations)) +
  geom_line(size = 1, color = citi_hex) +
  scale_x_date("") +
  scale_y_continuous("Citi Bike stations in use\n", labels = comma) +
  expand_limits(y = 0) +
  ggtitle("Citi Bike Stations in Use", "Program expansion in August 2015 added nearly 150 stations") +
  theme_wsj(base_size = 18)
dev.off()

# bike transports
transports_monthly = dbGetQuery(myPostgres$con,
                              "SELECT
                               month,
                               SUM(transported_to_other_station) / SUM(total_drop_offs) frac
                               FROM monthly_station_aggregates
                               GROUP BY month
                               ORDER BY month
                               ")

png(filename = "graphs/monthly_station_transports.png", width = 640, height = 480)
ggplot(data = transports_monthly, aes(x = month, y = frac)) +
  geom_line(size = 1, color = citi_hex) +
  scale_x_date("") +
  scale_y_continuous("% of bikes transported\n", labels = percent) +
  expand_limits(y = 0) +
  title_with_subtitle("NYC Citi Bike Station-to-Station Transports",
                      "% of bikes that are manually moved to a different station after being dropped off") +
  theme_tws(base_size = 18) +
  theme(legend.position = "bottom")
add_credits()
dev.off()

nta_transports_hourly = dbGetQuery(myPostgres$con,"
                              SELECT
                              ntaname,
                              hour,
                              SUM(transported_to_other_station) / SUM(total_drop_offs) frac,
                              SUM(total_drop_offs) total
                              FROM hourly_station_aggregates a
                              INNER JOIN stations s
                              ON a.end_station_id = s.id
                              GROUP BY ntaname, hour
                              HAVING SUM(total_drop_offs) > 100
                              ORDER BY ntaname, hour
                              ")
nta_transports_hourly = nta_transports_hourly %>%
  mutate(timestamp_for_x_axis = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC"))

ntas = c("East Village", "Fort Greene", "Midtown-Midtown South")
for (n in ntas) {
  p = ggplot(data = filter(nta_transports_hourly, ntaname == n),
             aes(x = timestamp_for_x_axis, y = frac)) +
    geom_bar(stat = "identity", fill = citi_hex) +
    scale_x_datetime("\nDrop off hour", labels = date_format("%l %p")) +
    scale_y_continuous("% of bikes transported\n", labels = percent) +
    title_with_subtitle(n, "% of Citi Bikes that are manually moved to a different station after being dropped off") +
    theme_tws(base_size = 18)
  
  png(filename = paste0("graphs/transports_", to_slug(n), ".png"), width = 640, height = 400)
  print(p)
  add_credits()
  dev.off()
}

privacy = dbGetQuery(myPostgres$con, "
                SELECT
                gender,
                age,
                SUM(CASE WHEN count = 1 THEN 1.0 ELSE 0 END) / SUM(count) AS uniq_frac,
                SUM(count) AS total
                FROM anonymous_analysis_hourly
                WHERE age BETWEEN 16 AND 80
                GROUP BY gender, age
                ORDER BY gender, age
                ")
privacy = privacy %>%
  mutate(gender = factor(gender, levels = c(2, 1), labels = c("female", "male"))) %>%
  filter(total > 5000)

png(filename = "graphs/uniquely_identifiable.png", width = 640, height = 420)
ggplot(data = privacy, aes(x = age, y = uniq_frac, color = gender)) +
  geom_line(size = 1) +
  scale_x_continuous("Rider age") +
  scale_y_continuous("% of trips uniquely identifiable\n", labels = percent) +
  expand_limits(y = c(0.7, 1)) +
  scale_color_discrete("") +
  title_with_subtitle("Uniquely Identifiable Citi Bike Trips", "Given age, gender, subscriber status, trip's start point and time rounded to nearest hr") +
  theme_tws(base_size = 18) +
  theme(legend.position = "bottom")
add_credits()
dev.off()

remoteH2O <- h2o.init(ip='34.200.247.252', startH2O=FALSE, port = 54321, nthreads = -1) #  Connection successful!

# Push the data into h2o
model_data.db.h2o<-as.h2o(nyc_2010.db)



#Shutdown h2o
h2o.shutdown()
