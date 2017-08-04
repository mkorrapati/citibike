library(h2o)
library(dplyr)
library(RPostgreSQL)

myPostgres<- src_postgres(dbname="nyc-citibike-data",
                          host="dsla.c0l5qodhdfzv.us-east-1.rds.amazonaws.com",#192.168.15.254
                          port=5432,
                          user="murali",
                          password="murali123")

dbListTables(myPostgres$con)

# Imported the file as a table into database using the createTable.sql script

nyc_2010.db<-tbl(src = myPostgres, from = "nyct2010" )

glimpse(nyc_2010.db)

remoteH2O <- h2o.init(ip='34.200.247.252', startH2O=FALSE, port = 54321, nthreads = -1) #  Connection successful!

# Push the data into h2o
model_data.db.h2o<-as.h2o(nyc_2010.db)



#Shutdown h2o
h2o.shutdown()
