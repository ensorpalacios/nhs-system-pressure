library(RMySQL)
library(dplyr)
# Write to MYSQL
# Connection details
host <- Sys.getenv("DB_HOST")
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_CRED")

# Create the connection
conn <- dbConnect(dbDriver("MySQL"),
                  dbname = dbname,
                  host = host,
                  port = 3306,
                  user = user,
                  password=password)


model_out <- dbGetQuery(conn, "select * from nhs_bed_pressure") %>%
  mutate(index = lubridate::ymd(index))

historic_data <- dbGetQuery(conn, "select * from nhs_bed_pressure_historic") %>%
  mutate(index = lubridate::ymd(index))