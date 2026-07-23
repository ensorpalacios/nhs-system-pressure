library(RMariaDB)
library(dplyr)
library(lubridate)
local <- FALSE 

if (local) {
  conn <- dbConnect(RSQLite::SQLite(), here("target/data/local_db/local_shiny_dev.sqlite"))
  message("Connected to: Local SQLite")
} else {
# Write to MYSQL
# Connection details
host <- Sys.getenv("DB_HOST")
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_CRED")
#port <- 3306 # Default MySQL port (change if needed)

# Create the connection
conn <- dbConnect(RMariaDB::MariaDB(),
                  dbname = dbname,
                  host = host,
                  port = 3306,
                  user = user,
                  password=password)
  message("Connected to: Hosted SQLite database")
}

model_out <- dbGetQuery(conn, "select * from nhs_bed_pressure") %>%
  mutate(index = lubridate::ymd(index)) %>%
  mutate(index = index) %>%
  rename(
    risk_day = risk_day0.9,
    risk_w = risk_w0.9,
    risk_ws = risk_ws0.9,
    thr = `thr-0.9`
    ) %>%
  select(-c(
    risk_day0.85,
    risk_day0.95,
    risk_w0.85,
    risk_w0.95,
    risk_ws0.85,
    risk_ws0.95,
    `thr-0.85`,
    `thr-0.95`
    ))

historic_data <- dbGetQuery(conn, "select * from nhs_bed_pressure_historic") %>%
  mutate(index = lubridate::ymd(index)) %>%
mutate(index = index)