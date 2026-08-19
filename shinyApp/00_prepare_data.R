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
  filter(type == "forecast") %>%
  mutate(index = lubridate::ymd(index))

historic_data <- dbGetQuery(conn, "select * from nhs_bed_pressure_historic") %>%
  mutate(index = lubridate::ymd(index))

# Suggested per-site threshold (90th percentile of last ~6 months occupancy),
# used only to pre-fill the user-editable threshold inputs in app.R; the
# user can override it, and risk is then computed client-side against
# whatever value ends up in the inputs (see shiny-functions.R: compute_risk()).
thr_default <- compute_threshold_default(historic_data)