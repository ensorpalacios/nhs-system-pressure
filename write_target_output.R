library(targets)
library(stringr)
library(RMySQL)
library(dplyr)
library(distributional)

tar_make(callr_function = NULL)

model_out <- readRDS("target/data/output/model_out_flat.RDS")
historic_data <- readRDS("target/data/output/historic_data.RDS")

# compute mean/sd
model_out <-model_out %>%
  mutate(occ_mean = mean(occ), occ_var = variance(occ)) %>%
  select(-occ)

historic_data <- historic_data %>%
  filter(index >= (min(model_out$index, na.rm = TRUE)-lubridate::dweeks(2))) %>%
  filter(site != "<aggregated>")


local <- TRUE 


if (local) {
  conn <- dbConnect(RSQLite::SQLite(), "data/local_db/local_shiny_dev.sqlite")
  message("Connected to: Local SQLite")
} else {
# Write to MYSQL
# Connection details
host <- Sys.getenv("DB_HOST")
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_CRED")
port <- 3306 # Default MySQL port (change if needed)

# Create the connection
conn <- dbConnect(dbDriver("MySQL"),
                  dbname = dbname,
                  host = host,
                  port = 3306,
                  user = user,
                  password=password)
  message("Connected to: Hosted SQLite database")
}

dbWriteTable(conn, "nhs_bed_pressure", value = model_out, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "nhs_bed_pressure_historic", value = historic_data, overwrite = TRUE, row.names = FALSE)

