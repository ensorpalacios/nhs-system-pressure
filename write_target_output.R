library(targets)
library(stringr)
library(RMariaDB)
library(dplyr)
library(dbplyr)
library(distributional)

tar_make(callr_function = NULL)

model_out <- readRDS("target/data/output/model_out_flat.RDS")
historic_data <- readRDS("target/data/output/historic_data.RDS")

# compute mean/sd
historic_data <- historic_data %>%
  filter(site != "<aggregated>") |> 
  mutate(index = as.character(index))

model_out <- model_out %>%
  mutate(index = as.character(index), occ_mean = mean(occ), occ_var = variance(occ)) %>%
  select(-occ)


local <- FALSE 


if (local) {
  conn <- dbConnect(RMariaDB::MariaDB(), here("target/data/local_db/local_shiny_dev.sqlite"))
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
conn <- dbConnect(
  RMariaDB::MariaDB(),   # Specify the driver here
  dbname   = dbname,
  host     = host,
  port     = 3306,
  user     = user,
  password = password
)
  message("Connected to: Hosted SQLite database")
}

dbWriteTable(conn, "nhs_bed_pressure", value = model_out, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "nhs_bed_pressure_historic", value = historic_data, overwrite = TRUE, row.names = FALSE)

DBI::dbDisconnect(conn)
