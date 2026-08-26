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

# core stock data
con <- switch(.Platform$OS.type,
              windows = dbConnect(odbc::odbc(), "xsw"),
              unix = {
                dbConnect(
                  odbc::odbc(),
                  .connection_string = readr::read_lines("/root/sql/sql_connect_string_linux_sql18")
                )
              })

ucd_tbl <- tbl(
  con,
  in_catalog(
    catalog = "Analyst_SQL_Area",
    schema = "dbo",
    table = "tbl_BNSSG_Datasets_UrgentCare_Daily"
  )
)


core_stock <- ucd_tbl %>%
  filter(METRIC_ID %in% c('346199', '347347', '346113')) %>%
  filter(!is.na(Value)) %>%
  filter(Report_Date == max(Report_Date, na.rm = TRUE), .by = METRIC_ID) %>%
  collect() %>%
  mutate(Provider = recode(Provider, "NBT" = "Southmead")) %>%
  select(Provider, Value) %>% 
  deframe()

dbDisconnect(con)