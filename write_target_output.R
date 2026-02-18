library(targets)
library(stringr)
library(RMySQL)
library(dplyr)
library(distributional)

tar_make()

model_out <- readRDS("target/data/output/model_out_flat.RDS")

# compute mean/sd
model_out <-model_out %>%
  mutate(occ_mean = mean(occ), occ_var = variance(occ)) %>%
  select(-occ)


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


# delete old data
query_delete <- str_c("DELETE FROM nhs_bed_pressure")
DBI::dbGetQuery(conn, query_delete)
dbWriteTable(conn, "nhs_bed_pressure", value = model_out, overwrite = TRUE)
