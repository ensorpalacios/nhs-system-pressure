library(DBI)
library(tidyverse)
library(dbplyr)
library(lubridate)


con <- DBI::dbConnect(odbc::odbc(), "xsw")

report_start <- ymd('2022-01-01')
report_end <- ymd('2025-11-01')

ecds_tbl <- tbl(con,
               in_catalog(catalog = "Analyst_SQL_Area",
                          schema = "dbo",
                          table = "tbl_BNSSG_Datasets_UrgentCare_Daily")
)

metrics <- c('347334',
'347347',
'347348',
'346199',
'346203',
'346200',
'346183',
'346186',
'347351',
'347331',
'346098',
'346085',
'346113',
'346114',
'346117',
'347318',
'347357',
'346170',
'346209')

ecds_tbl %>%
  filter(METRIC_ID %in% metrics, between(Report_Date, report_start, report_end)) %>%
  collect() %>%
  mutate(METRIC_NAME = recode(
    METRIC_NAME,
    !!!c(
      # "Number of Discharges",
      "General & Acute Beds - Total G&A escalation beds open" = "Escalation beds open",
      "Of total G&A beds open, number occupied" = "Bed occupancy",
      "Total G&A escalation beds open" = "Escalation beds open",
      # "Number of Admissions",
      "Beds occupied by Long-stay patients 21+ days? beds occupied by patients with a length of stay of 21 or more days" = "Beds with 21+ days LOS",
      "Beds occupied by Long-stay patients 21+ daysâ€“ beds occupied by patients with a length of stay of 21 or more days" = "Beds with 21+ days LOS",
      "Beds occupied by Long-stay patients 21+ days– beds occupied by patients with a length of stay of 21 or more days" = "Beds with 21+ days LOS",
      "Beds occupied by Long-Stay Patients 21+ days" = "Beds with 21+ days LOS",
      "A&E Performance - Number of A&E Attendances - Type 1 - Paediatrics"  = "A&E attends - paediatrics",
      "Number of A&E Attendances - Type 1 - Paediatrics" = "A&E attends - paediatrics",
      "General & Acute Beds - Of total G&A beds open, number occupied" = "Bed occupancy",
      "Of total G&A beds open number occupied" = "Bed occupancy",
      "Total G & A core bed stock open"="Core stock open", 
      "Total G&A core bed stock open"="Core stock open", 
      "General & Acute Beds - Total G&A core bed stock open"="Core stock open"
    )
  )) %>%
  rename_with(.fn = str_to_lower)
