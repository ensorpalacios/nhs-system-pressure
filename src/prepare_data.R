#' Preprocess data
#'
#' Load, clean and recode data.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-07

# Shebang ---------------------------------------------------------------------
# !/usr/loca/bin/Rscript


# Install and import libraries ------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(readxl)
library(patchwork)
library(plotly)


# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/raw/")

# Admissions/discharges - 2023
df1 <- readRDS(file = paste0(data_path, "dat.RDS"))

# Admissions/discharges, acute bed occupancy, escalation beds - 2024
df2 <- read_excel(
                  paste0(
                         data_path,
                         "2024-09-01-to-2024-11-24-acute-occupancy.xlsx"
                         ),
                  sheet = 2
)

# Compute bed occupancy -------------------------------------------------------
# Set up time series to evaluate occupancy at
tseq <- seq.POSIXt(
  from = as.POSIXct("2023-01-01"),
  to = as.POSIXct("2023-12-13"), 
  by = "day"
)

# Use only BRI and Southmead hospitals
# sites <- unique(df1_r[["site"]]) |> set_names() 
sites <- c("BRI" = "RA701", "Southmead" = "RVJ01")
bed_occ <- do.call("bind_rows", map(tseq, function(x) {
  map_int(sites, function(y) {
    df1 |>
      filter(site == y) |>
      filter(arr <= x & dep >= x) |>
      nrow()
  })
}))

# As df
bed_occ <- data.frame(dates = tseq, bed_occ)

# Save df ---------------------------------------------------------------------
save_path <- here(str_glue('data/processed/'))
here()
if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

saveRDS(bed_occ, file = paste0(save_path, 'bed_occupancy.rds'))



plot1 <- bed_occ |>
  pivot_longer(cols=-dates,names_to="Hospital",values_to="val") %>%
  mutate(dates=as.POSIXct(dates)) %>%
  ggplot(aes(x=dates,y=val,colour=Hospital)) +
  geom_line() +
  # labs(title="All bed occupancy - wards and ED") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
plot2 <- bed_occ %>%
  pivot_longer(cols=-dates,names_to="Hospital",values_to="val") %>%
  group_by(Hospital) %>%
  mutate(val=max(val)-val) %>%
  mutate(dates=as.POSIXct(dates)) %>%
  ggplot(aes(x=dates,y=val,colour=Hospital)) +
  geom_line() +
  # labs(title="Spare bed occupancy (max = highest in period)") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())


# ggplotly(plot1 + plot2)
options(browser = '/usr/bin/google-chrome')
subplot(plot1, plot2, nrows = 2, shareX = TRUE) |>
  layout(annotations = list(
                            list(x = 0.2,  
                                 y = 0.95,  
                                 text = "All bed occupancy - wards and ED",  
                                 xref = "paper",  
                                 yref = "paper",  
                                 xanchor = "center",  
                                 yanchor = "bottom",  
                                 showarrow = FALSE 
                                 ),  
                            list( 
                                 x = 0.2,  
                                 y = 0.44,  
                                 text = "Spare bed occupancy (max = highest val recorded)",  
                                 xref = "paper",  
                                 yref = "paper",  
                                 xanchor = "center",  
                                 yanchor = "bottom",  
                                 showarrow = FALSE
                                 )
                            )
)

