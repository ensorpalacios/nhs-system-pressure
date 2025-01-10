#' Plot bed occupancy
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-08

# Shebang ---------------------------------------------------------------------
# !/usr/loca/bin/Rscript

# Import libraries ------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(readxl)
library(patchwork)
library(plotly)

# Load occupancy data ---------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
bed_occ <- readRDS(file = data_path)

# Generate plots --------------------------------------------------------------
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


multiplot <- plot1 + plot2 + plot_layout(nrow = 2)

# Save plot -------------------------------------------------------------------
save_path <- here("output/plots/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}
ggsave(paste0(save_path, "bed-occupancy.png"))

# Create interactive web-based plots ------------------------------------------
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

