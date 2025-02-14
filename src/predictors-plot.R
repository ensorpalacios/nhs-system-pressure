#' Plot predictors data
#'
#' Generate plot to describe predictors data and their relationship with bed occupancy
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles and Practice
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-08

# Shebang ---------------------------------------------------------------------
# !/usr/loca/bin/Rscript

# Import libraries ------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(patchwork)
# library(ggrain)
# library(fable)
# library(feasts)
library(tsibble)
# library(kable)
library(knitr)
library(magick)
library(kableExtra)

# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
bed_occ <- readRDS(file = data_path)
bed_occ <- bed_occ$frontier
sites <- c("BRI", "Southmead")

# Data in long format
bed_occ_l <- bed_occ |> pivot_longer(
  cols = c(bed_occ, adm, dis, bed_escal),
  names_to = "var"
) |>
  mutate(
    var = factor(var, levels = c("bed_occ", "adm", "dis", "bed_escal"))
)

# Plot bed_occ and predictors -------------------------------------------------
plot_data <- bed_occ_l |>
  as.data.frame() |> 
  ggplot(aes(x = index, y = value, colour = site, group = interaction(var, site))) +
  geom_line() +
  facet_wrap(
    vars(var), 
    ncol = 1,
    scales = "free"
  )

# Plot cross-correlation functions --------------------------------------------
# Similar to function in descriptive-plot.R
alpha_= 0.05

# Compute ccf
var_ccf = c("adm", "dis", "bed_escal")
tbl_ccf = map(var_ccf, \(y) {
  tmp_ccf = bed_occ |> 
    CCF(bed_occ, !!as.symbol(y)) |>
    mutate(var = y) |>
    update_tsibble(key = c(site, var))
}) |> bind_rows()

# Confidence interval
ci_lim = qnorm((1 + (1 - alpha_)) /2) / sqrt(nrow(bed_occ) / 2)

# Generate plot (! positive values means var_ccf leads bed_occ)
plt_ccf = tbl_ccf |>
  ggplot(aes(x = lag, y = ccf, group = var)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = ci_lim), linetype = 2, colour = 'blue') +
  geom_hline(aes(yintercept = -ci_lim), linetype = 2, colour = 'blue') +
  labs(x = "lag (days)") +
  facet_grid(rows = vars(var)) +
  xlim(0, NA)

# ccf(bed_occ |> filter(site=="BRI") |> pull(bed_occ),
#   bed_occ |> filter(site=="BRI") |> pull(adm))
#
# # Suggests adm and dis are higly coupled (possibly seasonal effect overly strong)
# bed_occ |> CCF(adm, dis) |> autoplot()


# Save plots ------------------------------------------------------------------
save_path <- here("output/plots/predictors/")

walk(provider, \(prov) {
  tmp_path = paste0(save_path, prov)
  if (!file.exists(tmp_path)) {
    dir.create(tmp_path, recursive = TRUE)
  }
})

plot_data |> ggsave(
        filename = paste0(save_path, "plot_var.eps"),
        width = 35, 
        height = 20, 
        units = "cm", 
        device = cairo_ps)

plt_ccf |> ggsave(
        filename = paste0(save_path, "plot_ccf.eps"),
        width = 35, 
        height = 20, 
        units = "cm", 
        device = cairo_ps)

  walk(plot_forecast[[prov]], \(x) {
    tmp_file = paste0(
      save_path, 
      prov, 
      "/",
      sub(".*-", "", x$labels$title)
    )
    x |>
      ggsave(
        filename = tmp_file,
        width = 35, 
        height = 20, 
        units = "cm", 
        device = cairo_ps)
  })
})

