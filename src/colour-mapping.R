#' Model colours
#' 
#' Define colour mapping for different models.
#' 
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-05-01

# Import packages --------------------------------------------------------------
source(here("src/packages.R"))


# Define colour mapping --------------------------------------------------------
# Alternative colour scales
# show_col(rainbow(12))
# show_col(hue_pal()(12))
# show_col(viridis_pal(option="turbo")(12))

col_models = viridis_pal(option = "turbo")(19) # Colour mapping
# names(col_models) = c("arima", 
#                       "arima_dae",
#                       "arima_dae_c",
#                       "arima_dae_c_locf",
#                       "arima_dae_f",
#                       "arima_dae_f_locf",
#                       "es_ae_c",
#                       "rf",
#                       # "es_ae_f",
#                       # "rf_dae_c",
#                       "mean",
#                       "naive",
#                       "snaive",
#                       "baseline_min",
#                       "copd_min")
names(col_models) = 
  c(
    "arima",
    "arima_dad",
    "arima_dad_l",
    "arima_dad_nof",
    "arima_dado",
    "arima_dado_l",
    "locf_arima_dad_l",
    "locf_arima_dad_rec",
    "locf_arima_dad_l_nof_rec",
    "locf_arima_dad",
    "locf_arima_dad_nof",
    "locf_arima_dado_l",
    "locf_arima_dado",
    "locf_es_ado_f",
    "rf_dado_f",
    "mean",
    "naive",
    "snaive",
    "baseline_min"
  )