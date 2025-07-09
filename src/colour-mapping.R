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

col_models = viridis_pal(option = "turbo")(28) # Colour mapping
names(col_models) = 
  c(
    "arima",
    "arima_dad",
    "arima_dad_l",
    "arima_dad_nof",
    "arima_dado",
    "arima_dado_l",
    "var_ad",
    "var_ad_nof",
    "var_ad2",
    "var_ad2_nof",
    "var_ad3",
    "var_ad3_nof",
    "var_BRI",
    "var_Southmead",
    "locf_arima_dad_l",
    "locf_arima_dad_rec",
    "locf_arima_dad_l_nof_rec",
    "locf_arima_dad",
    "locf_arima_dad_nof",
    "locf_arima_dado_l",
    "locf_arima_dado",
    "locf_es_ado_f",
    "rf_dado_f",
    "rf_dado_f_int",
    "mean",
    "naive",
    "snaive",
    "baseline_min"
  )
