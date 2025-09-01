#' Model colours
#' 
#' Define colour mapping for different models.
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-01

# Import packages --------------------------------------------------------------
source(here("src/packages.R"))


# Define colour mapping --------------------------------------------------------
# Alternative colour scales
# show_col(rainbow(12))
# show_col(hue_pal()(12))
# show_col(viridis_pal(option="turbo")(12))

col_models = viridis_pal(option = "turbo")(17) # Colour mapping
names(col_models) = 
  c(
    "arima",
    "arima_dad_l",
    "arima_dad_rec",
    "var_ad",
    "var_ad2",
    "var_h",
    "nn",
    "es",
    "rf",
    "rf_int",
    "xgb",
    "tslm",
    "snaive",
    "baseline_min",
    "crps",
    "equal",
    "wilker"
  ) %>% as.factor()


