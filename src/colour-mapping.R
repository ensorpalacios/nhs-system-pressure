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

col_models = viridis_pal(option = "turbo")(16) # Colour mapping
names(col_models) = 
  c(
    "arima",
    "arima_dad_l",
    "arima_dado_l",
    "var_ad",
    "var_ad2",
    "var_BRI",
    "xpred_arima_dad_l",
    "xpred_arima_dad_rec",
    "xpred_es",
    "xpred_rf",
    "xpred_rf_int",
    "xpred_xgb",
    "tslm",
    "naive",
    "snaive",
    "baseline_min"
  )

col_models <- # Same color var_BRI and var_Southmead
  col_models %>% c(c("var_Southmead" = col_models[["var_BRI"]]))


