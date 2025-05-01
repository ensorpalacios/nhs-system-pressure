#' Model colours
#' 
#' Define colour mapping for different models.
#' 
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-05-01

# Import packages --------------------------------------------------------------
library(scales)


# Define colour mapping --------------------------------------------------------
# Alternative colour scales
# show_col(rainbow(12))
# show_col(hue_pal()(12))
# show_col(viridis_pal(option="turbo")(12))

col_models = viridis_pal(option = "turbo")(12) # Colour mapping
names(col_models) = c("arima", 
                      "arima_d",
                      "arima_dad",
                      "arima_de",
                      "arima_dade",
                      "es_e",
                      "mean",
                      "naive",
                      "snaive",
                      "naive",
                      "baseline_min",
                      "wilker_min")