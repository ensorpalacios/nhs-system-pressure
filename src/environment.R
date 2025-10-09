#' Prepare environment
#' 
#' First script to run for loading libraries
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-26

# Load packages ----------------------------------------------------------------
# Full packages
library(conflicted)
library(tibble)
library(dplyr)
library(data.table)
library(purrr)
library(fable)
library(tsibble)
library(feasts)
library(smooth)
library(ggplot2)
library(patchwork)
library(knitr)
library(kableExtra)
library(knitr)
library(gt)
library(FKF)
library(bsts)


# Single functions (using import package)
import::from(here, here)
import::from(magrittr, "%>%")
import::from(tidyr, pivot_longer, pivot_wider, replace_na, drop_na)
import::from(forcats, fct_rev)
import::from(distributional, dist_normal, variance, dist_mixture, cdf)
import::from(stringr, str_sub, str_extract)
import::from(imputeTS, ggplot_na_distribution, ggplot_na_imputations)
import::from(scales, viridis_pal)
import::from(slider, slide_dbl)
import::from(readxl, read_excel)
import::from(imputeTS, na_ma, na_kalman)
import::from(randomForest, randomForest)
import::from(stringr, str_glue)
import::from(mltools, one_hot)
# import::from(data.table, as.data.table, ":=", data.table, rbindlist, copy)
import::from(astsa, LagReg, pre.white)
import::from(readr, parse_number)
import::from(xgboost, xgb.train, xgb.DMatrix, getinfo)
import::from(Matrix, sparse.model.matrix)
import::from(pagedown, chrome_print)


# Resolve conflicts (using conflicted package)
conflicts_prefer(
  dplyr::filter,
  fabletools::accuracy # used in computing metrics for fable fc
)



# Load custom functions --------------------------------------------------------
source("src/functions.R")



# Load global parameters -------------------------------------------------------
# Analyse trend
occ_with_trend = FALSE # if FALSE forecast trend separately

# Lag/split data parameters
horizon = 7 # Forecast horizon & lag
len_test = "5 months" # Train/test
initial <- "16 weeks" # Cv split - training set
assess <- "1 weeks" # Cv split - validation set
skip <- "9 days" # Cv split - separation between training sets

# Threshold for "dangerous" occupancy
threshold_prob = 0.9 # percentile of total observed occupancy

# Colour mapping
col_models = viridis_pal(option = "turbo")(28) # Colour mapping
names(col_models) =
  c(
    "arima",
    "arima_dad_l",
    "arima_dadp_l",
    "arima_dadpt_l",
    "arima_dadpl_l",
    "arima_dadplt_l",
    "arima_dad_rec",
    "arima_dadp_rec",
    "arima_dadpl_rec",
    "arima_dadplt_rec",
    "var_ad",
    "var_ad2",
    "var_paed",
    "var_los",
    "var_h",
    "nn",
    "es",
    "rf",
    "rf_int",
    "rf_int_not",
    "xgb",
    "xgb_not",
    "tslm",
    "snaive",
    "baseline_min",
    "crps",
    "equal",
    "wilker"
  )