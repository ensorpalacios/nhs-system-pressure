#' Load packages
#' 
#' Load all packages needed for the project
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-05-26

# Load packages ----------------------------------------------------------------
# Full package
library(conflicted)
# library(data.table)
library(tibble)
library(dplyr)
library(purrr)
library(fable)
library(tsibble)
library(feasts)
library(smooth)
library(ggplot2)
library(patchwork)
library(knitr)
library(kableExtra)

# Single functions (using import package)
import::from(here, here)
import::from(magrittr, "%>%")
import::from(tidyr, pivot_longer, pivot_wider, replace_na, drop_na)
import::from(forcats, fct_rev)
import::from(distributional, dist_normal, variance)
import::from(stringr, str_sub, str_extract)
import::from(imputeTS, ggplot_na_distribution, ggplot_na_imputations)
import::from(scales, viridis_pal)
import::from(slider, slide_dbl)
import::from(readxl, read_excel)
import::from(imputeTS, na_ma, na_kalman)
import::from(randomForest, randomForest)
import::from(stringr, str_glue)
import::from(mltools, one_hot)
import::from(data.table, as.data.table)
import::from(astsa, LagReg, pre.white)
import::from(readr, parse_number)
import::from(data.table, ":=")
import::from(xgboost, xgb.train, xgb.DMatrix, getinfo)
import::from(Matrix, sparse.model.matrix)


# Resolve conflicts (using conflicted package)
conflicts_prefer(
  dplyr::filter,
  fabletools::accuracy, # used in computing metrics for fable fc
)