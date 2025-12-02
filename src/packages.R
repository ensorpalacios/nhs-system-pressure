#' Load packages
#' 
#' Run only once
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-10-11

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
import::from(ncdf4, nc_open, ncvar_add)

library(ncdf4)

# Resolve conflicts (using conflicted package)
conflicts_prefer(
  dplyr::filter,
  fabletools::accuracy # used in computing metrics for fable fc
)