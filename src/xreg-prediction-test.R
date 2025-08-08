#' Test xreg model
#'
#' Compare different models used for predicting regressors, including mean,
#' naive, snaive, arima, ets models; include also pulled model (fc average)

#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles 
#' and Practice.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-07-14

# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")


# Load data --------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
ts_occ <- readRDS(file = data_path)
sites <- ts_occ$site |> unique()



# Select relevant variables ----------------------------------------------------
ts_occ <- 
  ts_occ %>%
  select(
    -(ts_occ %>% names %>% grep("_m", .)), # original data with missing values
    -adm, -dis,
    -escal, -core,
    -ad_diff, -ad_diff2, -ad_diff3 # ignore these in this script!
    )



# Lag/split dataset ------------------------------------------------------------
horizon = 7
ts_occ_lag <- lag_fun(ts_occ, .lag = horizon) # lag data


split_data_tt <- # Train/test set
  split_tt(ts_occ_lag)


initial <- "16 weeks" 
assess <- "1 weeks"
skip <- "6 weeks"
split_data_cv <- # Cv train/validation sets
  split_cv(split_data_tt, initial, assess, skip)

splits <- split_data_cv$split %>% unique() # save cv splits names
idx_start_test <- split_data_cv$type %>% grep("test", .) %>% head(1)



# Predict test exogenous -------------------------------------------------------
# Exclude occ_other (as in fit-models-short.R)
xpredict_method = c("tslm", "naive", "snaive", "arima", "ets", "pull")
tbl_data <- 
  map(xpredict_method, \(.xmod) {
    data_xpredict <- 
      xpredict_fun(
        split_data_cv, 
        c("occ", "ad_diff"), 
        idx_start_test, 
        .xmod
      ) %>% 
      mutate(
        xpredict_mod = .xmod,
      )
  }) %>% 
  list_rbind() %>% 
  mutate(
    across(
      c(contains(c("occ", "ad_diff")), -contains("lag")),
      ~ {
        obs = split_data_cv %>% pull(cur_column())
        abs(obs - .x)
      },
      .names = "{.col}_error"
    )
  )



# Plot prediction errors -------------------------------------------------------
plt_err <- 
  tbl_data %>%
  filter(type == "test", !is_aggregated(site)) %>% 
  select(xpredict_mod, site, contains("error"), -contains("other")) %>% 
  pivot_longer(-c(xpredict_mod, site)) %>% 
  ggplot(aes(x = name, y = value, colour = xpredict_mod)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(vars(site)) +
  scale_x_discrete(guide = guide_axis(angle = 45))



# Save plot --------------------------------------------------------------------
save_path = here("output/plots/xreg_predictions/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

plt_err %>% 
  ggsave(file = paste0(save_path, "xreg_err.eps"), width = 11, height = 7)


# for (x in tbl_data$split %>% unique()) {
#   x11()
#   ok =
#     tbl_data %>% filter(split==x, type == "test", site == "BRI",
#                         xpredict_mod == "pull") %>%
#     select(type, index, contains("ad_diff_f"), - contains("other"), - split, -site) %>%
#     mutate(predict = "yes") %>%
#     bind_rows(.,
#               split_data_cv%>%
#                 filter(split==x, site == "BRI", type == "test") %>%
#                 ungroup() %>%
#                 select(type, index, contains("ad_diff_f"), - contains("other"), - split, -site) %>%
#                 mutate(predict = "no")
#     ) %>%
#     pivot_longer(-c(type, index, predict)) %>%
#     filter(type == "test") %>%
#     ggplot(aes(x=index, y=value, colour = predict)) + geom_line() +
#     facet_wrap(vars(name))
#   print(ok)
# }