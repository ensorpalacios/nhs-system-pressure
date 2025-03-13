#!/usr/bin/env Rscript

#' Run ARIMA model
#'
#' Run different ARIMA model, inlcuding automatic search, (d=1, D=1), arima
#' with regressors (week days, other time series)
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles and Practice
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-10

# Import libraries ------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(fable)
library(feasts)
library(tsibble)
library(xtable)
library(ggdist)

# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
ls_occ <- readRDS(file = data_path)


# Preliminaries ---------------------------------------------------------------
provider <- c("provider_level", "frontier")
names(ls_occ) <- provider
sites <- ls_occ[[1]]$site |> unique()

# Cross validated data
cv_occ <- vector("list", 2)
names(cv_occ) <- provider
cv_occ$provider_level <- ls_occ$provider_level |>
  stretch_tsibble(.init = 332, .step = 4) |>
  filter(.id != 4)

cv_occ$frontier <- ls_occ$frontier |>
  stretch_tsibble(.init = 70, .step = 4) |>
  filter(.id != 4) 


# ARIMA -----------------------------------------------------------------------
# Load models
save_path <- here("data/models/arima/")
arima_fit_l <- readRDS(paste0(save_path, 'arima_fit.rds'))

# Fit model for each data provider
arima_fit <- vector("list", 2)
names(arima_fit) <- provider

arima_fit$provider_level <- cv_occ$provider_level |>
  model(
    auto = ARIMA(bed_occ, stepwise = FALSE),
    dD = ARIMA(
      bed_occ, 
      order_constraint = d == 1 & D == 1, 
      stepwise = FALSE
    ),
    dweek = ARIMA(
      bed_occ ~ days_ + pdq(d = 1), 
      stepwise = FALSE
    ),
    week = ARIMA(
      bed_occ ~ days_, 
      stepwise = FALSE
    )
  )
arima_fit$frontier <- cv_occ$frontier |>
  model(
    auto = ARIMA(
      bed_occ, 
      stepwise = FALSE
    ),
    dweek = ARIMA(
      bed_occ ~ days_ + pdq(d = 1), 
      stepwise = FALSE
    ),
    week = ARIMA(
      bed_occ ~ days_, 
      stepwise = FALSE
    ),
    week_ade = ARIMA(
      bed_occ ~ days_ + adm + dis + bed_escal, 
      stepwise = FALSE
    ),
    week_adelag = ARIMA(
      bed_occ ~ 
        days_ +
          adm + dis + bed_escal +
          lag(adm, 1) + lag(dis, 1) + lag(bed_escal, 1) +
          lag(adm, 2) + lag(dis, 2) + lag(bed_escal, 2) +
          lag(adm, 3) + lag(dis, 3) + lag(bed_escal, 3) +
          lag(adm, 4) + lag(dis, 4) + lag(bed_escal, 4) +
          lag(adm, 5) + lag(dis, 5) + lag(bed_escal, 5) +
          lag(adm, 6) + lag(dis, 6) + lag(bed_escal, 6) +
          lag(adm, 7) + lag(dis, 7) + lag(bed_escal, 7) +
          lag(adm, 8) + lag(dis, 8) + lag(bed_escal, 8),
      stepwise = FALSE
    )
  )

# Save in long format
arima_fit_l <- map(provider, \(prov) {
  arima_fit[[prov]] |> 
    pivot_longer(
      c(-site, -.id),
      names_to = "model",
      values_to = "par")
}) |> set_names(provider)


# Multivariate regression -----------------------------------------------------




# Save models -----------------------------------------------------------------

# Save
save_path <- here("data/models/arima/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}
saveRDS(arima_fit_l, file = paste0(save_path, 'arima_fit.rds'))


# Information criteria --------------------------------------------------------
# Use model with less data (id 1)
tbl_ic <- vector("list", 2)
names(tbl_ic) <- provider

tbl_ic$provider_level <- arima_fit_l$provider_level |> 
  filter(.id == 1) |> 
  glance() |> 
  select(.id:BIC, -.id, -.model) |> 
  arrange(site, AICc) |>
  xtable()

tbl_ic$frontier <- arima_fit_l$frontier |> 
  filter(.id == 1) |> 
  glance() |> 
  select(.id:BIC, -.id, -.model) |> 
  arrange(site, AICc) |>
  xtable()


# Plots/tests for residuals -----------------------------------------------------
plot_res <- vector("list", 2)
test_res <- vector("list", 2)
names(plot_res) <- provider
names(test_res) <- provider

# Loop through providers
map(provider, \(prov) {
  # Use model with less data (id 1)
  tmp_res = which(arima_fit_l[[prov]]$.id == 1) |>
    map(\(x) {
      # Generate plot res
      tmp_mabble = arima_fit_l[[prov]][x, ]
      tmp_title = str_glue(
        "{tmp_mabble$site}_{tmp_mabble$model}_{tmp_mabble$par}"
      )
      tmp_plot = tmp_mabble |> 
        gg_tsresiduals() +
        ggtitle(tmp_title)

      # Compute Ljung-Box test
      tmp_dof = tmp_mabble |> 
        tidy() |>
        nrow()
      tmp_test = tmp_mabble |>
        augment() |>
        features(.innov, ljung_box, lag = 14, dof = tmp_dof)

      list(tmp_plot, tmp_test)
    })

  # Separate two lists - attention: global assignment within map
  # Need to change this!!
  plot_res[[prov]] <<- tmp_res |> map(1) 
  test_res[[prov]] <<- tmp_res |> map(2) |> bind_rows()
  })


# Forecast --------------------------------------------------------------------
list_forecast <- map(provider, \(prov) {
  tmp_fit1 = arima_fit_l[[prov]]
  # List of site/model combinations
  ls_names = expand.grid(
    tmp_fit1$site |> unique(),
    tmp_fit1$model |> unique()
  )
  names(ls_names) = c("site", "model")
  tmp_names = paste0(ls_names[, 1], "-", ls_names[, 2])
  ls_names = ls_names |> split(seq(nrow(ls_names)))
  names(ls_names) = tmp_names

  # Loop through site/model combinations
  map(ls_names, \(x) {
    tmp_fit2 = tmp_fit1 |>
      filter(site == x$site & model == x$model) |>
      as_mable(key=c(".id", "site"), model="par")

    # Generate forecast
    if (!any(grepl("LM", as.character(tmp_fit2$par)))) {
      tmp_forecast = tmp_fit2 |> forecast(h = "1 week")
    } else {
      tmp_new_data = cv_occ[[prov]] |> 
        new_data(n = 7) |>
        left_join(ls_occ[[prov]], by = c("index", "site"))
      tmp_forecast = tmp_fit2 |> 
        forecast(new_data = tmp_new_data)
    }

    # Convert tsibble to fable
    tmp_forecast = tmp_forecast |> 
      group_by(.id) |> 
      mutate(
        h = row_number(),
        .model = x$model
      ) |>
      ungroup() |>
      as_fable(response= "bed_occ", distribution = bed_occ)
  })
})
names(list_forecast) <- provider


# Forecast accuracy -----------------------------------------------------------
# Loop through providers
tbl_acc <- map(provider, \(prov) {
  tmp_forecast = list_forecast[[prov]]
  # Loop through sites
  tmp_acc = map(sites, \(site) {
    mask_site = tmp_forecast |> 
      names() |> 
      str_detect(site)
    # Compute accuracy metrics
    map(tmp_forecast[mask_site], \(x) {
      x |> 
        accuracy(ls_occ[[prov]], by=c("h", ".model"))
    }) |> bind_rows()
  }) |> set_names(sites)
}) |> set_names(provider)


# Plot forecast ---------------------------------------------------------------
plot_forecast <- map(provider, \(prov) {
  tmp_forecast = list_forecast[[prov]]
  tmp_names = tmp_forecast |> names()
  map(tmp_names, \(x) {
    # Extract predictive interavals of forecast
    tmp_forecast2 = tmp_forecast[[x]]
    predict_int = tmp_forecast2 |> hilo() |> select(contains("%"))
    
    # Get real data with reduced index
    tmp_site = x |> 
      gsub("-.*", "", x=_)
    tmp_data = ls_occ[[prov]] |> 
      filter(site == tmp_site)
    length_time = tmp_data$index |> 
      tail(1) - tmp_data$index[1]
    time_start = tmp_data$index[1] + floor(length_time / 1.5)
    time_end = tmp_forecast2 |> 
      group_by(.id) |>
      slice_tail() |> 
      as.data.frame() |> 
      pull(index) |>
      as.list()
    names(time_end) = c("1", "2", "3")

    tmp_data2 = imap(time_end, \(.x, .y) {
      tmp_data3 = tmp_data |> 
        filter_index(
          time_start |> as.character() ~ .x |> as.character()
        )
      tmp_data3[[".id"]] = .y |> as.integer()
      tmp_data3 |> update_tsibble(key = c(.id, site))
    }) |> bind_rows()

    # Generate plot
    tmp_forecast2 |>
      ggplot() +
      stat_lineribbon(
        aes(x = index, ydist = bed_occ, group = .id), 
        alpha = 0.7,
      ) +
      scale_fill_brewer(direction = -1) +
      geom_line(
        aes(x = index, y = .mean),
        color = "blue",
        linewidth = 2
      ) +
      geom_line(
        data = tmp_data2,
        aes(
          x = index,
          y = bed_occ,
          group = .id
        )
      ) +
      facet_wrap(.~ .id, ncol = 1) +
      ggtitle(paste0(prov, "-", x))
  }) |> set_names(tmp_names)
}) |> set_names(provider)


# Save plots ------------------------------------------------------------------
save_path <- here("output/plots/models/arima/")

walk(provider, \(prov) {
  tmp_path = paste0(save_path, prov)
  if (!file.exists(tmp_path)) {
    dir.create(tmp_path, recursive = TRUE)
  }
})

walk(provider, \(prov) {
  walk(plot_forecast[[prov]], \(x) {
    tmp_file = paste0(
      save_path, 
      prov, 
      "/",
      sub(".*-", "", x$labels$title),
      ".png"
    )
    x |>
      ggsave(
        filename = tmp_file,
        width = 35, 
        height = 20, 
        units = "cm")
        # units = "cm", 
        # device = cairo_ps)
  })
})


# arima_fit$provider_level |> filter(site=="Southmead") |> glance() |> select(.id:BIC, -site) |> arrange(.id, AICc)
# arima_fit$frontier |> filter(site=="Southmead") |> glance() |> select(.id:BIC, -site) |> arrange(.id, AICc)


# Check residuals (Ljung–Box test)

# Plot characteristic roots
gg_arma()

arima_fit$provider_level |> filter(site=="BRI") |> components()

arima_fit$provider_level |> tidy()  |> filter(.model == "auto_week") |> print(n=100)
arima_fit$frontier|> tidy()  |> filter(.model == "auto_week") |> print(n=300)


harvest <- tsibble(
  year = rep(2010:2012, 2),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
arima_fit$provider_level  |> filter(site=="BRI")|> group_by(.id) |> mutate(bed_occ = bed_occ_z+(1*.id)) |> autoplot()

arima_fit$provider_level$auto_week$
bb <- ls_occ$provider_level|> 
  model(
    auto = ARIMA(bed_occ))
aa |> filter(site=="BRI") |> select(auto) |>()
arima_fit$provider_level|> filter(site=="BRI") |> tidy()
aa == bb
augment(aa) ==augment(bb)

  report()
arima_fit$provider_level <- ls_occ$provider_level |> 
  model(
    auto = ARIMA(bed_occ, stepwise = FALSE, approx = FALSE),
    auto_constraint = ARIMA(bed_occ, order_constraint = d == 1 & D == 1),
    auto_week = ARIMA(bed_occ, xreg(days_)
  )
)

arima_fit$provider_level |>
  filter(site=="Southmead") |> 
  forecast(h ="10 week") |>
  autoplot() +
  autolayer(ls_occ$provider_level |> filter(site=="Southmead") |> select(bed_occ) |> filter_index("2023-09" ~ .))
arima_fit$provider_level |>
  filter(site=="Southmead") |> 
  forecast(h ="10 week") |>
  autoplot() +
  geom_line(data =ls_occ$provider_level |> filter(site=="Southmead") |> select(bed_occ) |> filter_index("2023-09" ~ .) , aes(, x = index, y =bed_occ))
augment(arima_fit$provider_level)
arima_fit$provider_level |> 
  filter(site=="BRI") |> 
  augment() |>
  ggplot(aes(x=index)) +
  geom_line(aes(y = bed_occ, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

arima_fit$frontier <- ls_occ$frontier |> 
  model(
    auto_shallow = ARIMA(bed_occ),
    auto_deep = ARIMA(bed_occ, stepwise = FALSE, approx = FALSE),
    auto_constraint = ARIMA(bed_occ, order_constraint = d == 1 & D == 1))
)
# See model
arima |> 
  pivot_longer(-site, names_to = "Model name",
  values_to = "Orders")
# arima |> filter(site=="BRI") |>  select(auto_constraint) |> report()

# Order by AIC
arima |> glance() |> arrange(AIC) |> select(site:BIC)

# Check residuals
arima |> filter(site=="BRI") |>  
  select(auto_constraint) |> 
  gg_tsresiduals() |> 
  ggsave(filename="res_bri_auto_contraint.png")

arima |> filter(site=="Southmead") |>  
  select(auto_constraint) |> 
  gg_tsresiduals() |> 
  ggsave(filename="res_southmead_auto_contraint.png")

arima |> filter(site=="BRI") |>  
  select(auto_shallow) |> 
  gg_tsresiduals() |> 
  ggsave(filename="res_BRI_auto_shallow.png")

arima |> filter(site=="Southmead") |>  
  select(auto_shallow) |> 
  gg_tsresiduals() |> 
  ggsave(filename="res_southmead_auto_shallow.png")

arima |> filter(site=="BRI") |>  
  select(auto_deep) |> gg_tsresiduals() |> 
  ggsave(filename="res_bri_auto_deep.png")

arima |> filter(site=="Southmead") |>  
  select(auto_deep) |> 
  gg_tsresiduals() |> 
  ggsave(filename="res_southmead_auto_deep.png")

# Ljung-Box test for white-noise residuals
arima |> augment() |> filter(site=="BRI" & .model=="auto_constraint") |> 
  features(.innov, ljung_box, lag = 14)
arima |> augment() |> filter(site=="Southmead" & .model=="auto_constraint") |> 
  features(.innov, ljung_box, lag = 14, dof = 6)

arima |> augment() |> filter(site=="BRI" & .model=="auto_shallow") |> 
  features(.innov, ljung_box, lag = 14)
arima |> augment() |> filter(site=="Southmead" & .model=="auto_shallow") |> 
  features(.innov, ljung_box, lag = 14, dof = 6)

arima |> augment() |> filter(site=="BRI" & .model=="auto_deep") |> 
  features(.innov, ljung_box, lag = 14)
arima |> augment() |> filter(site=="Southmead" & .model=="auto_deep") |> 
  features(.innov, ljung_box, lag = 14, dof = 6)

# Forecast
arima |> 
  filter(site=="BRI") |> 
  select("auto_constraint") |> 
  forecast(h=100) |> 
  autoplot() + 
  geom_line(data=ts_occ |> filter(site=="BRI"), aes(y=bed_occ))
arima |> 
  filter(site=="Southmead") |> 
  select("auto_constraint") |> 
  forecast(h=100) |> 
  autoplot() + 
  geom_line(data=ts_occ |> filter(site=="Southmead"), aes(y=bed_occ))
arima |> 
  filter(site=="BRI") |> 
  select("auto_shallow") |> 
  forecast(h=100) |> 
  autoplot() + 
  geom_line(data=ts_occ |> filter(site=="BRI"), aes(y=bed_occ))
arima |> 
  filter(site=="Southmead") |> 
  select("auto_shallow") |> 
  forecast(h=100) |> 
  autoplot() + 
  geom_line(data=ts_occ |> filter(site=="Southmead"), aes(y=bed_occ))
arima |> 
  filter(site=="BRI") |> 
  select("auto_deep") |> 
  forecast(h=100) |> 
  autoplot() + 
  geom_line(data=ts_occ |> filter(site=="BRI"), aes(y=bed_occ))
arima |> 
  filter(site=="Southmead") |> 
  select("auto_deep") |> 
  forecast(h=100) |> 
  autoplot() + 
  geom_line(data=ts_occ |> filter(site=="Southmead"), aes(y=bed_occ))

# Plot residuals
gg_tsresiduals(arima |> filter(site=="BRI"))
gg_tsresiduals(arima |> filter(site=="Southmead"))

newdata_ <- ts_occ |> select(bed_occ) |> filter(site=="BRI")
ook <- arima |> filter(site=="Southmead") |> forecast(new_data=newdata_)
ook$bed_occ
new_data
|> autoplot() + 
  geom_line(data=ts_occ |> filter(site=="BRI"), aes(y=bed_occ))

report(arima)
arima_fit_l$frontier |> filter(model=="week_adlag", .id==1, site=="BRI") |> fitted()

# See coefficients model
arima_fit_l$frontier |> filter(model=="week_adelag", .id==1, site=="BRI") |> select(par) |> 
 tidy() |> print(n=100)
arima_fit_l$frontier |> filter(model=="week_ade", .id==1, site=="BRI") |> select(par) |> 
 tidy() |> print(n=100)
arima_fit_l$frontier |> filter(model=="week_ad", .id==1, site=="BRI") |> select(par) |> 
 tidy()
arima_fit_ll$frontier |> filter(model=="week_ade", .id==1, site=="BRI") |> select(par) |> 
 tidy()

arima_fit_l$frontier |> filter(model=="week_adlag", .id==1, site=="BRI") |> 
  pull(par) %>% .[[1]]

arima_fit_l$frontier |> tidy()
arima_fit_l$frontier |> filter(model=="week_adlag", .id==1, site=="BRI") |> 
  generate(ls_occ$frontier |> filter(site=="BRI"))
# arima_fit$provider_level |> coef() |> select(.id:statistic) |>  print(n=400)
# arima_fit$frontier |> coef() |> select(.id:statistic) |>  print(n=400)
# arima_fit$provider_level[1, ] |> accuracy()
# arima_fit$provider_level[1, ] |> glance()
# arima_fit_l$provider_level |>
#   filter(.id == 1 & model == "auto") |> augment()
# components()
# pivot_longer(-site)
# Model |> report()


# ndata <- cv_occ$frontier |> 
#   new_data(n=7) |> 
#   mutate(days_ = format(index, "%a") |> 
#         factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) |> 
#   update_tsibble(key=c(.id, site))
# arima_fit$frontier |> forecast(new_data=ndata) |>
#   group_by_key(.id) |> mutate(h=row_number()) |> ungroup() |> 
#   as_fable(response = "bed_occ", distrubution = bed_occ)


# # Plot cross-validated ts
# cv_occ$frontier |> 
#   filter(site=="BRI") |> 
#   group_by(.id) |>
#   mutate(bed_occ = bed_occ + .id/10) |>
#   ungroup() |> 
#   ggplot(aes(x=index, y=bed_occ, colour=factor(.id))) + geom_line()
#
#
# cv_occ$provider_level  |> filter(site=="BRI") |> group_by(.id) |> slice_tail(n=7) |> print(n=50)
#

# filter(site=="BRI") |> group_by(.id) |> slice_tail(n=7) |> print(n=50)
# ls_occ$provider_level |>
#   stretch_tsibble(.init = 334, .step = 4)|>
# filter(site=="BRI") |> group_by(.id) |> slice_tail(n=7) |> print(n=50)
