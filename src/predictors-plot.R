#' Plot predictors data

#' Generate plot to describe predictors data and their relationship with bed occupancy.
#' Lagged regression (LagReg(input, output)):
#' Find coefficients \betas of lagged regression model 
#' y = Sum_r(beta_r * x_r) + v_r, where r is lag, y is output signal, x
#' is input signal, and v is iid noise. Steps:
#' - pectral analysis of y and x to get coh, individual spectra, phase
#' - Fourier reppresentation of \betas (B) computed as 
#' sqrt(coh * spec_output/spec_input) * exp(1i * phase), where sqrt(.) is the
#' magnitude of the input/output gain (amplification of input into output at
#' certain freq) adjusted by the coh (linear, "predictable" x-y relationship 
#' for each freq) and exp(.) is the phase shift between input and output.
#' - Invert B, by discretizing the defining integral, to get the coefficients
#' betas 

#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles and Practice
#' CI auto-/cross-corraltion is 1−α/2 quantile * standard deviation of 
#' autocorrelation (sqrt(var)=1/sqrt(n))
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-08

# Prepare environment ----------------------------------------------------------
rm(list = ls())
source("src/environment.R")



# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
ts_occ <- readRDS(file = data_path)
ts_occ <- 
  ts_occ %>% 
  filter(!is_aggregated(site)) %>% 
  mutate(site = site %>% as.character())
sites <- ts_occ$site %>% unique()



# Plot bed occupancy -----------------------------------------------------------
# With missing values
plot_occ_miss <- 
  ggplot_na_distribution(
    ts_occ %>% filter(site == "BRI") %>%  .$occ_m,
    title = "Bed occupancy",
    subtitle = "BRI"
    ) +
  ggplot_na_imputations(
    ts_occ %>% filter(site == "Southmead") %>% .$occ_m,
    # ts_occ %>% filter(site == "Southmead") %>% .$occ_i,
    ts_occ %>% filter(site == "Southmead") %>% .$occ_wx,
    size_imputations = 5,
    title = NULL,
    subtitle = "Southmead"
    ) +
  plot_layout(ncol = 1, axis = "collect_x")


# Occupancy
plot_occ <-
  ts_occ %>% 
    ggplot(aes(x = index, y = occ, colour = site)) +
    geom_line(linewidth = 1) +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y = "bed occupancy") +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )


plot_occ_acf = plot_cf(ts_occ, .var = "occ", .lag = 100)



# Plot row predictors (highlight imputations) ----------------------------------
# Admissions - missing
plot_adm_miss <-
  ggplot_na_distribution(
    ts_occ %>% filter(site == "BRI") %>%  .$adm_m,
    title = "Admissions",
    subtitle = "BRI"
    ) +
  ggplot_na_imputations(
    ts_occ %>% filter(site == "Southmead") %>% .$adm_m,
    ts_occ %>% filter(site == "Southmead") %>% .$adm,
    size_imputations = 5,
    title = NULL,
    subtitle = "Southmead"
    ) +
  plot_layout(ncol = 1, axis = "collect_x")


# Discharges - missing
plot_dis_miss <-
  ggplot_na_distribution(
    ts_occ %>% filter(site == "BRI") %>%  .$dis_m,
    title = "Discharges",
    subtitle = "BRI"
    ) +
  ggplot_na_imputations(
    ts_occ %>% filter(site == "Southmead") %>% .$dis_m,
    ts_occ %>% filter(site == "Southmead") %>% .$dis,
    size_imputations = 5,
    title = NULL,
    subtitle = "Southmead"
    ) +
  plot_layout(ncol = 1, axis = "collect_x")


# Admission - discharges (raw and filtered)
tbl_ad_diff <- 
  ts_occ %>%
  pivot_longer(
    cols = c(ad_diff, ad_diff2, ad_diff_f, ad_diff2_f),
    names_to = "var"
  ) %>% 
  mutate(
    var = factor(var)#, levels = c("ad_diff", "filtered"))
  )
plot_ad_diff <- 
  tbl_ad_diff %>% 
  as.data.frame() %>% 
  ggplot(aes(x = index, y = value, colour = site)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(vars(var), ncol = 1, scales = "free")

plot_ad_diff_acf = plot_cf(ts_occ, .var = "ad_diff", .lag = 100)
plot_ad_diff2_acf = plot_cf(ts_occ, .var = "ad_diff2", .lag = 100)
plot_ad_diff3_acf = plot_cf(ts_occ, .var = "ad_diff3", .lag = 100)
plot_ad_diff_f_acf = plot_cf(ts_occ, .var = "ad_diff_f", .lag = 100)
plot_ad_diff2_f_acf = plot_cf(ts_occ, .var = "ad_diff2_f", .lag = 100)
plot_ad_diff3_f_acf = plot_cf(ts_occ, .var = "ad_diff3_f", .lag = 100)


# Bed escalation (raw and filtered)
tbl_escal <- 
  ts_occ %>%
  select(escal) %>% 
  pivot_longer(
    cols = c(escal),
    names_to = "var"
  ) %>% 
  mutate(
    var = factor(var)#, levels = c("ad_diff", "filtered"))
  )

plot_escal <- 
  tbl_escal %>% 
  as.data.frame() %>% 
  ggplot(aes(x = index, y = value)) +
  geom_line(linewidth = 1) +
  # geom_point(
  #   data = tbl_escal %>% filter(escal_c == T),
  #   aes(x = index, y = -5, size = 2), 
  #   colour = "black"
  # ) +
  facet_wrap(vars(site), ncol = 1, scales = "free")


# A&E paediatric - missing
plot_paed_miss <-
  ggplot_na_distribution(
    ts_occ %>% filter(site == "BRI") %>%  .$paed_m,
    title = "A&E paediatric",
    subtitle = "BRI"
    ) +
  ggplot_na_imputations(
    ts_occ %>% filter(site == "Southmead") %>% .$paed_m,
    ts_occ %>% filter(site == "Southmead") %>% .$paed,
    size_imputations = 5,
    title = NULL,
    subtitle = "Southmead"
    ) +
  plot_layout(ncol = 1, axis = "collect_x")

plot_paed_acf = plot_cf(ts_occ, .var = "paed", .lag = 100)


# Length of stay (+21) - missing
plot_los_miss <-
  ggplot_na_distribution(
    ts_occ %>% filter(site == "BRI") %>%  .$los_m,
    title = "Length of stay (+21)",
    subtitle = "BRI"
    ) +
  ggplot_na_imputations(
    ts_occ %>% filter(site == "Southmead") %>% .$los_m,
    ts_occ %>% filter(site == "Southmead") %>% .$los,
    size_imputations = 5,
    title = NULL,
    subtitle = "Southmead"
    ) +
  plot_layout(ncol = 1, axis = "collect_x")

plot_los_acf = plot_cf(ts_occ, .var = "los", .lag = 100)


# Temperature
ts_occ <- 
  ts_occ %>%
  mutate(
    tdiff = tmax - tmin,
    tdiff15 = if_else(tmin < 7 & tmax > 22, T, F)
    )
  
plot_temp <- 
  ts_occ %>% filter(!is_aggregated(site)) %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = tmax), colour = "red") +
  geom_line(aes(y = tmin), colour = "blue") +
  geom_vline(data = . %>% filter(tdiff15), aes(xintercept = index)) +
  facet_wrap(vars(site), ncol = 1)

tbl_temp <- 
  ts_occ %>% as_tibble() %>%  select(tmax, tmin, tdiff) %>% 
  summary() %>% 
  kbl() %>%# kable_minimal()
  kable_styling(full_width = F)


# Plot bed occupancy and predictors -------------------------------------------
ex_var = c("ad_diff_f", "ad_diff2_f", "paed", "los", "tmax", "tmin")


# Time series - all bed data
plot_beds <- 
  ts_occ %>% 
  pivot_longer(
    # cols = c(occ_i,
    cols = c(occ,
             core),
    names_to = "var"
    ) %>% 
      mutate(
        var = factor(
          var
          )
      ) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = index, y = value, colour = var)) +
  geom_line() +
  facet_wrap(vars(site), ncol = 1, scales = "free")


# Time series - bed occupation and predictors
ts_occ_l <- # convert in long format
  ts_occ %>% 
  pivot_longer(
    cols = c(occ, all_of(ex_var), -occ_other),
    names_to = "var"
  ) %>% 
  mutate(
    var = factor(var)
  )

plot_together <- # plot
  ts_occ_l |>
  as.data.frame() |> 
  mutate(
    var = factor(var, 
                 levels = c("occ", 
                            "ad_diff_f", "ad_diff2_f", 
                            "los", "paed",
                            "tmax", "tmin"))
    ) %>% 
  ggplot(aes(x = index, y = value, colour = site)) +
  geom_line() +
  facet_wrap(vars(var), ncol = 1, scales = "free")


# Cross-correlation (! positive values means ex_var leads bed occupancy)
# alpha_ <-  0.05
# ci_lim <- # confidence interval (see descriptive-plot.R) 
#   qnorm((1 + (1 - alpha_)) / 2) / sqrt(nrow(ts_occ) / 2) #
# 
# tbl_ccf = # compute ccf
#   map(ex_var, \(x) {
#     tmp_ccf = ts_occ |> 
#       CCF(occ, !!as.symbol(x)) |>
#       mutate(var = x) |>
#       update_tsibble(key = c(site, var))
#   }) |> bind_rows()
# 
# plot_ccf <-  # plot ccf
#   tbl_ccf |>
#     ggplot(aes(x = lag, y = ccf, group = var)) +
#     geom_segment(mapping = aes(xend = lag, yend = 0)) +
#     geom_hline(
#       aes(yintercept = ci_lim), 
#       linetype = 2, 
#       colour = 'blue') +
#     geom_hline(
#       aes(yintercept = -ci_lim), 
#       linetype = 2, 
#       colour = 'blue') +
#     labs(x = "lag (days)") +
#     facet_wrap(
#       factor(var) ~., 
#       ncol = 1)
#     # xlim(0, NA) # causes warning


# Preliminary analysis ---------------------------------------------------------
# Use only test data
split_data_tt <- # Train/test set
  split_tt(ts_occ, len_test)

# Lags/spectral analysis
plot_lags =
  map(sites, \(.site) {
    tmp_data = 
      split_data_tt %>% 
      filter(type == "train", site == .site) %>% as_tibble() %>% 
      select(c("occ", all_of(ex_var))) %>%
      ts(frequency = 7)
    
    # Pre-whitened cross-correlation
    plt_pwcorr =
      map(ex_var, \(.ex) {
        save_plot(pre.white(tmp_data[, .ex], tmp_data[, "occ"]))
      }) %>% 
      set_names(ex_var)
    
    # Lagged regression
    plt_lagreg = 
      map(ex_var, \(.ex) {
        list(
          "ex-occ" = 
            save_plot(
              LagReg(tmp_data[, .ex], tmp_data[, "occ"], L = 10, M = 30)
            ),
          "occ-ex" = 
            save_plot(
              LagReg(tmp_data[, .ex], tmp_data[, "occ"], L = 10, M = 30,
                     inverse = TRUE)
            )
        )
      }) %>% 
      set_names(ex_var)
    
    # Spectrum
    plt_spectr = save_plot(tmp_data %>% .[, "occ"] %>% spectrum(span = 15))
    
    list(
      "plt_pwcorr" = plt_pwcorr, 
      "plt_lagreg" = plt_lagreg, 
      "plt_spectr" = plt_spectr)
  }) %>% 
  set_names(sites)



# Save plots ------------------------------------------------------------------
save_path <- here("output/plots/predictors/")

if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

ls_plots <- list(
  "occ_miss" = plot_occ_miss,
  "occ" = plot_occ,
  "occ_acf" = plot_occ_acf,
  "adm_miss" = plot_adm_miss,
  "dis_miss" = plot_dis_miss,
  "dis_miss" = plot_dis_miss,
  "ad_diff" = plot_ad_diff,
  "ad_diff_acf" = plot_ad_diff_acf,
  "ad_diff2_acf" = plot_ad_diff2_acf,
  "ad_diff3_acf" = plot_ad_diff3_acf,
  "ad_diff_f_acf" = plot_ad_diff_f_acf,
  "ad_diff2_f_acf" = plot_ad_diff2_f_acf,
  "ad_diff3_f_acf" = plot_ad_diff3_f_acf,
  "escal" = plot_escal,
  "paed_miss" = plot_paed_miss,
  "paed_acf" = plot_paed_acf,
  "los_miss" = plot_los_miss,
  "los_acf" = plot_los_acf,
  "temperature" = plot_temp,
  "beds" = plot_beds,
  "together" = plot_together
  )
ls_plots_lag <- list(
  "spectr" = plot_lags %>% map(., ~ ( pluck(.x, "plt_spectr"))),
  "pwccf" = plot_lags %>% map(., ~ ( pluck(.x, "plt_pwcorr"))),
  "lagreg" = plot_lags %>% map(., ~ ( pluck(.x, "plt_lagreg")))
  )

iwalk(ls_plots, \(.plot, .title) {
  .plot %>% 
    ggsave(
      filename = paste(save_path, .title, ".svg"),
      width = 35, height = 20, units = "cm")
})

iwalk(ls_plots_lag, \(.plots, .title) {
  .plots %>% imap(., \(.plt, .site) {
      if (.title == "spectr") {
        svg(str_glue("{save_path}{.title}_{.site}.svg"))
        print(.plt)
        dev.off()
      } else if (.title == "pwccf") {
        .plt %>%
          iwalk(., \(.x, .ex) {
            svg(str_glue("{save_path}{.title}_{.ex}_{.site}.svg"))
            print(.x)
            dev.off()
          })
      } else if (.title == "lagreg") {
        .plt %>%
          iwalk(., \(.x, .ex) {
            iwalk(.x, \(.x, .var_order) {
              svg(
                str_glue("{save_path}{.title}_{.ex}_{.site}_{.var_order}.svg")
                )
            print(.x)
            dev.off()
            })
          })
      }
  })
})

tbl_temp %>% save_kable(paste0(save_path, "tbl_temp.pdf"))
# tbl_temp %>% save_kable(paste0(save_path, "tbl_temp.html"))