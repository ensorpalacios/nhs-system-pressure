#' Target functions
#'
#' Functions used within target workflow, evoked within list of tar_target()
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-12-10
#' -----------------------------------------------------------------------------

#' Load hospital data
load_hosp <-
  function() {

    con <- switch(
      .Platform$OS.type,
      windows = DBI::dbConnect(odbc::odbc(), "xsw"),
      unix = {
        DBI::dbConnect(odbc::odbc(), .connection_string = readr::read_lines("/root/sql/sql/sql_connect_string_linux_sql18"))
      }
    )


    report_start <- lubridate::ymd('2022-01-01')
    report_end <- lubridate::ymd('2025-11-01')

    ecds_tbl <- dplyr::tbl(
      con,
      dbplyr::in_catalog(
        catalog = "Analyst_SQL_Area",
        schema = "dbo",
        table = "tbl_BNSSG_Datasets_UrgentCare_Daily"
      )
    )

    metrics <- c(
      '347334',
      '347347',
      '347348',
      '346199',
      '346203',
      '346200',
      '346183',
      '346186',
      '347351',
      '347331',
      '346098',
      '346085',
      '346113',
      '346114',
      '346117',
      '347318',
      '347357',
      '346170',
      '346209'
    )

    hosp_data <-
      ecds_tbl %>%
      dplyr::filter(
        METRIC_ID %in% metrics,
        dplyr::between(Report_Date, report_start, report_end)
      ) %>%
      dplyr::collect() %>%
      dplyr::mutate(

        METRIC_NAME = dplyr::recode(
          METRIC_NAME,
          !!!c(
            # "Number of Discharges",
            "General & Acute Beds - Total G&A escalation beds open" = "Escalation beds open",
            "Of total G&A beds open, number occupied" = "Bed occupancy",
            "Total G&A escalation beds open" = "Escalation beds open",
            # "Number of Admissions",
            "Beds occupied by Long-stay patients 21+ days? beds occupied by patients with a length of stay of 21 or more days" = "Beds with 21+ days LOS",
            "Beds occupied by Long-stay patients 21+ daysâ€“ beds occupied by patients with a length of stay of 21 or more days" = "Beds with 21+ days LOS",
            "Beds occupied by Long-stay patients 21+ days– beds occupied by patients with a length of stay of 21 or more days" = "Beds with 21+ days LOS",
            "Beds occupied by Long-Stay Patients 21+ days" = "Beds with 21+ days LOS",
            "A&E Performance - Number of A&E Attendances - Type 1 - Paediatrics" = "A&E attends - paediatrics",
            "Number of A&E Attendances - Type 1 - Paediatrics" = "A&E attends - paediatrics",
            "General & Acute Beds - Of total G&A beds open, number occupied" = "Bed occupancy",
            "Of total G&A beds open number occupied" = "Bed occupancy",
            "Total G & A core bed stock open" = "Core stock open",
            "Total G&A core bed stock open" = "Core stock open",
            "General & Acute Beds - Total G&A core bed stock open" = "Core stock open"
          )
        )
      ) %>%
      dplyr::rename_with(.fn = stringr::str_to_lower) %>%
      dplyr::mutate(report_date = lubridate::ymd(report_date))

    # Save data
    file_path = file.path(save_path, "hosp_data.RDS")
    saveRDS(hosp_data, file = file_path)

    # Return path
    file_path
  }

#' Prepare data
#' @path_h Path hospital data
#' @path_t Path temperature data
prepare_data <-
  function(df_occ) {
    df_occ <- readRDS(df_occ)
    # Temperature data
    df_t <-
      get_temp_historic()
    df_t <-
      df_t %>%
      melt(id.vars = "report_date", variable.name = "metric_name") %>%
      mutate(report_date = lubridate::ymd(report_date))
    # Join data
    df_occ <-
      df_occ %>%
      bind_rows(
        copy(df_t)[, provider := "BRI"],
        copy(df_t)[, provider := "NBT"]
      )

    # Data wrangling:
    # keep BRI & Southmead, generate index, rename variables
    df_occ <-
      df_occ |>
      filter(
        provider == "BRI" | provider == "NBT"
      ) |>
      select(-metric_id) |>
      pivot_wider(names_from = metric_name, values_from = value) |>
      mutate(
        index = as.Date(report_date, tz = "GMT"),
        site = case_match(
          provider,
          "BRI" ~ "BRI",
          "NBT" ~ "Southmead"
        ),
        adm = `Number of Admissions`,
        dis = `Number of Discharges`,
        occ = `Bed occupancy`,
        paed = `A&E attends - paediatrics`,
        los = `Beds with 21+ days LOS`,
        provider = NULL,
        `Number of Admissions` = NULL,
        `Number of Discharges` = NULL,
        `Escalation beds open` = NULL,
        `Bed occupancy` = NULL,
        `Core stock open` = NULL,
        report_date = NULL,
        `A&E attends - paediatrics` = NULL,
        `Beds with 21+ days LOS` = NULL
      ) |>
      relocate(c(index, site)) |>
      arrange(index, site)

    # Covert to timeseries (tsibble object)
    ts_data <- df_occ |> as_tsibble(index = index, key = site)

    ######
    # Temporary while using data I have
    # Remove first 3/4 of 2022 data (due to strange behaviour)
    cat("starting data from 22/09/01")
    ts_data <-
      ts_data %>% filter(index >= as.Date("2022-09-01"))
    ######

    # Convert implicit gaps into explicit missing values
    ts_data <-
      ts_data |>
      fill_gaps(.full = TRUE) # fully balanced data

    # Impute missing values (simple moving average, window=7)
    impute_fun <-
      function(.dat) {
        na_seadec(
          .dat, algorithm = "ma",
          k = 3,
          weighting = "simple",
          find_frequency = TRUE
        )
      }
    ts_data <- # impute
      ts_data %>%
      group_by(site) %>%
      mutate(
        occ = occ %>% impute_fun(),
        dis = dis %>% impute_fun(),
        adm = adm %>% impute_fun(),
        paed = paed %>% impute_fun(),
        los = los %>% impute_fun(),
        tmin = tmin %>% impute_fun(), # shouldn't be necessary
        tmax = tmax %>% impute_fun() # shouldn't be necessary
      ) %>%
      ungroup()

    # Process bed occupation
    ts_data <-
      ts_data %>%
      group_by(site) %>%
      mutate(
        # stabilise (- xristmus effect)
        occ = stabilise(occ, index, .xdays = TRUE),
      ) %>%
      ungroup()

    # Aggregate BRI/Southmead
    ts_data <-
      ts_data %>%
      aggregate_key(
        site,
        occ = sum(occ),
        adm = sum(adm),
        dis = sum(dis),
        paed = sum(paed),
        los = sum(los),
        tmax = unique(tmax), # = across sites; necessary to return 1 value
        tmin = unique(tmin), # = across sites; necessary to return 1 value
      )

    # Process admissions-discharges
    ts_data <-
      ts_data %>%
      group_by(site) %>%
      mutate(
        # New variables
        ad_diff = adm - dis, # difference
        # stabilise (-holidays/week days effect)
        ad_diff = stabilise(ad_diff, index, .xdays = "ad-diff", .wdays = TRUE),
        ad_diff2 = c(0, diff(ad_diff)), # rate of change of ad_diff
        # Filter
        ad_diff_f = slide_dbl(ad_diff, mean, .before = 2),
        ad_diff2_f = slide_dbl(ad_diff2, mean, .before = 2)
      ) %>%
      ungroup()

    # Length of stay (+21)
    ts_data <-
      ts_data %>%
      group_by(site) %>%
      mutate(
        # Remove sudden drops from BRI (replace with moving avg)
        # iqr rule to detect (left-tail) outliers
        mask = los <
          quantile(los)[2] - 1.5 * (quantile(los)[4] - quantile(los)[2]),
        mavg = slide_dbl(los, mean, .before = 5, .after = 5),
        los = if_else(mask, mavg, los),
        mask = NULL,
        mavg = NULL
      ) %>%
      ungroup()

    # Add days of week and numeric time index
    ts_data <-
      ts_data %>%
      mutate(
        days_ = format(index, "%a") |>
          factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
        # t_ax = rep(1:(length(days_)/2), 2)
        t_ax = index %>% as.numeric(),
        t_ax = t_ax - min(t_ax) + 1
      )

    # Return time series data
    ts_data
  }

#' Compute threshold
#' @param ts_data Entire time series data
compute_threshold <- 
  function(ts_data) {
    # Recode sites
    ts_data <- 
      ts_data %>% filter(!is_aggregated(site)) %>%
      mutate(site = as.character(site))
    
    # Compute alarm threshold
    alarm_thr <- # compute threshold on (all) training data
      ts_data %>% as.data.table() %>%
      .[
        site != "aggregate",
        .(thr = quantile(occ, probs = threshold_prob)),
        by = site
      ]
    
    # Return value
    alarm_thr
  }


#' Forecast bed occupancy
#' @param ts_data Entire time series data
forecast_occ <- 
  function(ts_data) {
    sites <- ts_data$site %>% unique()
    
    
    # Reproducible analysis for rf and xgb
    set.seed(123) 
    
    # Select relevant variables
    ts_data <- 
      ts_data %>%
      select( # exclude
        -adm, -dis, -ad_diff, -ad_diff2
      )
    
    # Get last 16 weeks for training
    train_length <- 
      lubridate::duration(train_length) %>% 
      lubridate::time_length("days")
    start_training <- 
      max(ts_data$index) - train_length - 7 # + 7 days temporarily for lag_fun
    ts_data <- 
      ts_data %>% group_by(site) %>% 
      filter(index > start_training) %>% 
      ungroup()
    
    # Process exogenous variables
    xpredict_method = "pull" # include tslm, arima, ets
    data_xpredict <- 
      xpredict_fun(
        ts_data,
        c("occ", "ad_diff", "paed", "los"), 
        xpredict_method
        )
    
    
    # Add lags
    data_xpredict <- lag_fun(data_xpredict, .lag = horizon)
    
    
    # Factorise temperature data
    for (.lag in seq(0, 7)) data_xpredict = factorise_temp(data_xpredict, .lag)
    
    
    # Fit models
    list_best_models <-
      list(
        "BRI" = 
          c("arima_dadpl_rec", "arima_dadp_rec", "rf_int", 
            "var_paed", "var_h", "xgb"),
        "Southmead" = 
          c("arima_dadpl_rec", "arima_dadp_rec", "rf_int", 
            "var_paed", "var_los", "xgb")
      )
    
    # ARIMA aggregated
    fc_fable_rec <- 
      data_xpredict %>% 
      filter(type == "train") %>%
      tsibble(index = index, key = site) %>%
      model(
        arima_dadp_agg = 
          ARIMA(
            occ ~ 
              days_ + 
              ad_diff_f_lag3 + ad_diff2_f_lag3 +
              ad_diff_f_lag6 + ad_diff2_f_lag6 +
              paed + paed_lag1
          ),
        arima_dadpl_agg = 
          ARIMA(
            occ ~ 
              days_ + 
              ad_diff_f_lag3 + ad_diff2_f_lag3 +
              ad_diff_f_lag6 + ad_diff2_f_lag6 +
              paed + paed_lag1 +
              los + los_lag1
          )
      ) %>% 
      reconcile(
        arima_dadp_rec = min_trace(arima_dadp_agg, method = "mint_shrink"),
        arima_dadpl_rec = min_trace(arima_dadpl_agg, method = "mint_shrink")
      ) %>% 
      select(-arima_dadp_agg, -arima_dadpl_agg) %>%
      forecast(
        new_data = data_xpredict %>% 
          filter(type == "test") %>% 
          tsibble(index = index, key = site)
      ) 
    
    
    # Vector autoregressive models
    fc_fable_var_los <- # Length of stay (+21)
      data_xpredict %>% 
      filter(type == "train", !is_aggregated(site)) %>%
      mutate(site = site %>% as.character()) %>% 
      tsibble(index = index, key = site) %>%
      model(
        var_los = VAR(vars(occ, los) ~ season(period = "week"))
      ) %>% 
      forecast(h = horizon)
    
    fc_fable_var_paed <- # A&E paed
      data_xpredict %>% 
      filter(type == "train", !is_aggregated(site)) %>%
      mutate(site = site %>% as.character()) %>% 
      tsibble(index = index, key = site) %>%
      model(
        var_paed = VAR(vars(occ, paed) ~ season(period = "week"))
      ) %>% 
      forecast(h = horizon)
    
    fc_fable_var_h <- # BRI vs Southmead
      data_xpredict %>% 
      filter(type == "train", !is_aggregated(site)) %>%
      mutate(site = site %>% as.character()) %>% 
      tsibble(index = index, key = site) %>%
      select(occ) %>% 
      pivot_wider(names_from = site, values_from = occ) %>% 
      model(
        var_h = VAR(vars(BRI, Southmead) ~ season(period = "week"))
      ) %>% 
      forecast(h = horizon)
    
    fc_var <- # bind VAR fc
      map(
        list(
          fc_fable_var_los, fc_fable_var_paed
        ),
        \(.x) {
          .x %>% 
            as_tibble() %>% 
            mutate(
              occ = 
                dist_normal(
                  mean = .distribution %>% mean() %>% .[, "occ"],
                  sigma = .distribution %>% variance() %>% sqrt() %>% .[, "occ"]
                ),
              .mean = occ %>% mean(),
              .distribution = NULL
            ) %>% 
            as_tsibble(index = index, key = c(site, .model))
        }
      ) %>% 
      bind_rows() %>% 
      bind_rows(
        imap(
          fc_fable_var_h %>% pluck(".distribution") %>%  dimnames(),
          \(.x, .y) {
            fc_fable_var_h %>% 
              as_tibble() %>% 
              mutate(
                occ = 
                  dist_normal(
                    mean = .distribution %>% mean() %>% .[, .x],
                    sigma = .distribution %>% variance() %>% sqrt() %>% .[, .x]
                  ),
                site = .x,
                .model = "var_h",
                .mean = .mean[.y],
                .distribution = NULL
              ) %>% 
              as_tsibble(index = index, key = c(site, .model))
          }
        ) %>% 
          bind_rows()
      )
    
    
    # Random forest - interaction
    list_var_rf <- # list predictors
      data_xpredict %>%
      select(
        contains("occ"),
        contains("paed"), -paed,
        contains("los"), -los,
        contains("tvar"), -tvar,
        matches("ad_diff.*_f"), -ad_diff_f, -ad_diff2_f,
        contains("days_"), -days_) %>%
      names()
    
    data_xpredict_int <- # long format data with lag column
      data_xpredict %>%
      select(type, site, index, all_of(list_var_rf)) %>%
      rename_with(~ sub("occ_lag", "occ_same_lag", .x)) %>% 
      rename_with(~ sub("_lag", "-lag", .x, fixed = TRUE)) %>%
      pivot_longer(
        cols = c(contains("lag")),
        names_to = c(".value", "lag"),
        names_sep = "-"
      )
    
    rf_int_par <- # forecast (returns distribution parameters) ...
      rf_reg_int(
        data_xpredict_int %>% filter(!is_aggregated(site)), horizon
      )
    
    fc_rf_int <- # harmonise fc to fable ...
      harmonise_fc_par(rf_int_par, data_xpredict, "rf_int") %>% 
      as_tsibble(index = index, key = c("site", ".model"))
      
    
    # XGBoosting - interaction
    # Same predictors as rf interaction but without paed and los
    data_xpredict_int_xgb <- 
      data_xpredict_int %>% select(-c(paed, los))
    
    xgb_par <- # forecast (returns distribution parameters)...
      xgb_reg_int(
        data_xpredict_int_xgb %>% filter(!is_aggregated(site)), horizon
        )
    
    fc_xgb <-  # harmonise fc to fable ...
      harmonise_fc_par(xgb_par, data_xpredict, "xgb") %>% 
      as_tsibble(index = index, key = c("site", ".model"))
    
    
    # Join fc
    dimnames(fc_var$occ) <- "occ" # add name to column to match fc_fable
    dimnames(fc_rf_int$occ) <- "occ"
    dimnames(fc_xgb$occ) <- "occ"
    fc_all <- 
      list(fc_fable_rec, fc_var, fc_rf_int, fc_xgb) %>% 
      reduce(bind_rows) %>% 
      select(site, .model, index, occ, .mean)
    
    
    # Remove aggregated fc
    fc_all <- 
      fc_all %>% filter(!is_aggregated(site)) %>%
      mutate(site = as.character(site))
    
    
    # Return forecasts
    fc_all
  }


#' Fc combination wrapper
#' Wrapper function to generate weighted linearly combined fc. Take reciprocal
#' of crps/wilker as higher is worse
#' @param .fc Original fc to combine.
#' @param .weight Weights for forecast combination.
#' @param .list_models List of models to combine for each hospital.
fc_combination <- 
  function(.fc, .weights, .list_models) {
    # Data wrangling
    .fc <- # add fc horizon h
      .fc %>%
      group_by_key() %>% 
      mutate(
        h = as.numeric(index) - min(as.numeric(index)) + 1,
      ) %>% 
      ungroup()
    
    # fc_comb_lp <-
    #   lcomb_fun(.fc, .weights, .list_models, "none", "equal")
    # 
    # fc_comb_crps_u <-
    #   lcomb_fun(.fc, .weights, .list_models, "upper", "crps")
    # fc_comb_crps_u$`.model` = "crps_upper" # rename
    fc_comb_crps <-
      lcomb_fun(.fc, .weights, .list_models, "none", "crps")
    
    dimnames(fc_comb_crps$occ) <- "occ"
    
    .fc <- 
      list(.fc, fc_comb_crps) %>% 
      reduce(bind_rows) %>%  as_fable(".mean", "occ")
    
    # Return fc
    .fc
  }


#' Compute risk
#' @param fc_comb Forecast ensemble
#' @param alarm_thr Threshold for high/low system pressure
compute_risk <- 
  function(.fc, .thr) {
    # Reproducible analysis for bootstrapping splits
    set.seed(321)
    
    
    # Add alarm thresholds to fc
    fc_threshold <-
      .thr[
        .fc %>% select(site, .model, index, occ, .mean, h),  on = "site"
      ]
    
    
    # Compute threshold-crossing probabilities
    # Risk by days
    risk_d <-  # compute risk
      copy(fc_threshold)[
        , risk_day := 1 - cdf(occ, thr), by = .(site, .model, h)
      ]
    
    
    # Risk by week split (1-3h, 4-7h)
    risk_d[ # split week in two
      , week_split := ifelse(h <= 3, "close", "far")
    ]
    risk_ws <- 
      risk_d[ # compute risk
        , .(risk_ws = 1 - prod(1 - risk_day)), 
        by = .(site, .model, week_split) 
      ]
    
    
    # Risk by week
    risk_w <- 
      risk_d[ # compute risk
        , .(risk_w = 1 - prod(1 - risk_day)), 
        by = .(site, .model)
      ]
    
    list_risk <- 
      list(
        "risk_d" = risk_d,
        "risk_ws" = risk_ws,
        "risk_w" = risk_w
      )
    
    # Save risk predictions, threshold and fc by dates
    adate <- lubridate::today() # analysis date
    list_data <-
      list(
        "fc" = .fc,
        "threshold" = .thr,
        "risk" = list_risk,
        "date" = adate
      )

    target_output_path <- file.path(save_path, "output", paste0(adate, ".RDS"))
    
    if (!file.exists(dirname(target_output_path))) {
      dir.create(dirname(target_output_path), recursive = TRUE)
    }
    
    saveRDS(list_data, target_output_path)
    
    # Return list
    target_output_path
  }


# #' Plot risk
# #' @param risk_file Contains all elements for plot (risk prediction, fc, thr)
# plot_risk <- 
#   function(path_data) {
#     # Prepare for plot
#     all_data <- readRDS(path_data)
#     thr <- all_data$threshold
#     risk_d <- all_data$risk$risk_d
#     risk_ws <- all_data$risk$risk_ws
#     risk_w <- all_data$risk$risk_w
#     fc <- all_data$fc
    
#     sites <- thr[, unique(site)]
    
#     list_models = c("crps")
    
    
#     # Make plot
#     plt_risk <- 
#       map(sites, \(.site) {
#         # Prepare data
#         tmp_tbl = # daily risks
#           risk_d[
#             site == .site & .model %in% list_models
#           ]
        
#         tmp_thr = tmp_tbl[1, thr] # threshold (all equals)
        
#         risk_close = # week split close risk
#           risk_ws[
#             site == .site & .model %in% list_models &
#               week_split == "close"
#           ][ # add x-axis position
#             , x_axis := 1
#           ]
#         risk_far = # week split far risk
#           risk_ws[
#             site == .site & .model %in% list_models &
#               week_split == "far"
#           ][ # add x-axis position
#             , x_axis := 2
#           ]
#         risk_weeks = # join close/far
#           rbind(risk_close, risk_far)
        
#         risk_week = # week (whole) risk
#           risk_w[
#             site == .site & .model %in% list_models
#           ][ # add x-axis position
#             , x_axis := 3
#           ]
        
#         tmp_fc =
#           fc %>% 
#           filter(site == .site, .model %in% list_models)
        
#         # Plot
#         p1 = # predicted risk weekly (split and whole)
#           ggplot() +
#           geom_col(
#             data = risk_weeks,
#             aes(x = x_axis, y = risk_ws, fill = .model),
#             position = "dodge"
#           ) +
#           geom_col(
#             data = risk_week,
#             aes(x = x_axis, y = risk_w, fill = .model),
#             position = "dodge"
#           ) +
#           geom_hline(yintercept = 0.5, color = "red", lty = "11", linewidth = 1) +
#           # ylim(0, .8) +
#           # scale_colour_manual(name = "models", values = col_models) + 
#           scale_fill_manual(name = "models", values = col_models) +
#           scale_x_continuous(
#             breaks = c(1, 2, 3), labels = c("1-3", "4-7", "week")
#           )
        
#         p2 = # predicted risk daily
#           tmp_tbl %>% 
#           ggplot(aes(x = index, y = risk_day, fill = .model)) + 
#           geom_col(position = "dodge", width = 0.5) +
#           geom_hline(yintercept = 0.5, color = "red", lty = "11", linewidth = 1) +
#           # ylim(0, .8) +
#           # scale_colour_manual(name = "models", values = col_models) + 
#           scale_fill_manual(name = "models", values = col_models)
        
#         p3 = # time series
#           tmp_fc %>% 
#           autoplot() +
#           # geom_line(
#             # data = tmp_tbl[.model == tmp_tbl$.model[1]],
#             # aes(x = index, y = occ_obs, group = 1), linewidth = 2
#           # ) +
#           geom_hline(
#             yintercept = tmp_thr, color = "red", lty = "f8", linewidth = 2
#           )
#         # p3 = # time series
#         # tmp_tbl[.model == tmp_tbl$.model[1]] %>% 
#         # ggplot(aes(x = index, y = occ_obs, group = 1)) + 
#         # geom_line(linewidth = 2) +
#         # geom_hline(yintercept = tmp_thr, color = "red", lty = "f8", linewidth = 2)
        
#         # Join
#         p1 / p2 / p3 + 
#           plot_layout(
#             ncol = 1, axes = "collect_x", guides = "collect"
#           )
#       }) %>% 
#       set_names(sites)
    
#     # Return plot
#     plt_risk
# }