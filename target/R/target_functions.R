#' Target functions
#'
#' Functions used within target workflow, evoked within list of tar_target()
#' @param .new_date Dummy variable needed to make tar aware of new date
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-12-10
#' -----------------------------------------------------------------------------

#' Load hospital data
load_hosp <-
  function(.new_date) {
local <- FALSE
# local <- TRUE
if (local) {
  con <- dbConnect(RSQLite::SQLite(), here("target/data/local_db/local_dev.sqlite"))
  message("Connected to: Local SQLite")
} else {
  con <- switch(
    .Platform$OS.type,
    windows = dbConnect(odbc::odbc(), "xsw"),
    unix = {
      dbConnect(odbc::odbc(), .connection_string = readr::read_lines("/root/sql/sql_connect_string_linux_sql18"))
    }
  )
  message("Connected to: Hosted SQL Server")
}

# 2. Define the Table Reference (ecds_tbl)
if (local) {
  # SQLite doesn't understand catalogs/schemas, so we point to the flat table name
  ecds_tbl <- tbl(con, "urgent_care_daily")
} else {
  # Use the full path for the hosted environment
  ecds_tbl <- tbl(
    con, 
    in_catalog(
      catalog = "Analyst_SQL_Area", 
      schema = "dbo", 
      table = "tbl_BNSSG_Datasets_UrgentCare_Daily"
    )
  )
}

    report_end <- lubridate::today()
    report_start <- as.Date(report_end - lubridate::dyears(3))

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
      ecds_tbl  %>%
      dplyr::rename_with(.fn = stringr::str_to_lower) %>%
        dplyr::collect() %>%
      dplyr::mutate(report_date = lubridate::ymd(report_date)) %>%
      dplyr::filter(
        metric_id %in% metrics,
        dplyr::between(report_date, report_start, report_end)
      ) %>%
      dplyr::mutate(

        metric_name = dplyr::recode(
          metric_name,
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
      dplyr::mutate(report_date = lubridate::ymd(report_date))
    # Save data
    file_path = file.path(save_path, "hosp_data.RDS")
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
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
    today <- df_occ$report_date |> max()
    start <- df_occ$report_date |> min()

    # Temperature data (historical data)
    df_t <-
      get_temp(.historic = TRUE, .today = today, .start = start)

    df_t <-
      df_t %>%
      melt(id.vars = "report_date", variable.name = "metric_name")

    # Join data
    df_occ <-
      df_occ %>%
      bind_rows(
        copy(df_t)[, provider := "BRI"],
        copy(df_t)[, provider := "NBT"],
        copy(df_t)[, provider := "WGH"]
      )

    # Data wrangling:
    # Generate index, rename variables
    df_occ <-
      df_occ |>
      select(-metric_id) |>
      distinct(provider, report_date, metric_name, .keep_all = TRUE) |>
      pivot_wider(names_from = metric_name, values_from = value) |>
      mutate(
        index = lubridate::ymd(report_date),
        site = case_match(
          provider,
          "BRI" ~ "BRI",
          "NBT" ~ "Southmead",
          "WGH" ~ "WGH"
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

    # Convert implicit gaps into explicit missing values
    ts_data <-
      ts_data |>
      fill_gaps(.full = TRUE) # fully balanced data

    # Impute missing values (simple moving average, window=7)
    impute_fun <-
      function(.dat) {
        if (all(is.na(.dat))) {
          return(.dat)
        } else {
          withCallingHandlers(
            na_seadec(
              .dat,
              algorithm = "ma",
              k = 3,
              weighting = "simple",
              find_frequency = TRUE
            ),
            warning = function(w) {
              if (
                grepl(
                  "could not detect a seasonal pattern",
                  conditionMessage(w),
                  fixed = TRUE
                )
              ) {
                invokeRestart("muffleWarning")
              }
            }
          )
        }
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
    ts_data_bs <-
      ts_data %>%
      filter(site != "WGH") |>
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

    # Aggregate BRI/Southmead aggregate (BS) and WGH
    ts_data_wgh <- 
      ts_data_bs |> 
      filter(is_aggregated(site)) |>
      mutate(site = "BS") |>
      bind_rows(ts_data %>% filter(site == "WGH")) |> 
      mutate(paed = NULL, los = NULL) |> # remove paed and los for WGH
      aggregate_key(
        site,
        occ = sum(occ),
        adm = sum(adm),
        dis = sum(dis),
        tmax = unique(tmax), # = across sites; necessary to return 1 value
        tmin = unique(tmin), # = across sites; necessary to return 1 value
      )


    # Process admissions-discharges
    ts_data_bs <- # BRI, Southmead
      ts_data_bs %>%
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

    ts_data_wgh <- # WGH
      ts_data_wgh %>%
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
    ts_data_bs <- # BRI/Southmead only (no los for WGH)
      ts_data_bs %>%
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
    ts_data_bs <- # BRI, Southmead
      ts_data_bs %>%
      mutate(
        days_ = format(index, "%a") |>
          factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
        # t_ax = rep(1:(length(days_)/2), 2)
        t_ax = index %>% as.numeric(),
        t_ax = t_ax - min(t_ax) + 1
      )

    ts_data_wgh <- # WGH
      ts_data_wgh %>%
      mutate(
        days_ = format(index, "%a") |>
          factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
        # t_ax = rep(1:(length(days_)/2), 2)
        t_ax = index %>% as.numeric(),
        t_ax = t_ax - min(t_ax) + 1
      )

    # Return time series data
    list("bs" = ts_data_bs, "wgh" = ts_data_wgh)
  }





#' Forecast bed occupancy
#' @param ts_data Entire time series data
forecast_occ <-
  function(ts_data) {
    
    # Get data and sites
    ts_data_bs <- ts_data$bs
    ts_data_wgh <- ts_data$wgh

    ts_data <- # Joint hospital data
      bind_rows(
        ts_data$bs |> filter(!is_aggregated(site)),
        ts_data$wgh |> filter(site == "WGH")
      ) |>
      mutate(site = as.character(site))
    sites <- ts_data$site %>% unique()

    # Reproducible analysis for rf and xgb
    set.seed(123)

    # Select relevant variables
    ts_data_bs <-
      ts_data_bs %>%
      select(
        # exclude
        -adm,
        -dis,
        -ad_diff,
        -ad_diff2
      )
    ts_data_wgh <-
      ts_data_wgh %>%
      select(
        # exclude
        -adm,
        -dis,
        -ad_diff,
        -ad_diff2
      )

    # Get last 16 weeks for training
    train_length <-
      lubridate::duration(train_length) %>%
      lubridate::time_length("days")
    start_training <-
      max(ts_data$index) - train_length - 7 # + 7 days temporarily for lag_fun
    ts_data_bs <-
      ts_data_bs %>%
      group_by(site) %>%
      filter(index > start_training) %>%
      ungroup()
    ts_data_wgh <-
      ts_data_wgh %>%
      group_by(site) %>%
      filter(index > start_training) %>%
      ungroup()

    # Process exogenous variables
    xpredict_method <- "pull" # include tslm, arima, ets
    data_xpredict_bs <-
      xpredict_fun(
        ts_data_bs,
        c("occ", "ad_diff", "paed", "los"),
        xpredict_method
      )
    data_xpredict_wgh <-
      xpredict_fun(
        ts_data_wgh,
        c("occ", "ad_diff"),
        xpredict_method
      )

    # Add lags
    data_xpredict_bs <- lag_fun(data_xpredict_bs, .lag = horizon)
    data_xpredict_wgh <- lag_fun(data_xpredict_wgh, .lag = horizon)

    # Factorise temperature data
    for (.lag in seq(0, 7)) {
      data_xpredict_bs <- factorise_temp(data_xpredict_bs, .lag)
    }
    for (.lag in seq(0, 7)) {
      data_xpredict_wgh <- factorise_temp(data_xpredict_wgh, .lag)
    }

    # Fit models
    # ARIMA aggregated
    fc_fable_rec_bs <- # BRI/Southmead
      data_xpredict_bs %>%
      filter(type == "train") %>%
      tsibble(index = index, key = site) %>%
      model(
        arima_dadp_agg = ARIMA(
          occ ~
            days_ +
            ad_diff_f_lag3 +
            ad_diff2_f_lag3 +
            ad_diff_f_lag6 +
            ad_diff2_f_lag6 +
            paed +
            paed_lag1
        ),
        arima_dadpl_agg = ARIMA(
          occ ~
            days_ +
            ad_diff_f_lag3 +
            ad_diff2_f_lag3 +
            ad_diff_f_lag6 +
            ad_diff2_f_lag6 +
            paed +
            paed_lag1 +
            los +
            los_lag1
        )
      ) %>%
      reconcile(
        arima_dadp_rec = min_trace(arima_dadp_agg, method = "mint_shrink"),
        arima_dadpl_rec = min_trace(arima_dadpl_agg, method = "mint_shrink")
      ) %>%
      select(-arima_dadp_agg, -arima_dadpl_agg) %>%
      forecast(
        new_data = data_xpredict_bs %>%
          filter(type == "test") %>%
          tsibble(index = index, key = site)
      )

    fc_fable_rec_wgh <- # WGH
      data_xpredict_wgh %>%
      filter(type == "train") %>%
      tsibble(index = index, key = site) %>%
      model(
        arima_dad_agg = ARIMA(
          occ ~
            days_ +
            ad_diff_f_lag3 +
            ad_diff2_f_lag3 +
            ad_diff_f_lag6 +
            ad_diff2_f_lag6
        )
      ) %>%
      reconcile(
        arima_dad_rec = min_trace(arima_dad_agg, method = "mint_shrink")
      ) %>%
      select(-arima_dad_agg) %>%
      forecast(
        new_data = data_xpredict_wgh %>%
          filter(type == "test") %>%
          tsibble(index = index, key = site)
      )

    # Vector autoregressive models
    # (BRI/Shouthmead)
    fc_fable_var_ad2_bs <- # ad diff2 filtered
      data_xpredict_bs %>%
      filter(type == "train", !is_aggregated(site)) %>%
      mutate(site = site %>% as.character()) %>%
      tsibble(index = index, key = site) %>%
      model(
        var_ad2 = VAR(vars(occ, ad_diff2_f) ~ season(period = "week"))
      ) %>%
      forecast(h = horizon)

    fc_fable_var_paed_bs <- # A&E paed
      data_xpredict_bs %>%
      filter(type == "train", !is_aggregated(site)) %>%
      mutate(site = site %>% as.character()) %>%
      tsibble(index = index, key = site) %>%
      model(
        var_paed = VAR(vars(occ, paed) ~ season(period = "week"))
      ) %>%
      forecast(h = horizon)

    fc_fable_var_h_bs <- # BRI vs Southmead
      data_xpredict_bs %>%
      filter(type == "train", !is_aggregated(site)) %>%
      mutate(site = site %>% as.character()) %>%
      tsibble(index = index, key = site) %>%
      select(occ) %>%
      pivot_wider(names_from = site, values_from = occ) %>%
      model(
        var_h = VAR(vars(BRI, Southmead) ~ season(period = "week"))
      ) %>%
      forecast(h = horizon)

    # WGH
    fc_fable_var_ad_wgh <- # ad diff filtered
      data_xpredict_wgh %>%
      filter(type == "train", site == "WGH") %>%
      mutate(site = site %>% as.character()) %>%
      tsibble(index = index, key = site) %>%
      model(
        var_ad = VAR(vars(occ, ad_diff_f) ~ season(period = "week"))
      ) %>%
      forecast(h = horizon)

    fc_fable_var_ad2_wgh <- # ad diff2 filtered
      data_xpredict_wgh %>%
      filter(type == "train", site == "WGH") %>%
      mutate(site = site %>% as.character()) %>%
      tsibble(index = index, key = site) %>%
      model(
        var_ad2 = VAR(vars(occ, ad_diff2_f) ~ season(period = "week"))
      ) %>%
      forecast(h = horizon)

    fc_fable_var_h_wgh <- # BS vs WGH
      data_xpredict_wgh %>%
      filter(type == "train", !is_aggregated(site)) %>%
      mutate(site = site %>% as.character()) %>%
      tsibble(index = index, key = site) %>%
      select(occ) %>%
      pivot_wider(names_from = site, values_from = occ) %>%
      model(
        var_h = VAR(vars(BS, WGH) ~ season(period = "week"))
      ) %>%
      forecast(h = horizon)

    fc_var_bs <- # bind VAR fc (BRI/Southmead)
      map(
        list(
          fc_fable_var_ad2_bs,
          fc_fable_var_paed_bs
        ),
        \(.x) {
          .x %>%
            as_tibble() %>%
            mutate(
              occ = dist_normal(
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
          fc_fable_var_h_bs %>% pluck(".distribution") %>% dimnames(),
          \(.x, .y) {
            fc_fable_var_h_bs %>%
              as_tibble() %>%
              mutate(
                occ = dist_normal(
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

    fc_var_wgh <- # bind VAR fc (WGH)
      map(
        list(
          fc_fable_var_ad_wgh,
          fc_fable_var_ad2_wgh
        ),
        \(.x) {
          .x %>%
            as_tibble() %>%
            mutate(
              occ = dist_normal(
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
          fc_fable_var_h_wgh %>% pluck(".distribution") %>% dimnames(),
          \(.x, .y) {
            fc_fable_var_h_wgh %>%
              as_tibble() %>%
              mutate(
                occ = dist_normal(
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
          bind_rows() |> 
          filter(site == "WGH")
      )

    # Random forest - interaction
    # Bri/Southmead
    list_var_rf_bs <- # list predictors (BRI/Southmead)
      data_xpredict_bs %>%
      select(
        contains("occ"),
        contains("paed"),
        -paed,
        contains("los"),
        -los,
        contains("tvar"),
        -tvar,
        matches("ad_diff.*_f"),
        -ad_diff_f,
        -ad_diff2_f,
        contains("days_"),
        -days_
      ) %>%
      names()

    data_xpredict_int_bs <- # long format data with lag column (BRI/Southmead)
      data_xpredict_bs %>%
      select(type, site, index, all_of(list_var_rf_bs)) %>%
      rename_with(~ sub("occ_lag", "occ_same_lag", .x)) %>%
      rename_with(~ sub("_lag", "-lag", .x, fixed = TRUE)) %>%
      pivot_longer(
        cols = c(contains("lag")),
        names_to = c(".value", "lag"),
        names_sep = "-"
      )

    rf_int_par_bs <- # forecast (returns distribution parameters) ...
      rf_reg_int(
        data_xpredict_int_bs %>% filter(!is_aggregated(site)),
        horizon
      )

    fc_rf_int_bs <- # harmonise fc to fable ...
      harmonise_fc_par(rf_int_par_bs, data_xpredict_bs, "rf_int") %>%
      as_tsibble(index = index, key = c("site", ".model"))
    
    # WGH
    list_var_rf_wgh <- # list predictors (WGH)
      grep("paed|los", list_var_rf_bs, value = TRUE, invert = TRUE)

    data_xpredict_int_wgh <- # long format data with lag column (WGH)
      data_xpredict_wgh %>%
      select(type, site, index, all_of(list_var_rf_wgh)) %>%
      rename_with(~ sub("occ_lag", "occ_same_lag", .x)) %>%
      rename_with(~ sub("_lag", "-lag", .x, fixed = TRUE)) %>%
      pivot_longer(
        cols = c(contains("lag")),
        names_to = c(".value", "lag"),
        names_sep = "-"
      )

    rf_int_par_wgh <- # forecast (returns distribution parameters) ...
      rf_reg_int(
        data_xpredict_int_wgh %>% filter(!is_aggregated(site)),
        horizon
      )

    fc_rf_int_wgh <- # harmonise fc to fable ...
      harmonise_fc_par(rf_int_par_wgh, data_xpredict_wgh, "rf_int") %>%
      as_tsibble(index = index, key = c("site", ".model")) |> 
      filter(site == "WGH")

    

    # XGBoosting - interaction
    # Same predictors as rf interaction but without paed and los
    # BRI/Southmead
    data_xpredict_int_xgb_bs <-
      data_xpredict_int_bs %>% select(-c(paed, los))

    xgb_par_bs <- # forecast (returns distribution parameters)...
      xgb_reg_int(
        data_xpredict_int_xgb_bs %>% filter(!is_aggregated(site)),
        horizon
      )

    fc_xgb_bs <- # harmonise fc to fable ...
      harmonise_fc_par(xgb_par_bs, data_xpredict_bs, "xgb") %>%
      as_tsibble(index = index, key = c("site", ".model"))
  
    # WGH
    data_xpredict_int_xgb_wgh <- data_xpredict_int_wgh

    xgb_par_wgh <- # forecast (returns distribution parameters)...
      xgb_reg_int(
        data_xpredict_int_xgb_wgh %>% filter(!is_aggregated(site)),
        horizon
      )

    fc_xgb_wgh <- # harmonise fc to fable ...
      harmonise_fc_par(xgb_par_wgh, data_xpredict_wgh, "xgb") %>%
      as_tsibble(index = index, key = c("site", ".model")) |> 
      filter(site == "WGH")
    

    # Join fc
    dimnames(fc_var_bs$occ) <- "occ" # add name to column to match fc_fable
    dimnames(fc_var_wgh$occ) <- "occ" # add name to column to match fc_fable
    dimnames(fc_rf_int_bs$occ) <- "occ"
    dimnames(fc_rf_int_wgh$occ) <- "occ"
    dimnames(fc_xgb_bs$occ) <- "occ"
    dimnames(fc_xgb_wgh$occ) <- "occ"
    fc_all <-
      list(
        fc_fable_rec_bs, filter(fc_fable_rec_wgh, site == "WGH"), 
        fc_var_bs, fc_var_wgh, 
        fc_rf_int_bs, fc_rf_int_wgh,
        fc_xgb_bs, fc_xgb_wgh
      ) %>%
      reduce(bind_rows) %>%
      select(site, .model, index, occ, .mean)

    # Remove aggregated fc
    fc_all <-
      fc_all %>%
      filter(!is_aggregated(site)) %>%
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
    
    fc_comb_crps <-
      lcomb_fun(.fc, .weights, .list_models, "none", "equal")
    
    dimnames(fc_comb_crps$occ) <- "occ"
    
    .fc <- 
      list(.fc, fc_comb_crps) %>% 
      reduce(bind_rows) %>%  as_fable(".mean", "occ")
    
    # Return fc
    .fc
  }


#' Prepare target output (predictions)
#' Tag and save the forecast ensemble. Threshold and risk-of-crossing are
#' intentionally *not* computed here -- the threshold is a value the Shiny
#' user types in, so it is computed client-side (see shinyApp/shiny-
#' functions.R: compute_threshold_default() and compute_risk()) from the
#' occ_mean/occ_var summary stats written to the database. The target
#' output only needs to carry the forecast itself.
#' @param .fc Forecast ensemble (fcc_file)
prepare_output_pred <-
  function(.fc) {
    .fc <- as.data.table(.fc)
    .fc[, type := "forecast"]
    .fc[, date_fc := lubridate::today()]

    target_output_path <- file.path(save_path, "output", paste0("model_out_flat", ".RDS"))

    if (!file.exists(dirname(target_output_path))) {
      dir.create(dirname(target_output_path), recursive = TRUE)
    }

    saveRDS(.fc, target_output_path)

    # Return path
    target_output_path
  }


#' Prepare target output (historical data)
#' @param  .table RDS file containing historical data
prepare_output_hist <-
  function(.table) {
    # Join hospital data
    .table <-
      bind_rows(
        .table$bs |> filter(!is_aggregated(site)),
        .table$wgh |> filter(site == "WGH")
      ) |>
      mutate(site = as.character(site))

    output <- .table

    output$site <- as.character(output$site)
    target_output_path <- file.path(
      save_path,
      "output",
      paste0("historic_data", ".RDS")
    )
    if (!file.exists(dirname(target_output_path))) {
      dir.create(dirname(target_output_path), recursive = TRUE)
    } else if (!file.exists(dirname(target_output_path))) {
    file.remove(target_output_path)  
    saveRDS(output, target_output_path)
    }

    # Return list
    target_output_path
  }