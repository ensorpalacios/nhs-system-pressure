#' Helper functions
#'
#' Include all helper functions (functions used in multiple scripts or too 
#' lengthy); includes functions for:
#' - Split/augment (lag) data for fitting and testing.
#' - Plot functions
#' - Utility functions
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-04-23

# Functions to split and augment data for analysis -----------------------------
#' Create lagged data
#' @param .data The data as a tsibble
#' @param .lag Number of lags
#' @export
lag_fun <- 
  function(.data, .lag = 7) {
    .lag = .lag + 1 # include no lag
    sites = .data$site %>% unique()
    
    map(sites, \(.site) {
      tmp_data = # site specific data
        .data %>% filter(site == .site)
      
      # Days (one hot encoding)
      tmp_days =
        tmp_data %>% 
        model.matrix(~ 0 + days_, data = .) # 0 for no intercept/ref
      
      # Lagged variables (mode.matrix not working with multiple cat variables)
      tmp_lagged = # design matrix
        tmp_data %>% as_tibble() %>% 
        select(-c(index, site, days_, t_ax)) %>% 
        relocate(occ) %>% 
        as.data.table() %>% one_hot() %>% as.matrix()
       # names() %>% 
       #  paste(collapse = "+") %>% paste("~ 0 +", .) %>% formula() %>% 
       #  model.matrix(data = tmp_data)

      tmp_lag_names = # name lagged variables
        tmp_lagged %>% dimnames %>% .[[2]]
      tmp_lag_names = 
        map(seq(.lag), \(.nlag) {
          if (.nlag == 1) { 
            tmp_lag_names
          } else {
            # str_c(tmp_lag_names, "_lag", .nlag - 1)
            paste0(tmp_lag_names, "_lag", .nlag - 1)
          }
        }) %>% 
        unlist()
      
      tmp_lagged = # expand time
        embed(tmp_lagged, .lag)
      
      colnames(tmp_lagged) =
        tmp_lag_names
      
      # Full df
      tibble(
        tmp_data %>% 
          slice(.lag:n()) %>% 
          select(index, site, t_ax, days_),
        tmp_lagged %>% as_tibble(),
        tmp_days %>% as_tibble() %>%  slice(.lag:n())
      )
    }) %>% 
      list_rbind()
  }

#' train-test split
#' @param .ts_occ The data as a tsibble
#' @param .len_test Length of the test set in months as string (leave 5 months)
#' @export
split_tt <- 
  function(.ts_occ, .len_test) {
    sites = .ts_occ$site %>% unique()
    map(sites, \(.site) {
      # Split data
      tmp_split = .ts_occ %>%
        as_tibble() %>% # (for using time_series_split)
        dplyr::filter(site == .site) %>%
        timetk::time_series_split(
          date_var = index, 
          assess = .len_test, # length test set
          cumulative = TRUE)
      
      # Extract training set
      tmp_train =
        tmp_split %>% 
        rsample::training() %>% 
        mutate(type = "train", .before = 1)
      
      # Extract test set
      tmp_test = 
        tmp_split %>%
        rsample::testing() %>% 
        mutate(type = "test", .before = 1)
      
      # Return training and test set as list
      bind_rows(tmp_train, tmp_test)
    }) %>% 
      bind_rows() %>% 
      relocate(site, .before = 2)
  }

#' cross-validation split
#' @param .ts_occ_tt The data set split in train/test set by split_data_tt
#' @param .initial Length testing sets
#' @param .assess Length validation sets
#' @param .skip Separation between consecutive training starting dates
#' @export
split_cv <- 
  function(.ts_occ_tt, .initial, .assess, .skip) {
    sites = .ts_occ_tt$site %>% unique()
    map(sites, \(.site) {
      tmp_split = # split with re-sampling
        .ts_occ_tt %>% 
        dplyr::filter(type == "train", site == .site) %>% 
        timetk::time_series_cv(
          date_var = index,
          initial = .initial, # (length training set)
          assess = .assess, # (length validation set)
          skip = .skip # (separation between training sets)
        )
      
      tmp_split = # combine training and validation sets in tibble
        map2(tmp_split$splits, tmp_split$id, \(.split, .name) {
          .name = .name %>% str_sub(-2)
          tmp_train = 
            .split %>% 
            rsample::training() %>% 
            mutate(split = .name, .before = 1)
          tmp_assess = 
            .split %>% 
            rsample::testing() %>% 
            mutate(split = .name, .before = 1, type = "test")
          bind_rows(tmp_train, tmp_assess)
        }) %>% 
        bind_rows
    }) %>% 
      bind_rows() %>% # (combine sites)
      relocate(site, .before = 3)
  }

#' extract site- and split-specific training data from cross-validated
#' train-validation sets (split_data_cv)
#' @param .data_sel Data 
#' @param .site List of hospitals
#' @param .split List of cv splits
#' @param .vars List of y/x variables in the model
#' @param .type Character indicating whether to include all or only train data
#' @export
select_training <- # select training set (by site and split)
  function(.data_sel, .site, .split, .vars = NULL, .type = "train", ...) {
    # .data_sel is list (site) of list (split) of training sets 
    # .vrs is list of outcome (..1) and predictors
    # .type specifies filter (only training or training/test data)
    if (is.null(.vars)) stop(".vars is null; must match exact colnames")

    if (.type == "all") {
    .data_sel %>% 
      filter(site == .site, split == .split) %>% 
      select(
        all_of(.vars %>% append("type", after = 0))
        )
    } else if (.type == "train") {
    .data_sel %>% 
      filter(type == "train", site == .site, split == .split) %>% 
      select(all_of(.vars))
    }
  }


#' extract site- and split-specific trained models from cross-validated
#' train-validation sets (split_data_cv)
#' @param .data Data 
#' @param .site List of hospitals
#' @param .split List of cv splits
select_model <- # select model fit (by site and split)
  function(.data, .site, .split, ...)  {
    # .data is list (site) of list (split) of model fits
    # attention: data_xpredict defined globally; leave this here!
    .data_xpredict = list(...)$xpredict
    list(
      "model" = .data[[.site]][[.split]],
      "index" = .data_xpredict %>% filter(site == .site, split == .split)
    )
  }



#' extract site-, split- and model-specific forecast set from cross-validated
#' train-validation sets (split_data_cv)
#' @param .data Data 
#' @param .site List of hospitals
#' @param .split List of cv splits
#' @param .models List of models from which to generate forecasts
#' @export
select_fc <- # select forecast (by site and split) - used in cv_wrap()
  function(.data, .site, .split, .models, ...) {
    # .data is list (site) of list (splits) of list with
    # $all full data and $fc forecast;
    # .model is a list of model whose forecasts are plotted
    list(
      "all" = 
        .data$all %>% 
        filter(site == .site, split == .split) %>% 
        as_tsibble(index = index),
      "fc" =
        .data$fc %>% 
        filter(site == .site, .model %in% .models, split == .split)
    )
  }


#' Select smoothed dataset
#' Used in fit-fc-trend.R.
#' @param .data List of smoothed bed occupancy training sets per site and split.
#' @param .site Site of split.
#' @param .split Cv split of split.
select_smooth <- 
  function(.data_, .site, .split, ...) {
    modifyList(pluck(.data_, .site, .split), list(site = .site, split = .split))
  }


#' wrapper over site- and cv split-specific data
#' @param .data Data organised by site and splits
#' @param .select Function to select data to pass to .function
#' @param .function Function to apply to subset of data
#' @param ... Parameters for .select function
#' @export
cv_wrap <-
  function(.data, .select, .function, ...) { 
    if (tibble::is_tibble(.data)) {
      sites = .data$site %>% unique()
      splits = .data$split %>% unique()
    } else if ("fc" %in% names(.data)) {
      sites = flatten(.data) %>% .$site %>% unique()
      splits = flatten(.data) %>% .$split %>% unique()
    } else if (is.list(.data)) {
      sites = .data %>% names()
      splits = .data[[1]] %>% names()
    } else stop(".data not a tibble or list")
    
    map(sites, \(.site) {
      map(splits, \(.split) {
        .select(.data, .site, .split, ...) %>% 
          .function(...)
      }) %>% set_names(splits)
    }) %>% set_names(sites)
  }



# Functions to compute/harmonise fc to fable data structure --------------------
#' Compute ETS fc
#' Compute forecasts for error trend seasonal model and harmonise fc to fable
#' data structure.
#' @param .data Includes observed occ and es models
#' @param ... Includes horizon (number of h forecasts)
es_forecast <- # define esx forecast
  function(.data, ...) {
    .horizon = list(...)$hrz
    
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    tmp_fc = # compute forecasts
      forecast(
        .data$model, h = .horizon,
        interval = "prediction",
        level = .95,
        newdata = .data$index
      )
    tmp_fc = # add forecast distributions
      reduce(
        list(
          # .data$index %>% select(split, site, type, index),
          .data$index,
          tmp_fc$mean %>% as.data.frame() %>% set_names("mean"), 
          tmp_fc$lower %>% as.data.frame(), 
          tmp_fc$upper %>% as.data.frame()
        ),
        cbind
      ) %>% 
      mutate( # create forecast distribution (assuming nid error)
        .model = "es",
        occ = dist_normal(mean, sd = (`Upper bound (97.5%)` - mean) / 1.96),
        .mean = mean, # necessary for fable::autoplot
        mean = NULL,
        `Lower bound (2.5%)` = NULL,
        `Upper bound (97.5%)` = NULL
      )
  }

 
#' Harmonise rf fc
#' Harmonise forecasts for random forest model to fable data structure.
#' @param .data Includes observed occ and rf fc parameters.
#' @param ... Includes name of model for saving fc.
rf_forecast = 
  function(.data, ...) {
    # Function to harmonise fc to fable data structure
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    .data$model = 
      .data$model %>% map_dfr(~ as.data.frame(.x)) %>% as_tibble()
    tmp_fc =
      bind_cols(.data$model, .data$index) %>% 
      mutate(
        .model = "rf",
        .mean = mean, # necessary for fable::autoplot
        occ = dist_normal(mu = mean, sd = sd),
        mean = NULL,
        sd = NULL
      ) %>%
      relocate(c(.model, occ, .mean), .after = index)
  }


#' Harmonise rf-interaction fc
#' Harmonise forecasts for random forest with interaction model to fable data
#' structure.
#' @param .data Includes observed occ and rf fc parameters.
#' @param ... Includes name of model for saving fc.
rf_forecast_int =
  function(.data, ...) {
    # Function to harmonise fc to fable data structure
    nmodel = list(...)$nmodel # name of model
    
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    .data$model = 
      .data$model %>% as_tibble()
    tmp_fc =
      bind_cols(.data$model, .data$index) %>% 
      mutate(
        .model = nmodel,
        .mean = mean, # necessary for fable::autoplot
        occ = dist_normal(mu = mean, sd = sd),
        mean = NULL,
        sd = NULL
      ) %>%
      relocate(c(.model, occ, .mean), .after = index)
  }


#' Harmonise xgb fc
#' Harmonise forecasts for XGB model to fable data structure.
#' @param .data Includes observed occ and rf fc parameters.
#' @param ... Includes name of model for saving fc.
xgb_forecast =
  function(.data, ...) {
    # Function to harmonise fc to fable data structure
    nmodel = list(...)$nmodel # name of model
    
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    .data$model = 
      .data$model %>% as_tibble()
    tmp_fc =
      bind_cols(.data$model, .data$index) %>% 
      mutate(
        .model = nmodel,
        .mean = mean, # necessary for fable::autoplot
        occ = dist_normal(mu = mean, sd = sd),
        mean = NULL,
        sd = NULL
      ) %>%
      relocate(c(.model, occ, .mean), .after = index)
  }



# Plots ------------------------------------------------------------------------
#' Correlation plot function; generate plot with acf and pacf
#' @param ts_tbl tbl with time series
#' @param ... used to specify variable to plot, number of lags, alpha value
#' @export
# ACF/PCF plot function
plot_cf = function(ts_tbl, .var = NULL, .lag = 50, .alpha = 0.05){
  # Compute acf and pac.alpha
  tmp_acf = ts_tbl |> ACF(!!as.symbol(.var), lag_max = .lag)
  corfun = "acf"
  tmp_pacf = ts_tbl |> PACF(!!as.symbol(.var), lag_max = .lag)
  corfun = "pacf"

  # Confidence interval
  ci_lim = qnorm((1 + (1 - .alpha)) /2) / sqrt(nrow(ts_tbl) / 2)

  # Generate plot
  plt_acf = tmp_acf |>
    ggplot(aes(x = lag, y = acf)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ci_lim), linetype = 2, colour = 'blue') +
    geom_hline(aes(yintercept = -ci_lim), linetype = 2, colour = 'blue') +
    facet_wrap(
      ~ site,
      nrow = 2,
      scales = "free_y") +
    labs(x = "lag (days)")
  plt_pacf = tmp_pacf |>
    ggplot(aes(x = lag, y = pacf)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ci_lim), linetype = 2, colour = 'blue') +
    geom_hline(aes(yintercept = -ci_lim), linetype = 2, colour = 'blue') +
    facet_wrap(
      ~ site,
      nrow = 2,
      scales = "free_y") +
    labs(x = "lag (days)")
  plt_acf + plt_pacf + plot_layout(axis_title="collect")
}


#' Brighten colours
#' Fake increase alpha value of colour palette by mixing foreground colours with
#' background white. Useful to plot in formats that do not support transparency.
#' @param .palette Original colour palette (for models)
#' @param .alpha Faked alpha value
bright_col <- 
  function(.palette, .alpha) {
    .palette = col2rgb(.palette) / 255
    .white = col2rgb("#FFFFFF") / 255
    
    apply(.palette, 2, \(.col) {
      .col = .col * .alpha + .white * (1 - .alpha)
      # paste0(rgb(.col[1], .col[2], .col[3]), "FF")
      rgb(.col[1], .col[2], .col[3])
    })
  }


#' Save html from html
#' Save tables as html to produce colours, then save pdf from html
#' @param .file File to save
#' @param .path Path where to save
html2pdf <- 
  function(.file, .path) {
    html_file <- paste0(.path, ".html")
    pdf_file <- paste0(.path, ".pdf")
    .file %>% gtsave(filename = html_file)
    chrome_print(html_file, pdf_file)
  }


#' Plot forecasts
#' Plot fc for each site and split
#' @param .data Data containing observed ("all") and fc occ ("fc").
plot_forecast <- # plot forecast function
  function(.data, ...) {
    # Reduce length observations (x axis)
    .data$all = 
      .data$all %>% 
      group_by(split, type, site) %>% 
      mutate(
        start_index = 
          case_when(
            type == "train" ~
              head(index, 1) + 
              (tail(index, 1) - head(index, 1)) / 1.7,
            type == "test" ~
              head(index, 1)
          )
      ) %>% 
      ungroup() %>% 
      filter(index >= start_index)
    
    # Plot
    if (!list(...)$trend) { # for normal fc
      .data$fc %>%
        autoplot() +
        autolayer(.data$all, .vars = occ) +
        scale_colour_manual(name = "models", values = col_models) + 
        scale_fill_manual(name = "models", values = col_models) + 
        scale_y_continuous(breaks = c(600, 700)) +
        facet_wrap(vars(.model), ncol = 1, strip.position = "right")
    } else { # for trend fc
      .data$fc %>% as_fable("occ_wm", "occ_wm") %>% 
        autoplot() +
        autolayer(.data$all, .vars = occ_wt) +
        autolayer(.data$fc %>% as_tsibble(), .vars = occ_s)
    }
  }


# Utility functions ------------------------------------------------------------
#' Z-score data
#' @param  .data time series (vector)
#' @export
zs_fun <- 
  function(.data) {
    (.data - mean(.data, na.rm = TRUE)) / sd(.data, na.rm = TRUE)
    }


#' Stabilise time series
#' regression (optional)
#' @param .var variable to detrend (time series/vector)
#' @param .index time index in y-m-d (time series/vector)
#' @param .xdays whether to remove effect of special days (e.g., Christmus)
#' @param .wdays whether to remove effect of week days
#' @param .stationary whether to make .var stationary
#' @export
stabilise <- 
  function(.var, .index, .xdays = FALSE, .wdays = FALSE, .stationary = FALSE) {
    # Prepare data
    tmp_data = 
      tibble(index = .index, var = .var) %>% 
      mutate(
        var_mean = mean(var),
        var_demean = var - var_mean
      ) %>% 
      as_tsibble(index = index)
    
    # Remove effects of special days
    if (.xdays) {
      christmus_period = 
        .index %>% base::format("%Y") %>% unique() %>%  # years in data
        map(\(.year) {
          seq(
            as.Date(str_glue("{.year}-12-24")), 
            as.Date(str_glue("{.year}-12-26")),
            by = "days")
        }) %>% 
        purrr::reduce(c)
      
      tmp_data = 
        tmp_data %>% 
        mutate(
          christmus = 
            case_when(index %in% christmus_period ~ 1, .default = 0) %>%
            ksmooth(index, ., kernel = "normal", bandwidth = 5) %>% .$y
        )
      
      fit_xdays =
        tmp_data %>% model(TSLM(formula("var ~ christmus"))) %>%
        coef() %>% filter(term == "christmus") %>% select(estimate) %>% pull()

      tmp_data =
        tmp_data %>% 
        mutate(var = var - c(christmus * fit_xdays),
               var_mean = mean(var), # new data mean
               var_demean = var - var_mean) # new mean-subtracted data 
    }
    
    # Remove effects of week days
    if (.wdays) {
      fit_wdays =
        tmp_data %>% 
        mutate(
          wdays = factor(weekdays(index))
        ) %>% 
        model(lm = TSLM(var_demean ~ wdays)) %>% # fit on mean-subtracted data
        residuals() %>% pull(.resid)
      
      tmp_data =
        tmp_data %>% 
        mutate(var = fit_wdays + var_mean, # add back the mean 
               var_demean = fit_wdays) # new mean-subtracted data 
    }
    
    # Make stationary
    if (.stationary) {
      is_stationary =
        tmp_data %>% features(var, unitroot_kpss) %>% 
        select(kpss_pvalue) %>% {(pull(.) > 0.05)}
      while (!is_stationary) {
        if (tmp_data %>% features(var, unitroot_nsdiffs) %>% pull() > 0) {
          tmp_data = tmp_data %>% mutate(var = difference(var, 7))
        }
        if (tmp_data %>% features(var, unitroot_ndiffs) %>% pull() > 0) {
          tmp_data = tmp_data %>% mutate(var = difference(var))
        }
        is_stationary =
          tmp_data %>% features(var, unitroot_kpss) %>% 
          select(kpss_pvalue) %>% {(pull(.) > 0.05)}
      }
      tmp_data =
        tmp_data %>% # fill NA with mean
        mutate(var = replace_na(var, mean(var, na.rm = TRUE)))
    }
    
    return(tmp_data %>% pull(var))
  }


#' Silent function
#' @param .fun function to run without plots
#' @export
save_plot <- 
  function(.fun) {
    dev.new(width = 10, height = 10) # open null graphic device (big enough!)
    .fun
    tmp_plot = recordPlot()
    dev.off()
    tmp_plot
  }


#' Predict regressor function
#' Predict regressors using tslm, snaive, arima, ets model; 
#' replace test values for different lags appropriately (lag 0 replace 
#' all -- lag7 replace none): use predicted if t = T + h, else use observed, 
#' where t is actual time, T is last time of test data (.idx_test - 1) and 
#' h is forecast horizon.
#' @param .data tibble with cv splits and sites as groups
#' @param .var list of variables to predict
#' @param .idx_test index where test data start
#' @param .type choose model for x-predictions
xpredict_fun <- 
  function(.data, .var, .idx_test, .type) {
    .data =
      .data %>% 
      group_by(split, site) %>% 
      mutate(
        across(
          c(contains(.var), -contains("lag")),
          ~ {
            # Train and test data
            x_ts =
              tibble(index, split, type, site, y = .x, days_,)
            
            x_predict = 
              x_ts %>%
              filter(type == "train") %>%
              as_tsibble(index = index, key = c(split, site))
            
            x_test =
              x_ts %>%
              filter(type == "test") %>%
              as_tsibble(index = index, key = c(split, site))
            
            if (.type == "tslm") {
              x_predict =  
                x_predict %>% model(xmodel= TSLM(y ~ trend() + days_))
              
            } else if (.type == "snaive") {
              x_predict =  x_predict %>% model(xmodel= SNAIVE(y ~ lag("week")))
              
            } else if (.type == "arima") {
              x_predict =  x_predict %>% model(xmodel= ARIMA(y ~ days_))
              
            } else if (.type == "ets") {
              x_predict =  x_predict %>% model(xmodel= ETS(y))
              
            } else if (.type == "pull") {
              x_predict =  x_predict %>% 
                model(
                  tslm = TSLM(y ~ trend() + days_),
                  ets = ETS(y), 
                  arima = ARIMA(y ~ days_)
                  ) %>% 
                mutate(xmodel = (tslm + ets + arima) / 3)
            }
            
            x_predict = 
              x_predict %>%
              forecast(new_data = x_test) %>% 
              filter(.model == "xmodel")
            
            # Replace true with predicted (all)
            .x[x_ts$type == "test"] = round(x_predict$.mean, 2)
            .x
          },
          .names = "{.col}_predicted"
        ),
        across( # replace predicted with observed when t = T + h
          c(contains(.var), -contains("predicted")),
          ~ {
            tmp_lag = 
              tryCatch({
                cur_column() %>% str_sub(start = -1) %>% as.numeric()
              }, warning = function(w) 0)
            
            
            if (n() - .idx_test - tmp_lag >= 0) {
              tmp_horizon = n() - .idx_test - tmp_lag
              
              tmp_predicted =
                gsub("_lag.*", "", cur_column()) %>%
                paste0("_predicted") %>%
                get()
              tmp_predicted =
                tmp_predicted[.idx_test: (.idx_test + tmp_horizon)]
              
              .x[row_number() >= (.idx_test + tmp_lag)] =
                tmp_predicted
              .x
            } else {
              .x
            }
          }
        ),
        across(
          contains("predicted"),
          ~ {.x = NULL}
        )
      ) %>% 
      ungroup()
  }


#' Recode sites
#' Recode from aggregate (yes/no) to simple character
#' @param .tbl Tibble containing data organised by site
rec_site <- 
  function(.tbl) {
    .tbl %>% 
      mutate(
        site = if_else(is_aggregated(site), "aggregate", site),
        site = site %>% as.character()
      )
  }



# Functions to fit models and produce fc ---------------------------------------
#' Random forest fit and predict
#' Random forest for forecast. Train RF on data in wide format: lagged data have
#' their own column. Additionally predict bed occupancy for the next 7 days;
#' easier to train and predict in one for loop. Finally, extract forecast
#' parameters (i.e., expected forecast values) and compute confidence interval
#' using average root mean squared error from oob data. Use predictors of
#' increasing lag to predict bed occupancy with increasing time horizon h.
#' @param .data_rf tibble of data
#' @param .temp/tmp1 unused (used in select_training)
#' @param .horizon forecast horizon
rf_reg <- 
  function(.data_rf, .tmp, .tmp1, .horizon) {
    # Train set
    data_train = 
      .data_rf %>% filter(type == "train")
    y_train = 
      data_train %>% select(occ)
    xl_train = # lagged data
      data_train %>%  select(contains("lag"))
    xd_train = # days
      data_train %>% select(starts_with("days_"))
    
    # Test set
    data_test = 
      .data_rf %>% filter(type == "test")
    y_test = 
      data_test %>% select(occ)
    xl_test = # lagged data 
      data_test %>%  select(contains("lag"))
    xd_test = # days
      data_test %>% select(starts_with("days_"))
    
    # Loop over horizons
    rf_save = 
      map(seq(.horizon), \(.h) {
        # Select data
        tmp_lag = str_glue("lag{.h}")
        tmp_train = 
          bind_cols(y_train, xl_train %>% select(ends_with(tmp_lag)), xd_train)
        tmp_test = 
          bind_cols(y_test, xl_test %>% select(ends_with(tmp_lag)), xd_test) %>% 
          slice(.h) # predict only the .h day ahead from last observed .lag days
        
        # Compute
        tmp_fit = randomForest(occ ~ ., data = tmp_train, ntree = 7000)
        tmp_fc = predict(tmp_fit,  tmp_test, predict.all = TRUE)
        
        # Forecast parameters
        tmp_par = 
          list(
            "mean" = tmp_fc$aggregate, 
            "sd" = tmp_fit$mse %>% sqrt() %>% mean() # from oob errors
          )
        
        # Return as list
        tmp_ls = list("fit" = tmp_fit, "fc" = tmp_fc, "par" = tmp_par)
      }) %>% 
      set_names(seq(.horizon))
  }


#' Random forest - interaction
#' Random forest for forecast with interaction for lag. Train RF on data in 
#' long format: lagged data are stacked and paird with lag column; let the
#' random forests learn the interaction between lag and var_lagged. Additionally
#' predict bed occupancy for the next 7 days; easier to train and predict in 
#' one for loop. Finally, extract forecast parameters (i.e., expected forecast
#' values) and compute confidence interval using average root mean squared 
#' error from oob data.
#' @param .data_rf tibble of data
#' @param .temp/tmp1 unused (used in select_training)
#' @param .horizon forecast horizon
rf_reg_int <- 
  function(.data_rf, .tmp, .tmp1, .horizon) {
    # Train set
    data_train = 
      .data_rf %>% filter(type == "train") %>% select(-type)
    
    # Test set
    data_test = 
      .data_rf %>% filter(type == "test") %>% select(-type)
    
    # Compute
    tmp_fit = randomForest(occ ~ ., data = data_train, ntree = 1000)
    tmp_fc = predict(tmp_fit,  data_test, predict.all = TRUE)
    
    # Group fc by lag
    tmp_fc_individuals = tmp_fc$individual %>% as_tibble()
    max_lag = data_test$lag %>% parse_number() %>% max()
    tmp_fc_individuals$lag = rep(seq(.horizon), each = max_lag)
    tmp_fc_individuals = 
      map(tmp_fc_individuals$lag %>% unique(), \(.lag) {
        tmp_fc_individuals %>% filter(lag == .lag) %>% select(-lag) %>% 
          unlist() %>% t() %>% as_tibble()
      }) %>% 
      list_rbind()
    
    tmp_par = 
      list(
        "mean" = tmp_fc_individuals %>% rowMeans(), 
        "sd" = tmp_fit$mse %>% sqrt() %>% mean() # from oob errors
      )
    
    tmp_ls = list("fit" = tmp_fit, "fc" = tmp_fc_individuals, "par" = tmp_par)
  }



#' XGBoost - interaction
#' @param .data_rf tibble of data
#' @param .temp/tmp1 unused (used in select_training)
#' @param .horizon forecast horizon
xgb_reg_int <- 
  function(.data_rf, .tmp, .tmp1, .horizon) {
    # Quantile loss function (grad and hess)
    qreg = 
      function(.alpha) {
        function(preds, dtrain) {
          tmp_labels = getinfo(dtrain, "label")
          pe = tmp_labels - preds
          grad = if_else(pe < 0, 1 - .alpha, -.alpha)
          hess = rep(1, length(grad))
          list(grad = grad, hess = hess)
        }
      }
    
    # Train set
    data_train =
      .data_rf %>%
      filter(type == "train") %>% select(-type)
    data_train =
      xgb.DMatrix(
        data = sparse.model.matrix(occ ~ . - 1, data = data_train),
        label = data_train %>% pull(occ)
      )
    
    # Test set
    data_test = 
      .data_rf %>% 
      filter(type == "test") %>% select(-type)
    max_lag = # save for later
      data_test$lag %>% parse_number() %>% max()
    data_test =
      xgb.DMatrix(
        data = sparse.model.matrix(occ ~ . - 1, data = data_test),
        label = data_test %>% pull(occ)
      )
    
    # Fit and forecast
    # for -1/+1 sd (assuming normal distribution)
    fc =
      map(c("1q" = .25, "mu" = 0.5, "3q" = .75), \(.alpha){ 
        if (.alpha == 0.5) {
          params = 
            list(
              objective = "reg:squarederror", # for mean forecasting
              eta = 0.3, # default learning rate
              gamma = 0, # default min loss reduction for leaf split
              lambda = 1, # default L2 regularisation
              alpha = 0, # default L1 regularisation
              base_score = # initialise predictions based on sample mean
                getinfo(data_train, "label") %>% mean()
            )
        } else {
          params = 
            list(
              objective = qreg(.alpha), # for quantile forecasting
              eta = 0.3, # default learning rate
              gamma = 0, # default min loss reduction for leaf split
              lambda = 1, # default L2 regularisation
              alpha = 0, # default L1 regularisation
              base_score = # initialise predictions based on sample mean
                getinfo(data_train, "label") %>% mean()
            )
        }
        tmp_fit =
          xgb.train(
            params,
            data_train,
            nrounds = 150 # max number of boosting interactions
          )
        
        tmp_fc = predict(tmp_fit,  data_test)
        
        # Average forecasts over different lag rows
        tmp_fc %>% matrix(nrow = max_lag, ncol = .horizon) %>% t() %>% 
          rowMeans()
      }) %>%  
      bind_rows() %>%
      mutate(sd = sqrt(((`3q` - `1q`) / 1.349) ** 2))
    list("mean" = fc$mu, "sd" = fc$sd)
  }



# Metric-related functions -----------------------------------------------------
#' Crps function
#' Compute crps for all cv fc.
#' @param .obs True occ values
#' @param .fc Forecast probabilities
#' @param .penalty Bias to use (upper=right, none, lower=left of dist.)
crps_func <-  # Compute crps
  function(.obs, .fc, .penalty) {
    # .obs: observed value
    # .fc: forecast distribution
    # .penalty: 
    tmp_alpha = seq(0.01, 0.99, 0.01) # alpha level ([0, 1])
    tmp_weight =
      .penalty %>% 
      case_match(
        "upper" ~ expr("tmp_alpha ** 2"),
        "none" ~ expr("1"),
        "lower" ~ expr("(1 - tmp_alpha) ** 2")
      )
    
    map2(.fc, .obs, \(.dist, .obs_) {
      tmp_qf = # quantile forecast
        quantile(.dist, tmp_alpha)[[1]]
      case_when(
        .obs_ > tmp_qf ~ 
          -tmp_alpha * (tmp_qf - .obs_) * eval(parse(text = tmp_weight)),
        .obs_ <= tmp_qf ~ 
          (1 - tmp_alpha) * (tmp_qf - .obs_) * eval(parse(text = tmp_weight))
      ) %>% 
        sum() * 2 / (length(tmp_alpha))
    }) %>% 
      list_c()
    # tmp_domain = seq(0, 2000, 1) # ATTENTION: ad hoc domain common to BRI/Southmead
    # crps_p =
    #   map2(.fc, .obs, \(.dist, .obs_) {
    #   case_when(
    #     tmp_domain < .obs_ ~  cdf(.dist, tmp_domain)[[1]] ** 2,
    #     tmp_domain >= .obs_  ~ (cdf(.dist, tmp_domain)[[1]] - 1) ** 2
    #   ) %>% 
    #     sum() * 
    #        ((tail(tmp_domain, 1) - head(tmp_domain, 1)) /
    #           (length(tmp_domain) - 1))
    # }) %>% 
    #   list_c()
  }


#' Wilker function
#' Compute average wilker score over discretized alpha (predictive interval)
#' values for all cv fc.
#' @param .obs True occ values
#' @param .fc Forecast probabilities
#' @param .penalty Bias to use (upper=right, none, lower=left of dist.)
wilker_func <- # compute Wilker score - used in wilker_wrap()
  function(.obs, .fc, .penalty) {
    # .obs: observed value
    # .fc: forecast distribution
    # .penalty: penalise more observations above or below prediction interval
    ci_width = seq(0.05, 0.95, 0.05) # width of the confidence interval
    map(ci_width, \(.width) {
      upper = .fc %>% quantile(0.5 + .width / 2) # (upper interval)
      lower = .fc %>% quantile(0.5 - .width / 2) # (lower interval)
      ci_width = upper - lower # width confidence interval
      .penalty =
        case_when(
          .penalty == "upper" ~ 2,
          .penalty == "none" ~ 1,
          .penalty == "lower" ~ .5
        )
      case_when(
        .obs > upper ~ ci_width + (2 * .penalty) /.width * (.obs - upper),
        .obs < lower ~ ci_width + (2 / .penalty) /.width * (lower - .obs),
        .default = ci_width
      ) %>% 
        as_tibble_col(.width %>% as.character())
    }) %>% 
      list_cbind() %>% 
      rowMeans()
  }


#' Wrap metrics
#' Wrapper for crps_fun and wilker_fun to compute metrics for all cv fc.
#' @param .data Tibble containing true observations and fc distributions.
wrap_metric <- # general wrapper over metric function - used in cv_wrap()
  function(.data, ...) {
    # Organise obs and fc distributions in one tibble
    tmp_data =
      .data$all %>% 
      filter(type == "test") %>% 
      select(split, site, index, occ) %>% 
      left_join(
        .data$fc %>%
          as_tibble() %>%
          select(index, .model, occ) %>% 
          pivot_wider(names_from = .model, values_from = occ),
        by = "index"
      )
    
    # Compute metric for each model and penalty
    ls_models = tmp_data %>% names %>% tail(-4)
    penalty = c("upper", "none", "lower")
    map(penalty, \(.penalty) {
      map(ls_models, \(.model_name) {
        tmp_obs = tmp_data[["occ"]]
        tmp_fc = tmp_data[[.model_name]] # forecast distribution
        tibble(
          split = tmp_data$split,
          site = tmp_data$site,
          penalty = .penalty,
          index = tmp_data$index,
          wilker = wilker_func(tmp_obs, tmp_fc, .penalty),
          crps = crps_func(tmp_obs, tmp_fc, .penalty)
        ) %>% pivot_longer(
          cols = where(is.numeric), 
          names_to = "metric",
          values_to = .model_name)
      }) %>% 
        reduce(
          left_join, 
          by = c("split", "site", "penalty", "index", "metric")
        ) %>% 
        pivot_longer(
          cols = where(is.numeric),
          names_to = "models"
        )
    }) %>% 
      list_rbind()
  }


#' Process metrics 
#' Post-process output of wrap_metric function, which is a wrapper for crps_fun
#' and wilker_fun. Add t_ax, scale metrics by tslm metrics, convert penalty to
#' factor.
#' @param .metrics Tibble of metrics
process_metrics <- # data wrangling
  function(.metrics) {
    n_split <- # number of splits 
      .metrics$split %>% unique() %>% tail(1) %>% as.numeric()
    
    .metrics <- # add joint time axis
      .metrics %>%
      group_by(split) %>% 
      mutate(
        t_ax = as.numeric(index),
        t_ax = t_ax - (t_ax[1]),
        t_ax = t_ax + 7 * (n_split - as.numeric(split))
      ) %>% 
      ungroup()
    
    .metrics <- # scale by tslm model score
      .metrics %>% 
      group_by(site, penalty, metric, index) %>%
      mutate(
        value_s = value / value[models == "tslm"]
      ) %>% 
      ungroup()
    
    .metrics <-   
      .metrics %>% 
      mutate(
        penalty = factor(penalty) %>% fct_rev()
      )
  }


# Function to re-normalise weights in case of floating point issue; check
# whether weights sum to 1 when renormalising.
renorm <- function(.weight) {
  if (!(identical(sum(.weight), 1))) {
    cat("Weights sum to:", sum(.weight), "- normalizing...\n") 
    .weight / sum(.weight)} else {
      .weight
    }
}


#' Fc linear combination
#' Combine forecasts using weighted linear combination to generate a
#' distribution mixture. Combination is weighted, using either equal, crps or
#' wilker scores (.method).
#' @param .fc Original fc from different models
#' @param .weights Tibble of metrics summaries containing weights
#' @param .method Type of scoring methods used for generating weights
lcomb_fun <- 
  function(.fc, .weights, .list_m, .method) {
    .fc %>%
      group_by(split, site, h) %>%
      summarise(
        occ =
          tryCatch({
            do.call(
              dist_mixture,
              c(
                as.list(occ[.model %in% 
                              c(.list_m %>% pluck(as.character(site)[1]))]),
                list(weights = #new_weight
                       .weights %>%
                       filter(
                         .site == site %>% as.character() %>% .[1],
                         .penalty == "upper",
                         .models  %in%
                           c(.list_m %>% pluck(as.character(site)[1])),
                         .h == h[1],
                         .metric == .method
                       ) %>% pull(metric_avg) %>% 
                       renorm()
                )
              )
            )}, error = function(e) {
              message("model weights not summint to 1 when combining fc")
            })
      ) %>% 
      mutate(.model = .method) %>%
      ungroup()
  }


#' Fc combination wrapper
#' Wrapper function to generate weighted linearly combined fc. Take reciprocal
#' of crps/wilker as higher is worse
#' @param .fc Original fc to combine.
#' @param .metrics Original metrics to use for fc weighting.
fc_comb_wrap <- 
  function(.fc, .metrics) {
    # Data wrangling
    n_split <- # number of splits 
      .fc$split %>% unique() %>% tail(1) %>% as.numeric()
    
    
    .fc <- # add fc horizon (need redo t_ax!)
      .fc %>%
      group_by(split) %>% 
      mutate(
        t_ax = as.numeric(index),
        t_ax = t_ax - (t_ax[1]),
        t_ax = t_ax + 7 * (n_split - as.numeric(split)),
        h = t_ax - min(t_ax) + 1
      ) %>% 
      ungroup()
    
    
    # List models
    list_best_southmead <- 
      c("arima_dadpl_l", "arima_dad_rec", "rf_int", "var_ad2", "xgb", "tslm")
    list_best_bri <- 
      c("arima_dadp_l", "arima_dad_rec", "es", "rf_int", "var_ad", "var_h") # save2
      # c("arima_dad_l", "arima_dad_rec", "rf_int", "var_ad", "var_ad2",
        # "var_h", "xgb") # save1
    list_best_metric <-
      list("BRI" = list_best_bri, "Southmead" = list_best_southmead)
    
    # Compute avg scores for each model
    metric_avg <-
      .metrics %>% #pluck("metrics") %>%
      group_by(split) %>% 
      mutate(h = t_ax - min(t_ax) + 1) %>% # create horizon idx for grouping
      group_by(site, penalty, metric, models, h) %>% # not by split
      summarise(metric_avg = mean(value)) %>% # average metric by group
      ungroup() 
    
    metric_avg <- # add "." to names to avoid confusion with fc
      metric_avg %>% rename_with(~ paste0(".", .x), everything())
    
    metric_avg <- # add equal weights for linear pooling
      metric_avg %>% group_by(.site, .penalty, .models, .h) %>% 
      group_modify(~ add_row(.x, .metric = "equal", .metric_avg = 1)) %>% 
      ungroup()
    
    metric_avg <- # take reciprocal of crps and wilker
      metric_avg %>% 
      mutate(.metric_avg = 1 / .metric_avg)
    
    metric_avg <- # normalise weights
      metric_avg %>% 
      group_by(.site, .penalty, .h, .metric) %>% 
      mutate(
        metric_avg = if_else( # site-specific normalisation factor 
            .site == "BRI",
              .metric_avg / sum(.metric_avg[.models %in% list_best_bri]),
              .metric_avg / sum(.metric_avg[.models %in% list_best_southmead])
          )
      ) %>% 
      ungroup()
      
     
    fc_comb_lp <-
      lcomb_fun(.fc, metric_avg, list_best_metric, "equal")
    
    fc_comb_crps <-
      lcomb_fun(.fc, metric_avg, list_best_metric, "crps")
    
    fc_comb_wilker <-
      lcomb_fun(.fc, metric_avg, list_best_metric, "wilker")
    
    
    dimnames(fc_comb_lp$occ) <- "occ"
    dimnames(fc_comb_crps$occ) <- "occ"
    dimnames(fc_comb_wilker$occ) <- "occ"
    .fc <- 
      list(.fc, fc_comb_lp, fc_comb_crps, fc_comb_wilker) %>% 
      reduce(bind_rows) %>%  as_fable(".mean", "occ")
  }



# Functions for risk prediction ------------------------------------------------
#' Curve axes
#' Function to compute the axes of the ROC/PR curves from risk prediction
#' classifier. Curves represent x/y values for different decision boundaries
#' (db), that is, probability above which classifier sets threshold-crossing as
#' true, based on forecasted probability of crossing the allarm threshold. If
#' denominator is 0, set ROC/PR value for that db to 0.
#' @param .SD Subset of data, from data.table.
#' @param .type Whether to compute axis for ROC or PR curve.
ax_curve_fun <- 
  function(.SD, .type) {
    .db <- seq(0.995, 0, -0.005)
    .obs_cross = .SD[, obs_cross]
    
    if (.type == "roc") { # for ROC curve
      res <- lapply(.db, function(.x) {
        .x = sprintf("db_%.3f", .x)
        pos = .SD[[.x]]
        TP = sum(.obs_cross & pos)
        FP = sum(!.obs_cross & pos)
        FN = sum(.obs_cross & !pos)
        TN = sum(!.obs_cross & !pos)
        
        TPR = if ((TP + FN) > 0) round(TP / (TP + FN), 2) else 0
        FPR = if ((FP + TN) > 0) round(FP / (FP + TN), 2) else 0
        
        return(data.table(db = .x, TPR = TPR, FPR = FPR))
      })
    } else { # for PR curve
      res <- lapply(.db, function(.x) {
        .x = sprintf("db_%.3f", .x)
        pos = .SD[[.x]]
        TP = sum(.obs_cross & pos)
        FP = sum(!.obs_cross & pos)
        FN = sum(.obs_cross & !pos)
        TN = sum(!.obs_cross & !pos)
        
        PPV = if ((TP + FP) > 0) round(TP / (TP + FP), 2) else 0
        TPR = if ((TP + FN) > 0) round(TP / (TP + FN), 2) else 0
        
        return(data.table(db = .x, PPV = PPV, TPR = TPR))
      })
    }
    rbindlist(res) 
  }


#' Bootstrap curves
#' Bootstrap decision boundary (db) dataset, namely, a table containing for each
#' split, site and model a classification of threshold crossing as T/F based on
#' the forecasted risk of threshold crossing and a db (prob above which classify
#' threshold-crossing as T), ranging from 1-99%. Bootstrapping takes random
#' sample of splits, n times. Seed set in parent script.
#' @param .SD Subset of data, from data.table.
#' @param .type Whether to compute axis for ROC or PR curve.
boots_curves <- 
  function(.dt_tbl, .nboot = 50) {
    .splits = .dt_tbl[, unique(split)]
    .nsplits = length(.splits)
    .dt_tbl =
      map(seq(1, .nboot), \(.x) {
        .boots = sample(.splits, .nsplits, replace = T)
        rbindlist(lapply(.boots, \(.s) {.dt_tbl[split == .s]}))[, nboot := .x]
      }) %>% 
      rbindlist()
  }


#' AUC function
#' Compute area under the curve (for both ROC and RP) using rectangle quadrature
#' rule (sum of rectangle with hight as mean of y between consecutive points)
#' @param .SD Subset of table, from data.table
auc_fun <- 
  function(.SD){
    .Nr = nrow(.SD)
    .Nc = ncol(.SD) # 2nd to last is y, last is x
    dx <- diff(.SD[[.Nc]])
    y <- .SD[[.Nc - 1]]
    height <- (y[2:.Nr] + y[1:.Nr - 1]) / 2
    list(auc = sum(dx * height))
  } 



# Functions for temperature variable -------------------------------------------
#' Discretise temperature
#' Make temperature variable a factor, inspired by Rizmie et al., 2022 (Impact
#' of extreme temperatures on emergency hospital admissions by age and socio-
#' economic status in England): used both tmax and tmin to better capture 
#' extremes in daily temperature (picks and troughs), instead of tmean; take as
#' reference level days with 7<tmax & tmin <22, then compute 5-degree bins, and
#' use t<2 and t>27 as min and max levels; used these levels after inspecting
#' quartile distribution of t available. Function called within a for loop
#' looping across lags (0-7).
#' @param .tbl Tibble with lagged tmax and tmin
#' @param .lag Specific lag of data to factorise
factorise_temp <- 
  function(.tbl, .lag) {
    # Var names
    .lname = if (.lag == 0) "" else paste0("_lag", .lag)
    .newvar = paste0("tvar", .lname)
    .tmin = paste0("tmin", .lname)
    .tmax = paste0("tmax", .lname)
    # Transform
    .tbl %>%
      mutate(
        !!.newvar := 
          case_when(
            !!sym(.tmin) > 7 & !!sym(.tmax) < 22 ~ "7-22", # reference
            !!sym(.tmin) > 2 & !!sym(.tmin) <= 7 ~ "2-7",
            !!sym(.tmin) <= 2 ~ "less-2",
            !!sym(.tmax) >= 22 & !!sym(.tmax) < 27 ~ "20-25",
            !!sym(.tmax) >= 27 ~ "more-27",
            .default = "largetdiff"
          ) %>% factor() %>% relevel(ref = "7-22"),
        !!sym(.tmin) := NULL,
        !!sym(.tmax) := NULL
      )
  }



# Functions for trend prediction -----------------------------------------------
#' Smooth var
#' Use cubic spline to smooth training set for each split. Parameters nknows and
#' spar selected to give balance between data point fidelty and smooth function.
#' @param .data_ Data for each site and cv split.
smooth_fun <- 
  function(.y) {
    a0 = c(.y[1], 0) # initial value estimate
    P0 = 
      matrix(c(100, 0, 0, 100), ncol = 2) # initial uncertainty estimate
    .dt = matrix(c(0, 0)) # intercept of transition equation
    ct = matrix(0) # intercept of measurement equation
    Tt = matrix(c(1, 1, 0, 1), ncol = 2) # factor of transition equation
    Zt = matrix(c(1, 0), ncol =2) # factor of measurement equation
    HHt = 
      matrix(c(.1, 0, 0, 200), ncol = 2) # variance of transition innovations
    GGt = matrix(100) # variance of measurement error
    yt = matrix(.y, nrow = 1) # observed outcome
    fit = 
      fkf(
        a0 = a0, P0 = P0, dt = .dt, ct = ct, Tt = Tt, 
        Zt = Zt, HHt = HHt, GGt = GGt, yt = yt
        )
    fit$att[1, ]
    # ny = length(.y)
    # x = seq(1, ny)
    # .y
    # tmp_smooth = 
    #   smooth.spline(x, .y, nknots = ceiling(sqrt(ny)), spar = 0.2)
    # tmp_smooth$y
  }


#' Fit trend
#' Fit smoothed bed occupancy with structural state space model. Saving fc both
#' with and without mean.
#' @param .data_ Data including smoothed occ
fit_trend <- 
  function(.data_, .horizon = NULL, ...) {
    # Training data
    train = .data_ %>% filter(type == "train") %>% pull(occ_s)
    train_mean = mean(train)
    
    # Fit/fc STS model
    ss = AddLocalLinearTrend(list(), train)
    ss = AddSeasonal(ss, train, nseasons = 7)
    model = bsts(train, ss, niter = 500)
    fc = predict(model, horizon = .horizon, burn = 100)
    
    # Organise data
    fc =
      .data_ %>% filter(type == "test") %>%
      mutate( # create forecast distribution (assuming nid error)
        .model = "sts_trend",
        occ = # trend fc without mean
          dist_normal(fc$mean - train_mean, sd = apply(fc$distribution, 2, sd)),
        occ_wm = # trend fc with mean
          dist_normal(fc$mean, sd = apply(fc$distribution, 2, sd)),
        .mean = fc$mean, # necessary for fable::autoplot
      )
  }


