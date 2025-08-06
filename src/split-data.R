#' Helper functions
#'
#' Include all helper functions (functions used in multiple scripts or too 
#' lengthy); includes functions for:
#' - Split/augment (lag) data for fitting and testing.
#' - Plot functions
#' - Utility functions
#' 
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-04-23

# Functions to split/augment/bootstrap data for analysis -----------------------
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
  function(.ts_occ, .len_test = "5 months") {
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

#' extract site-, split- and model-specific forecast set from cross-validated
#' train-validation sets (split_data_cv)
#' @param .data_sel Data 
#' @param .site List of hospitals
#' @param .split List of cv splits
#' @param .vars List of y/x variables in the model
#' @param .type Character indicating whether to include all or only train data
#' @export
select_training <- # select training set (by site and split)
  function(.data_sel, .site, .split, .vars = NULL, .type = "train") {
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


#' extract site-, split- and model-specific forecast set from cross-validated
#' train-validation sets (split_data_cv)
#' @param .data Data 
#' @param .site List of hospitals
#' @param .split List of cv splits
#' @param .models List of models from which to generate forecasts
#' @export
select_fc <- # select forecast (by site and split) - used in cv_wrap()
  function(.data, .site, .split, .models) {
    # .data is list (site) of list (splits) of list with
    # $all full data and $fc forecast;
    # .model is a list of model whose forecasts are plotted
    list(
      "all" = 
        .data$all %>% 
        filter(site == .site, split == .split) %>% 
        as_tsibble(index = index),
      "test" =
        .data$fc %>% 
        filter(site == .site, .model %in% .models, split == .split)
    )
  }


#' extract site-, and split-specific model fit/fc parameters (es, rf, xgb)
#' @param .data fits/fc parameters
#' @param .site from cv_wrap
#' @param .split from cv_wrap
select_model <- # select model fit (by site and split)
  function(.data, .site, .split, ...)  {
    # .data is list (site) of list (split) of model fits
    list(
      "model" = .data[[.site]][[.split]],
      "index" = data_xpredict %>% filter(site == .site, split == .split)
    )
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
          .function()
      }) %>% set_names(splits)
    }) %>% set_names(sites)
  }


#' Block Bootstrap
#' Function to created block bootstrap data data
#' fit-models-short.R)
#' @param tmp_data Training data
#' @param .sites List of sites
#' @param n_boot Number of bootstraps
#' @param block_size Length of block
block_boot <- 
  function(.tbl_data, .n_boot = 20, .b_size = 14) {
    tmp_sites = .tbl_data %>% pull(site) %>% as.character() %>% unique()
    
    map(tmp_sites, \(.site) {
      tmp_data = .tbl_data %>% filter(site == .site)
      len_data = dim(tmp_data)[1]
      samp_last = len_data - .b_size
      samp_n =  len_data / .b_size
      
      map(as.list(seq(1, .n_boot)), ~ {
        tmp_start = sample(seq(1, samp_last), samp_n)
        
        # Block bootstrap
        tmp_boot =
          map(tmp_start, \(.start) {
            tmp_data %>% 
              as_tibble() %>% 
              select(-index, -site, -t_ax) %>% 
              slice(.start:(.start + .b_size - 1))
          }) %>% 
          list_rbind() %>% 
          mutate(
            index = tmp_data$index, 
            site = tmp_data$site, 
            t_ax = tmp_data$t_ax
          ) %>% 
          as_tsibble(index = index, key = site) %>% 
          relocate(index, site, occ)
      })
    }) %>% 
      set_names(tmp_sites) %>% 
      list_transpose() %>% 
      map(\(.tmp_data) {
        .tmp_data %>% list_rbind()
      })
  }



#' #' Bootstrap & lag data
#' #' Function to created block bootstrapped training data. To do so, get rid of
#' #' lagged data, bootstrap training set, and lag new data. Sample with
#' #' replacement
#' #' Attention: horizon is global variables defined outside the function (in
#' #' fit-models-short.R)
#' #' @param tmp_data Training data
#' #' @param n_boot Number of bootstraps
#' #' @param block_size Length of block
#' #' @param boot Whether to bootstrap or only lag data
#' boot_lag <- 
#'   function(tmp_data, n_boot = 20, block_size = 14, boot = TRUE) {
#'     tmp_data %>% select(lag)
#'     tmp_data = tmp_data %>% filter(lag == "lag1")
#'     save_index = tmp_data$index
#'     len_data = max(save_index)
#'     samp_last = len_data - block_size # last possible index for .start
#'     samp_n =  len_data / block_size # number of bootstrapped samples
#'     nlag = horizon + 1
#'     
#'     map(as.list(seq(1, n_boot)), ~ {
#'       tmp_start = sample(seq(1, samp_last), samp_n, replace = TRUE)
#'       
#'       # Block bootstrap
#'       tmp_boot =
#'         map(tmp_start, \(.start) {
#'           tmp_data %>% 
#'             select(-index) %>% 
#'             slice(.start:(.start + block_size - 1))
#'         }) %>% 
#'         list_rbind()# %>% 
#'         # mutate(index = save_index)
#'       
#'       # Add lags
#'       tmp_lag = # matrix lagged data
#'         tmp_boot %>% select(-contains("days")) %>% 
#'         as.matrix()
#'       
#'       tmp_lag_names = # name lagged variables
#'         tmp_lag %>% dimnames %>% .[[2]]
#'       tmp_lag_names = 
#'         map(seq(nlag), \(.nlag) {
#'           if (.nlag == 1) { 
#'             tmp_lag_names
#'           } else {
#'             paste0(tmp_lag_names, "_lag", .nlag - 1)
#'           }
#'         }) %>% 
#'         unlist()
#'       
#'       tmp_lag = # expand time
#'         embed(tmp_lag, nlag)
#'       
#'       colnames(tmp_lag) = # rename cols
#'         tmp_lag_names
#'       
#'       tmp_all = # join lagged & non-lagged data
#'         tibble(
#'           tmp_boot %>% 
#'             slice(nlag:n()) %>% 
#'             select(contains("days")),
#'           tmp_lag %>% as_tibble(),
#'         ) %>% 
#'         relocate(occ)
#'       
#'       # Wide to long format
#'       tmp_all <- # long format data with lag column
#'         tmp_all %>%
#'         select(-occ_other, -ad_diff_f, -ad_diff2_f, -ad_diff3_f) %>%
#'         rename_with(~ sub("occ_lag", "occ_same_lag", .x)) %>%
#'         rename_with(~ sub("_lag", "-lag", .x, fixed = TRUE)) %>% # fixed for _
#'         pivot_longer(
#'           cols = c(contains("lag")),
#'           names_to = c(".value", "lag"),
#'           names_sep = "-"
#'         )
#'     })
#'   }


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
#' @param .detrend whether to detrend .var (LOESS) - only if .stationary = FALSE
#' @export
stabilise <- 
  function(
    .var, .index, 
    .xdays = FALSE, .wdays = FALSE, .stationary = FALSE, .detrend = FALSE
  ) {
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
    
    # Detrend
    if (.detrend) {
      tmp_loess = 
        tmp_data %>% 
        mutate(index = index %>% as.numeric()
        ) %>% # fit mean-subtracted .var
        loess(var ~ index, data = ., span = 0.3) %>% 
        predict()
      tmp_loess = tmp_loess - mean(tmp_loess) # mean-subtracted loess fit
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
#' Predict regressors (occ, occ_other, all ad_diff) using mean, naive/locf,
#' snaive, arima, ets model; replace test values for different 
#' lags appropriately (lag 0 replace #' all -- lag7 replace none).
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
            if (!any(is_aggregated(site)) & !grepl("occ_other", cur_column())) {
            # Predict with ARIMA
            x_ts =
              tibble(index, split, type, site, .x, days_)
            
            x_predict = 
              x_ts %>%
              filter(type == "train") %>%
              as_tsibble(index = index, key = c(split, site))
            
            if (.type == "mean") {
              x_predict =  x_predict %>% model(xmodel= MEAN(.x))
            } else if (.type == "naive") {
              x_predict =  x_predict %>% model(xmodel= NAIVE(.x))
            } else if (.type == "snaive") {
              x_predict =  x_predict %>% 
                model(xmodel= SNAIVE(.x ~ lag("week")))
            } else if (.type == "arima") {
              x_predict =  x_predict %>% model(xmodel= ARIMA(.x ~ days_))
            } else if (.type == "ets") {
              x_predict =  x_predict %>% model(xmodel= ETS(.x))
            } else if (.type == "pull") {
              x_predict =  x_predict %>% 
                model(
                  mean = MEAN(.x),
                  ets = ETS(.x), 
                  arima = ARIMA(.x ~ days_)
                  ) %>% 
                mutate(xmodel = (mean + ets + arima) / 3)
            }
            x_predict = 
              x_predict %>%
              forecast(
                new_data =
                  x_ts %>%
                  filter(type == "test") %>%
                  as_tsibble(index = index, key = c(split, site))
              ) %>% 
              filter(.model == "xmodel")
            
            
            # Replace true with predicted
            .x[x_ts$type == "test"] = round(x_predict$.mean, 2)
            .x
            } else {
              rep(NA, n())
            }
          },
          .names = "{.col}_predicted"
        ),
        across(
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
    # }
  }


#' Random forest fit and predict
#' Random forest for forecast. Train RF on data in wide format: lagged data have
#' their own column. Additionally predict bed occupancy for the next 7 days;
#' easier to train and predict in one for loop. Finally, extract forecast
#' parameters (i.e., expected forecast values) and compute confidence interval
#' using average root mean squared error from oob data. Use predictors of
#' increasing lag to predict bed occupancy with increasing time horizon h.
#' @param .data_rf tibble of data
#' @param .horizon forecast horizon
rf_reg <- 
  function(.data_rf, .horizon = horizon) {
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
#' @param .horizon forecast horizon
rf_reg_int <- 
  function(.data_rf, .horizon = horizon) {
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
    tmp_fc_individuals$lag = rep(seq(horizon), each = max_lag)
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
#' @param .horizon forecast horizon
xgb_reg_int <- 
  function(.data_rf, .horizon = horizon) {
    # Initialise
    seq_boots <- # list of bootstraps
      .data_rf %>% pull(boot) %>% unique()
    
    qreg = # quantile loss function (grad and hess)
      function(.alpha) {
        function(preds, dtrain) {
          tmp_labels = getinfo(dtrain, "label")
          pe = tmp_labels - preds
          grad = if_else(pe < 0, 1 - .alpha, -.alpha)
          hess = rep(1, length(grad))
          list(grad = grad, hess = hess)
        }
      }
    
    
    # Fit/forecast
    tmp_fc <- 
      map(seq_boots, \(.boot) { # for each bootstrap
        # Train set
        data_train =
          .data_rf %>%
          filter(type == "train", boot == .boot) %>% select(-type, -boot)# %>%
        data_train =
          xgb.DMatrix(
            data = sparse.model.matrix(occ ~ . - 1, data = data_train),
            label = data_train %>% pull(occ)
          )
        
        # Test set
        # Attention: always predict boot == 0 (original ts)!
        data_test = 
          .data_rf %>% 
          filter(type == "test", boot == 0) %>% select(-type, -boot)
        max_lag = # save for later
          data_test$lag %>% parse_number() %>% max()
        data_test =
          xgb.DMatrix(
            data = sparse.model.matrix(occ ~ . - 1, data = data_test),
            label = data_test %>% pull(occ)
          )
        
        # Fit and forecast
        # for -1/+1 sd (assuming normal distribution)
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
          
          # Average forecasts over lags
          tmp_fc %>% matrix(nrow = max_lag, ncol = horizon) %>% t() %>% 
            rowMeans()# %>% as.data.frame() %>% setNames(.idx)
        }) %>% 
          bind_rows() %>% 
        mutate(
          h = seq(1, n()),
          boot = .boot
        )
    }) %>%  
      list_rbind() %>% 
      group_by(h) %>% 
      mutate(var= ((`3q` - `1q`) / 1.349) ** 2) %>% # sample variance
      summarise( # mean and sd of mixture of bootstrap fc
        mean = mean(mu),
        sd = # from var = weighted average of var + variance of means
          sqrt(mean(var) + mean(mu - mean) ** 2) 
        )
    
    list("mean" = tmp_fc$mean, "sd" = tmp_fc$sd)
  }