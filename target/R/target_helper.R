#' Helper functions
#'
#' Include all helper functions
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-12-14

# Functions to prepare data ---------------------------------------------------
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
        select(-c(index, type, site, days_, t_ax)) %>% 
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
          select(index, type, site, t_ax, days_),
        tmp_lagged %>% as_tibble(.name_repair = "check_unique"),
        tmp_days %>% as_tibble() %>%  slice(.lag:n())
      )
    }) %>% 
      list_rbind()
  }


#' Stabilise time series
#' Correct artifacts in data, by removing Christmus and/or week days effects.
#' For Christmus effects, can either do nothing (xdays = FALSE), remove it by
#' using gaussian mask by default (xdays = TRUE) or by using 1st derivative of
#' gaussian as mask when applied to admission-discharge difference (ad-diff).
#' @param .var variable to detrend (time series/vector)
#' @param .index time index in y-m-d (time series/vector)
#' @param .xdays remove Christmus effect; either FALSE, TRUE, or "ad-diff"
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
    if (!isFALSE(.xdays)) {
      christmus_period = 
        .index %>% base::format("%Y") %>% unique() %>%  # years in data
        map(\(.year) {
          seq(
            as.Date(str_glue("{.year}-12-24")), 
            as.Date(str_glue("{.year}-12-26")),
            by = "days")
        }) %>% 
        purrr::reduce(c)
      
      tmp_data = # default gaussian kernel
        tmp_data %>% 
        mutate(
          christmus = 
            case_when(index %in% christmus_period ~ 1, .default = 0) %>%
            ksmooth(index, ., kernel = "normal", bandwidth = 5) %>% .$y
        )
      
      if (.xdays == "ad-diff") { # kernel 1st derivative for ad difference
        tmp_data =
          tmp_data %>% 
          mutate(christmus = diff(christmus) %>% c(., 0))
        cat("use gaussian kernel derivative")
      }
      
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



#' Predict regressor function
#' Predict regressors using tslm, snaive, arima, ets model; 
#' replace test values for different lags appropriately (lag 0 replace 
#' all -- lag7 replace none): use predicted if t = T + h, else use observed, 
#' where t is actual time, T is last time of test data (.idx_test - 1) and 
#' h is forecast horizon.
#' @param .data tibble with cv splits and sites as groups
#' @param .var list of variables to predict
#' @param .type choose model for x-predictions
xpredict_fun <- 
  function(.data, .var, .type) {
    new_index = seq(max(.data$index) + 1 , max(.data$index) + horizon)
    .data = # add rows for future data with index and type
      .data %>%
      group_by(site) %>% 
      mutate(type = "train") %>% 
      group_modify(
        ~ add_row(.x, index = new_index, type = "test")
      ) %>% 
      relocate(type, .before = index) %>% 
      ungroup()
    
    .data =
      .data %>% 
      group_by(site) %>% 
      mutate(
        across(
          c(contains(.var)),
          ~ {
            x_ts =
              tibble(index, site, type, y = .x, days_) %>% 
              as_tsibble(index = index, key = site)
    
            x_predict = # fit
              x_ts %>% filter(type == "train") %>% 
              model(
                tslm = TSLM(y ~ trend() + season("1 week")),
                ets = ETS(y), 
                arima = ARIMA(y ~ season("1 week"), greedy = FALSE)
              ) %>% 
              mutate(xmodel = (tslm + ets + arima) / 3)
            
            x_predict = # forecast
              x_predict %>%
              forecast(h = horizon) %>% # using global parameter! 
              filter(.model == "xmodel")
            
            # Add predictions
            .x[x_ts$type == "test"] = round(x_predict$.mean, 2)
            .x
          }
        )
      ) %>% 
      mutate(
        days_ = 
          replace(
            days_, 
            type == "test", 
            weekdays(index[type == "test"]) %>% str_sub(1, 3)
        ),
        t_ax = 
          replace(
            t_ax, 
            type == "test", 
            seq(max(t_ax, na.rm = T) + 1, max(t_ax, na.rm = T) + horizon)
        )
      ) %>% 
      ungroup()
    
    # Add temperature forecasts
    temp_fc <- 
      get_temp(.historic = FALSE, .today = new_index[1] - 1)
    

    # Replace missing temp with forecasts
    .data %>% 
      left_join(
        temp_fc %>%
        rename_with(.fn = \(x) str_c(x, "_api")),
         by = join_by(index == report_date_api)) %>%
      mutate(tmax = coalesce(tmax, tmax_api)) %>%
      mutate(tmin = coalesce(tmin, tmin_api)) %>%
      select(!matches("_api"))
  }


# Functions to compute/harmonise fc -------------------------------------------
#' Harmonise fc par
#' Harmonise fc distribution parameters for random forest and xgb models to 
#' fable data structure.
#' structure.
#' @param .fc Includes rf fc parameters.
#' @param .data Includes observed occ
#' @param .nmodel Includes name of model for saving fc.
harmonise_fc_par =
  function(.fc, .data, .nmodel) {
    # Function to harmonise fc to fable data structure
    .data %>% 
      filter(type == "test", !is_aggregated(site)) %>% 
      group_by(site) %>% 
      select(site, index) %>% 
      mutate(
        site = as.character(site),
        .model = .nmodel,
        .mean = pluck(.fc, site[[1]], "mean"), # necessary for fable::autoplot
        occ = dist_normal(
          mu = pluck(.fc, site[[1]], "mean"), 
          sd = pluck(.fc, site[[1]], "sd"))
      ) %>%
      relocate(c(.model, occ, .mean), .after = index) %>% 
      ungroup()
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
  function(.data_rf, .horizon) {
    sites <- unique(as.character(.data_rf$site))
    map(sites, \(.site) {
      # Train set
      data_train = 
        .data_rf %>% filter(type == "train", site == .site) %>% 
        select(-c(type, site, index))
      
      # Test set
      data_test = 
        .data_rf %>% filter(type == "test", site == .site) %>% 
        select(-c(type, site, index))
      
      # Compute
      tmp_fit = randomForest(occ ~ ., data = data_train, ntree = 1000)
      tmp_fc = predict(tmp_fit,  data_test, predict.all = TRUE)
      
      # Out-of-bag prediction error
      tmp_fit_pe = (tmp_fit$predicted - data_train$occ) %>% as_tibble() # oob pe
      
      
      # Group by lag
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
          "sd" = sd(tmp_fit_pe$value) # from oob errors
        )
      
      tmp_par
    }) %>% set_names(sites)
  }



#' XGBoost - interaction
#' @param .data_rf tibble of data
#' @param .temp/tmp1 unused (used in select_training)
#' @param .horizon forecast horizon
xgb_reg_int <- 
  function(.data_xgb, .horizon) {
    sites <- unique(as.character(.data_xgb$site))
    map(sites, \(.site) {
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
        .data_xgb %>%
        filter(type == "train", site == .site) %>% select(-c(type, site, index))
      data_train =
        xgb.DMatrix(
          data = sparse.model.matrix(occ ~ . - 1, data = data_train),
          label = data_train %>% pull(occ)
        )
      
      # Test set
      data_test = 
        .data_xgb %>% 
        filter(type == "test", site == .site) %>% select(-c(type, site, index))
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
    }) %>% set_names(sites)
  }



# # Plots ------------------------------------------------------------------------
# #' Plot forecasts
# #' Plot fc for each site and split
# #' @param .data Data containing observed ("all") and fc occ ("fc").
# plot_forecast <- # plot forecast function
#   function(.data, ...) {
#     # Reduce length observations (x axis)
#     .data$all = 
#       .data$all %>% 
#       group_by(split, type, site) %>% 
#       mutate(
#         start_index = 
#           case_when(
#             type == "train" ~
#               head(index, 1) + 
#               (tail(index, 1) - head(index, 1)) / 1.7,
#             type == "test" ~
#               head(index, 1)
#           )
#       ) %>% 
#       ungroup() %>% 
#       filter(index >= start_index)
    
#     # Plot
#     .data$fc %>%
#       autoplot() +
#       autolayer(.data$all, .vars = occ) +
#       scale_colour_manual(name = "models", values = col_models) + 
#       scale_fill_manual(name = "models", values = col_models) + 
#       scale_y_continuous(breaks = c(600, 700)) +
#       facet_wrap(vars(.model), ncol = 1, strip.position = "right")
#   }


# Metric-related functions -----------------------------------------------------
#' Function to re-normalise weights in case of floating point issue; check
#' whether weights sum to 1 when renormalising.
renorm <- function(.weight) {
  if (!(identical(sum(.weight), 1))) {
    cat("Weights sum to: ~", sum(.weight), "- normalizing...\n") 
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
  function(.fc, .weights, .list_m, .penalty_, .method) {
    n_models <- lapply(.list_m, length)
    .fc %>%
      group_by(site, h) %>%
      summarise(
        occ = tryCatch(
          {
            do.call(
              dist_mixture,
              c(
                as.list(occ[
                  .model %in%
                    c(.list_m %>% pluck(site[1]))
                ]),
                list(
                  weights = rep(
                    1 / pluck(n_models, site[1]),
                    pluck(n_models, site[1])
                  )
                )
                # list(
                #   #new_weight
                #   weights = .weights %>%
                #     filter(
                #       .site == site[1],
                #       .penalty == .penalty_,
                #       .models %in%
                #         c(.list_m %>% pluck(site[1])),
                #       .h == h[1],
                #       .metric == .method
                #     ) %>%
                #     pull(.weight) %>%
                #     renorm()
                # )
              )
            )
          },
          error = function(e) {
            message("model weights not summing to 1 when combining fc")
          }
        ),
        .mean = mean(occ)
      ) %>%
      mutate(.model = .method) %>%
      ungroup()
  }


# Functions for temperature variable -------------------------------------------
#' Get temperature data
#' Get both historical and forecast min/max temperatures from open-meteo api;
#' temperatuyre is 2 m from ground; for historical data, align to ts_occ.
get_temp <- 
  function(.historic, .today, .start = NULL) {

    if (isTRUE(.historic)) {
    res_h <- # historical data
      GET(
        str_glue("https://archive-api.open-meteo.com/v1/archive?latitude=51.458886&longitude=-2.596293&start_date={.start}&end_date={.today}&daily=temperature_2m_max,temperature_2m_min&timezone=GMT")
      )
    } else {
      res_h <- # forecast data
      GET(
        str_glue("https://api.open-meteo.com/v1/forecast?latitude=51.458886&longitude=-2.596293&daily=temperature_2m_max,temperature_2m_min&start_date={.today+1}&end_date={.today+7}")
      )
    }
    
    res_h <- res_h$content %>% rawToChar() %>% jsonlite::fromJSON()

    tbl_h <- 
      data.table(
        report_date = res_h$daily$time, 
        tmax = res_h$daily$temperature_2m_max, 
        tmin = res_h$daily$temperature_2m_min
      )[
        , report_date := lubridate::ymd(report_date)
      ]    

    # Return tbl
    tbl_h
  }



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
