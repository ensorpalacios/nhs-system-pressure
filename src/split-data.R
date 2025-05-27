#' Split data
#'
#' Split data for fitting and testing; split data in a training and test set; 
#' split training set into k testing and validation sets with resampling using
#' a roling window.
#' 
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-04-23

#' create lagged data
#' @param .data The data as a tsibble
#' @param .lag Number of lags
#' @export
lag_fun <- 
  function(.data, .lag = 12) {
    .lag = .lag + 1 # include no lag
    
    map(sites, \(.site) {
      tmp_data = # site specific data
        .data %>% filter(site == .site)
      
      # Days (one hot encoding)
      tmp_days =
        tmp_data %>% 
        model.matrix(~ 0 + days_, data = .) # 0 for no intercept/ref
      
      # Lagged variables
      tmp_lagged = # design matrix
        tmp_data %>% as_tibble() %>% 
        select(-c(index, site, days_, t_ax)) %>% 
        relocate(bed_occ, bed_occ_z) %>% names() %>% 
        paste(collapse = "+") %>% paste("~0+", .) %>% formula() %>% 
        model.matrix(data = tmp_data)
      
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
        tmp_data %>% slice(.lag:n()) %>% select(index, site, t_ax, days_),
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
