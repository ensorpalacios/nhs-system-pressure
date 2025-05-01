#' Split data
#'
#' Split data for fitting and testing; split data in a training and test set; 
#' split training set into k testing and validation sets with resampling using
#' a roling window.
#' 
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-04-23
#' 
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
#' @param .data Data organised by site and splits
#' @param .select Function to select data to pass to .function
#' @param .function Function to apply to subset of data
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
#' @export
cv_wrap <-
  function(.data, .select, .function, ...) { 
    if (is_tibble(.data)) {
      sites = .data$site %>% unique()
      splits = .data$split %>% unique()
    } else if (is.list(.data)) {
      sites = flatten(.data) %>% .$site %>% unique()
      splits = flatten(.data) %>% .$split %>% unique()
    } else stop(".data not a tibble or list")
    
    map(sites, \(.site) {
      map(splits, \(.split) {
        .select(.data, .site, .split, ...) %>% 
          .function()
      }) %>% set_names(splits)
    }) %>% set_names(sites)
  }
