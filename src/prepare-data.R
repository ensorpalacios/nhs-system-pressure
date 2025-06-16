#' Preprocess data
#'
#' Load, clean and recode data.
#' Attention: occ is total number of beds used, whereas escal and 
#' core are beds open, not used; however, assume that core beds are fully 
#' used before escalation beds, meaning that core beds open = used, and we can
#' recover escalation beds from total - core.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-07


# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")


# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/raw/")

# Admissions/discharges, acute bed occupancy, escalation beds - 2024
# Urgent care data
df_occ <- read_excel(
                  paste0(
                         data_path,
                         "2022-01-01-to-2025-01-31-acute-occupancy.xlsx"
                         ),
                  sheet = 2
)


# Fully balanced panel ---------------------------------------------------------
# Keep BRI & Southmead, generate index, rename variables
df_occ <- 
  df_occ |>
    filter(
      provider == "BRI" | provider == "NBT") |>
    select(-metric_id) |> 
    pivot_wider(names_from = metric_name, values_from = value) |>
    mutate(
      index = as.Date(report_date, tz = "GMT"),
      site = case_match(
        provider,
        "BRI" ~ "BRI", 
        "NBT" ~ "Southmead"),
      adm = `Number of Admissions`,
      dis = `Number of Discharges`,
      escal = `Escalation beds open`,
      core = `Core stock open`,
      occ = `Bed occupancy`,
      provider = NULL,
      `Number of Admissions` = NULL,
      `Number of Discharges` = NULL,
      `Escalation beds open` = NULL,
      `Bed occupancy` = NULL,
      `Core stock open` = NULL,
      report_date = NULL
    ) |>
    relocate(c(index, site)) |>
    arrange(index, site)

# Covert to timeseries (tsibble object)
ts_occ <- df_occ |> as_tsibble(index = index, key = site)

# Remove first 3/4 of 2022 data (due to strange behaviour)
ts_occ <- 
  ts_occ %>% filter(index >= as.Date("2022-09-01"))

# Convert implicit gaps into explicit missing values
ts_occ <- 
  ts_occ |>
    fill_gaps(.full = TRUE) # fully balanced data

# Impute missing values
ts_occ <- # impute
  ts_occ %>% 
  group_by(site) %>% 
  mutate(
    # Save data with missing values
    occ_m = occ,
    dis_m = dis,
    adm_m = adm,
    escal_m = escal,
    core_m = core,
    # Impute (simple moving average, window=7)
    occ_i = occ %>% na_ma(k = 3, weighting = "simple"),
    core = core %>% na_ma(k = 3, weighting = "simple"),
    dis = dis %>% na_ma(k = 3, weighting = "simple"),
    adm = adm %>% na_ma(k = 3, weighting = "simple"),
  ) %>% 
  ungroup()


# Process data -----------------------------------------------------------------
ts_occ <- 
  ts_occ %>% 
  group_by(site) %>% 
  mutate(
    # Process bed escalation
    escal = # assume core = core bed actually used
      (occ - core) %>% if_else(. < 0, 0, .), 
    # escal_c = escal %>% slide(aa, .before = 3),

    # New bed occupation (scaled by core)
    occ = occ_i / core, # scale occ by core
    occ = # subtract holidays effect + detrend
      stabilise(occ, index, .xdays = TRUE, .detrend = TRUE),

    # New admission-discharge variables
    ad_diff = adm - dis, # difference
    ad_diff = # subtract holidays + week days effect
      stabilise(ad_diff, index, .xdays = TRUE, .wdays = TRUE),
    ad_diff2 = c(0, diff(ad_diff)), # rate of change of ad_diff
    ad_diff3 = c(0, 0, diff(ad_diff, differences = 2)), # rate of rate of change
    
    # Filter
    ad_diff_f = slide_dbl(ad_diff, mean, .before = 2),
    ad_diff2_f = slide_dbl(ad_diff2, mean, .before = 2),
    ad_diff3_f = slide_dbl(ad_diff3, mean, .before = 2),
    
    # Z-score
    core = (1 - mean(occ)) / sd(occ), # as 100% ref for new occ
    occ = zs_fun(occ),
    ad_diff = zs_fun(ad_diff),
    ad_diff2 = zs_fun(ad_diff2),
    ad_diff_f = zs_fun(ad_diff_f),
    ad_diff2_f = zs_fun(ad_diff2_f),
    ad_diff3_f = zs_fun(ad_diff3_f),
  ) %>% 
  ungroup()

ts_occ$occ_other <- # add bed occupancy other hospital
  ts_occ %>% arrange(rev(site)) %>% pull(occ)


# Add days of week and time index
ts_occ <- 
  ts_occ |>  
    mutate(
      days_ = format(index, "%a") |> 
        factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
      t_ax = rep(1:(length(days_)/2), 2)
    )


# Save ts ---------------------------------------------------------------------
save_path <- here("data/processed/")
if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

saveRDS(ts_occ, file = paste0(save_path, 'tbl_occ.RDS'))
