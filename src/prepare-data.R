#' Preprocess data
#'
#' Load, clean and recode data.
#' Attention: occ is total number of beds used, whereas escal and 
#' core are beds open, not used; however, assume that core beds are fully 
#' used before escalation beds, meaning that core beds open = used, and we can
#' recover escalation beds from total - core.
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-01-07


# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")


# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/raw/")

# Admissions/discharges, acute bed occupancy, escalation beds - 2024
# Urgent care data
df_occ <- 
  read_excel(
    paste0(data_path, "2022-01-01-to-2025-01-31-acute-occupancy.xlsx"),
    sheet = 2
  )

# A&E pediatric & length of stay variables
df_pl <- 
  read_excel(
    paste0(data_path, "2022-01-01-to-2025-01-31-los-pead-attends.xlsx"),
    sheet = 2
  )


# Temperature data
tmp_max <- 
  read.table(
  paste0(data_path, "maxtemp_daily_totals.txt")
) %>% data.table() %>% 
  .[, .(report_date = V1[-1], tmax = as.numeric(V2[-1]))]

tmp_min <- 
  read.table(
  paste0(data_path, "mintemp_daily_totals.txt")
) %>% data.table() %>% 
  .[, .(report_date = V1[-1], tmin = as.numeric(V2[-1]))]

df_t <- # join tmax/tmin and melt
  tmp_min[tmp_max, on = "report_date"] %>%
  melt(
    id.vars = "report_date", 
    variable.name = "metric_name", 
    value.name = "value",
    variable.factor = F
  ) %>% .[ # Temporary code to allign temperature data with hospital data
    as.Date(report_date) <= as.Date("2025-01-29")
  ]


# Join data
df_occ <- 
  df_occ %>% 
  bind_rows(
    df_pl, 
    copy(df_t)[, provider := "BRI"], 
    copy(df_t)[, provider := "NBT"])



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
    paed_m = paed,
    los_m = los,
    # Impute (simple moving average, window=7)
    occ = occ %>% na_ma(k = 3, weighting = "simple"),
    core = core %>% na_ma(k = 3, weighting = "simple"),
    dis = dis %>% na_ma(k = 3, weighting = "simple"),
    adm = adm %>% na_ma(k = 3, weighting = "simple"),
    paed = paed %>% na_ma(k = 3, weighting = "simple"),
    los = los %>% na_ma(k = 3, weighting = "simple"),
    tmin = tmin %>% na_ma(k = 3, weighting = "simple"), # shouldn't be necessary
    tmax = tmax %>% na_ma(k = 3, weighting = "simple") # shouldn't be necessary
  ) %>% 
  ungroup()


# Resize ts as week multiple
ts_occ <- 
  ts_occ %>% 
  group_by(site) %>% 
  slice(1:(floor(n() / 7) * 7)) %>% 
  ungroup()



# Process data -----------------------------------------------------------------
# # Add total core beds (used later to compute relative site weight)
# ts_occ <-
#   ts_occ %>% 
#   as_tibble() %>% group_by(index) %>% 
#   summarise(core_all = sum(core)) %>% 
#   full_join(ts_occ, by = c("index")) %>% 
#   as_tsibble(index = index, key = "site")

# Process bed occupation and escalation
ts_occ <- 
  ts_occ %>% 
  group_by(site) %>% 
  mutate(
    # Escalation
    escal = # threshold above core
      (occ - core) %>% if_else(. < 0, 0, .),
    # Occupancy
    occ = # stabilise (- holidays effect & detrend)
      stabilise(occ, index, .xdays = TRUE, .detrend = TRUE)
  ) %>% 
  ungroup()


# Aggregate BRI/Southmead
ts_occ <- 
  ts_occ %>%
  aggregate_key(
    site, 
    occ = sum(occ),
    adm = sum(adm),
    dis = sum(dis),
    escal = sum(escal),
    core = sum(core),
    paed = sum(paed),
    los = sum(los),
    tmax = unique(tmax), # = across sites; necessary to return 1 value
    tmin = unique(tmin), # = across sites; necessary to return 1 value
    occ_m = sum(occ_m),
    adm_m = sum(adm_m),
    dis_m = sum(dis_m),
    escal_m = sum(escal_m),
    core_m = sum(core_m),
    paed_m = sum(paed_m),
    los_m = sum(los_m)
  )


# Process admissions-discharges
ts_occ <- 
  ts_occ %>% 
  group_by(site) %>% 
  mutate(
    # New variables
    ad_diff = adm - dis, # difference
    ad_diff = # stabilise (-holidays/week days effect)
      stabilise(ad_diff, index, .xdays = TRUE, .wdays = TRUE),
    ad_diff2 = c(0, diff(ad_diff)), # rate of change of ad_diff
    ad_diff3 = c(0, 0, diff(ad_diff, differences = 2)), # rate of rate of change
    
    # Filter
    ad_diff_f = slide_dbl(ad_diff, mean, .before = 2),
    ad_diff2_f = slide_dbl(ad_diff2, mean, .before = 2),
    ad_diff3_f = slide_dbl(ad_diff3, mean, .before = 2)
    
    # # Z-score
    # core = (1 - mean(occ)) / sd(occ), # as 100% ref for new occ
    # occ = zs_fun(occ),
    # ad_diff = zs_fun(ad_diff),
    # ad_diff2 = zs_fun(ad_diff2),
    # ad_diff_f = zs_fun(ad_diff_f),
    # ad_diff2_f = zs_fun(ad_diff2_f),
    # ad_diff3_f = zs_fun(ad_diff3_f),
  ) %>% 
  ungroup()


# Length of stay (+21)
ts_occ <- 
  ts_occ %>% 
  group_by(site) %>% 
  mutate(
    # Remove sudden drops from BRI (replace with moving avg)
    mask = # iqr rule to detect (left-tail) outliers 
      los < quantile(los)[2] - 1.5 * (quantile(los)[4] - quantile(los)[2]),
    mavg = slide_dbl(los, mean, .before = 5, .after = 5),
    los = if_else(mask, mavg, los),
    mask = NULL,
    mavg = NULL,
    # Filter
    los = # stabilise (-holidays/week days effect)
      stabilise(los, index, .xdays = TRUE, .wdays = TRUE),
  ) %>% 
  ungroup()


# Process A&E paediatric
ts_occ <- 
  ts_occ %>% 
  group_by(site) %>% 
  mutate(
    paed = # stabilise (-holidays/week days effect)
      stabilise(paed, index, .xdays = TRUE, .wdays = TRUE),
  ) %>% 
  ungroup()


# Add bed occupancy from other hospital
ts_occ <- 
  ts_occ %>% 
  index_by(index) %>%
  mutate(
    tmp_mask = site %in% c("BRI", "Southmead"),
    occ_other = 
      occ[tmp_mask] %>% 
      rev() %>% 
      .[tmp_mask %>% if_else(row_number(), NA) %>%  rank(na.last = "keep")],
    tmp_mask = NULL
  ) %>% 
  ungroup()


# Add days of week and numeric time index
ts_occ <- 
  ts_occ %>%   
    mutate(
      days_ = format(index, "%a") |> 
        factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
      # t_ax = rep(1:(length(days_)/2), 2)
      t_ax = index %>% as.numeric(),
      t_ax = t_ax - min(t_ax) + 1
    )



# Save ts ---------------------------------------------------------------------
save_path <- here("data/processed/")
if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

saveRDS(ts_occ, file = paste0(save_path, 'tbl_occ.RDS'))
