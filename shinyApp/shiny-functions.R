#' Shiny functions
#'
#' Functions used for plotting in server
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2026-03-09

#' Default occupancy threshold
#' Suggests a starting per-site threshold value to pre-fill the user-editable
#' threshold inputs: the .prob percentile of occupancy over the last ~6
#' months (183 days) of historic data. Ported from the pipeline's old
#' compute_threshold() (target/R/target_functions.R) so it runs client-side
#' against the historic_data already loaded in the app, instead of a value
#' transported from target -- the user can freely override the suggestion.
#' @param .hist Historical data (data.table with site, index, occ)
#' @param .prob Percentile used for the suggested default
compute_threshold_default <- function(.hist, .prob = 0.9) {
  as.data.table(.hist)[
    order(index),
    .(thr = round(quantile(tail(occ, 183), probs = .prob))),
    by = site
  ]
}

#' Compute risk of threshold crossing
#' Reconstructs the forecast distribution from occ_mean/occ_var -- the
#' summary stats persisted to the database, using the same Gaussian
#' approximation plot_fc() already relies on for its prediction ribbons --
#' and computes the probability of crossing a user-supplied, per-site
#' occupancy threshold. Ported from the pipeline's old compute_risk()
#' (target/R/target_functions.R), now run client-side against a live
#' threshold instead of fixed historical percentiles computed in the
#' pipeline.
#' @param .fc Forecast (data.table with site, .model, index, h, occ_mean, occ_var)
#' @param .thr Per-site threshold (data.table with site, thr)
compute_risk <- function(.fc, .thr) {
  .fc <- as.data.table(.fc)
  .fc <- .fc[.thr, on = "site"]
  .fc[, occ := dist_normal(occ_mean, sqrt(occ_var))]

  # Risk by day
  risk_d <- copy(.fc)[, risk_day := 1 - cdf(occ, thr), by = .(site, .model, h)]
  risk_d[, week_split := ifelse(h <= 3, "close", "far")]

  # Risk by week split (1-3h, 4-7h)
  risk_ws <- risk_d[, .(risk_ws = 1 - prod(1 - risk_day)), by = .(site, .model, week_split)]

  # Risk by week
  risk_w <- risk_d[, .(risk_w = 1 - prod(1 - risk_day)), by = .(site, .model)]

  list("risk_d" = risk_d, "risk_ws" = risk_ws, "risk_w" = risk_w)
}


#' Forecast plot
#' Function for plotting bed occupancy historical data (2 weeks) and forecasts (1 week)
#' @param .fc Forecasts
#' @param .thr Threshold for high system pressure
#' @param .site Site to display
#' @param .hist Historical data
plot_fc <- function(.fc, .hist, .thr, .site) {
  
  # --- GLOBAL SCALING FACTOR ---
  # Change this ONE number to scale the entire plot up or down
  # 1 = Default size, 0.7 = 70% of original size, etc.
  sf <- 0.9  
  
  # CORE stock (could update this to be live from the data?)
  .core_stock = c(BRI = 656, Southmead = 916, WGH = 274)
  
  # --- SPOOF HOOK: Redirect WGH to Southmead data ---
  target_site <- .site
  
  .fc <- .fc[site == target_site]         
  .hist <- .hist[site == target_site]     
  .thr <- .thr()[site == target_site, thr]   
  
  pal <- get_site_palette(.site)
  .hist = .hist[index >= max(index) - lubridate::dweeks(2), ]
  compute_quantiles <- function(q, .data) {
    quantile(.data, p = c(q))
  }
  .fc[, occ := dist_normal(occ_mean, sqrt(occ_var))]
  .fc[,
      c("10%", "25%", "75%", "90%") := lapply(
        c(0.1, 0.25, 0.75, .9),
        compute_quantiles,
        .data = .SD[, occ]
      )
  ]
  
  bridge <- .hist |> 
    slice_max(index, n = 1) |> 
    mutate(
      `mean(occ)` = occ,
      `10%` = occ, `90%` = occ,
      `25%` = occ, `75%` = occ
    )
  
  .fc_connected <- bind_rows(bridge, .fc) %>% mutate(occ_display = mean(if_else(is.na(.mean), occ, .mean)), .by = index)
  .fc_connected <- .fc_connected |> mutate(site = .site)
  .hist <- .hist |> mutate(site = .site)
  
  # --- CALCULATE CUSTOM X-AXIS BREAKS ---
  # Anchor the 3-day sequence on the final day of historical data
  anchor_date <- max(.hist$index, na.rm = TRUE)
  min_date <- min(.hist$index, na.rm = TRUE)
  max_date <- max(.fc_connected$index, na.rm = TRUE)
  
  custom_breaks <- sort(unique(c(
    seq(anchor_date, min_date, by = "-3 days"),
    seq(anchor_date, max_date, by = "3 days")
  )))
  
  .fc_connected |>
    ggplot() +
    # Threshold region
    annotate(
      "rect",
      xmin = as.Date(-Inf),
      xmax = as.Date(Inf),
      ymin = .thr,
      ymax = Inf,
      fill = "firebrick",
      alpha = 0.05
    ) +
    # Threshold region
    annotate(
      "rect",
      xmin = as.Date(-Inf),
      xmax = as.Date(Inf),
      ymax = .core_stock[target_site],
      ymin = -Inf,
      fill = "lightblue",
      alpha = 0.1
    ) +
    geom_hline(
      yintercept = .thr,
      colour = "firebrick",
      linetype = "solid",
      linewidth = 1 * sf     # SCALED
    ) +
    annotate(
      "text",
      x = min(.hist$index, na.rm = TRUE),
      y = .thr,
      label = str_c("Threshold (", round(.thr), ")"),
      vjust = -1,
      hjust = 0,
      # color = "firebrick",
      fontface = "italic",
      size = 5 * sf          # SCALED
    ) +
    geom_hline(
      yintercept = .core_stock[target_site],
      colour = "lightblue3",
      linetype = "solid",
      linewidth = 1 * sf       # SCALED
    ) +
    annotate(
      "text",
      x = min(.hist$index, na.rm = TRUE),
      y = .core_stock[target_site],
      label = str_c("Core stock open (", round(.core_stock[target_site]), ")"),
      vjust = 1.5,
      hjust = 0,
      # color = "firebrick",
      fontface = "italic",
      size = 5 * sf          # SCALED
    ) +
    # Prediction Intervals
    geom_ribbon(
      data = .fc_connected,
      aes(x = index, ymin = `10%`, ymax = `90%`, fill = "80%"),
      alpha = 0.15
    ) +
    geom_ribbon(
      data = .fc_connected,
      aes(x = index, ymin = `25%`, ymax = `75%`, fill = "50%"),
      alpha = 0.35
    ) +
    # Mean Forecast Line (Themed)
    geom_pointpath(
      data = .fc_connected,
      aes(x = index, y = occ_display),
      color = pal$primary,
      size = 1.5 * sf,         # SCALED
      shape = NA,
      linewidth = 1 * sf     # SCALED
    ) +
    geom_point_interactive(
      # Drop the first 'bridge' row so the cross doesn't render on top of the historical point
      data = .fc_connected |> slice(-1), 
      aes(
        x = index, 
        y = occ_display,
        tooltip = str_c(target_site, ", ", format(index, "%b %d"),
                        "\n",
                        "Forecasted occupancy: ", scales::comma(round(occ_display)))
      ),
      shape = 4,           # 4 is an 'X' cross, 3 is a '+' cross
      color = pal$primary,
      size = 2.5 * sf,       # SCALED: Controls the overall diameter of the cross
      stroke = 2 * sf        # SCALED: Controls the thickness of the lines making up the cross
    ) +
    geom_pointpath(
      data = .hist,
      color = pal$mid,
      aes(x = index, y = occ),
      linewidth = 1.2 * sf,  # SCALED
      size = 1.5 * sf,         # SCALED
      shape = NA,
      color = "grey20"
    ) +
    geom_point_interactive(data = .hist,
                           aes(x = index, y = occ, tooltip = str_c(
                             target_site,
                             ", ",
                             format(index, "%b %d"),
                             "\n",
                             "Bed occupancy: ",
                             scales::comma(round(occ), big.mark = ","))),
                           color = pal$mid,
                           size = 3 * sf) + # SCALED
    scale_x_date(
      labels = function(x) {
        base_lbls <- format(x, "%a %d")
        months <- format(x, "%b")
        month_changed <- c(TRUE, months[-1] != months[-length(months)])
        month_changed[is.na(month_changed)] <- FALSE
        
        first_valid_idx <- which(!is.na(months))[1]
        if (!is.na(first_valid_idx)) {
          month_changed[first_valid_idx] <- TRUE
        }
        
        out_lbls <- ifelse(
          month_changed,
          paste0(months, "\n", base_lbls),
          base_lbls
        )
        out_lbls[is.na(x)] <- ""
        return(out_lbls)
      },
      breaks = custom_breaks,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(limits = c(0.9*.core_stock[target_site], NA), expand = expansion(mult = c(0.25, 0.25))) +
    # Dynamic Interval Palette mapping
    scale_fill_manual(
      values = c("50%" = pal$mid, "80%" = pal$dark) # 🟦 Themed Ribbon intervals
    ) +
    labs(y = "Acute bed occupancy") +
    facet_wrap(~site, strip.position = "top") +
    theme_minimal(base_size = 11 * sf) + # SCALED BASE SIZE
    theme(
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 20 * sf, color = "grey30", face = "bold"), # SCALED
      axis.text = element_text(size = 12 * sf, color = "grey30"),                   # SCALED
      panel.grid.minor = element_blank(),
      legend.position = "none",
      # Styled Horizontal Row Banner
      strip.placement = "inside",
      strip.text = element_text(
        size = 16 * sf,                                                             # SCALED
        face = "bold",
        color = pal$mid,
        hjust = 0.02
      ),
      strip.background = element_rect(fill = pal$light, color = NA) # 🟦 Themed background
    )
}

#' Time series plot
#' Function for plotting training data time series
#' @param .hist Historical data
#' @param .site Site to display
#' @param .var Variable to plot (character)
plot_ts <- function(.hist, .site, .var) {
  if (.var != "temp") {
    tmp_ts <- .hist[site == .site, .SD, .SDcols = c("index", .var)]
    tmp_ts <- tmp_ts[index >= max(index) - lubridate::dweeks(16)]
    tmp_ts |>
      ggplot(aes(x = index, y = !!sym(.var))) +
      geom_line() +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(size = 14)
      )
  } else {
    tmp_ts <- .hist[site == .site, .SD, .SDcols = c("index", "tmax", "tmin")]
    tmp_ts <- tmp_ts[index >= max(index) - lubridate::dweeks(16)]
    tmp_plot <-
      tmp_ts |>
      ggplot(aes(x = index)) +
      geom_line(aes(y = tmax, colour = "tmax")) +
      geom_line(aes(y = tmin, colour = "tmin")) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(size = 14)
      )
    if (.site == "BRI") {
      tmp_plot +
        theme(
          legend.title = element_blank(),
          legend.position = c(0.5, 1),
          legend.direction = "horizontal",
          legend.justification = c(0.5, 1),
          legend.text = element_text(size = 14, colour = "black"),
          # legend.key.width = unit(2, "cm"),
          # legend.key.height = unit(0.6, "cm"),
          # legend.spacing.x = unit(0.5, "cm"),
          # legend.spacing.y = unit(0.2, "cm"),
          # legend.margin = margin(t = 5, b = 5),
          legend.box.background = element_rect(
            fill = "white",
            colour = "grey70"
          )
        )
    } else {
      {
        tmp_plot +
          theme(
            legend.position = "none"
          )
      }
    }
  }
}



plot_riskd <- function(
    .risk_d,
    .risk_ws_close,
    .risk_ws_far,
    .risk_w,
    .site,
    .type
) {
  
  # --- GLOBAL SCALING FACTOR ---
  sf <- 0.9  
  
  # --- BAR EXTENSION ---
  # 0 = center of the bar. 0.45 = touches the very edge of the bar.
  # 0.35 extends nicely past the center without touching the sides.
  bar_ext <- 0.35 
  
  # Get data
  target_site <- .site
  
  .risk_d <- .risk_d[site == target_site, ]
  .risk_ws_close <- .risk_ws_close[site == target_site]
  .risk_ws_far <- .risk_ws_far[site == target_site]
  .risk_w <- .risk_w[site == target_site]
  
  pal <- get_site_palette(.site)
  
  .risk_d <- .risk_d |> mutate(site = .site)
  
  # Multi-day risk lines (with fractional offset applied to stretch the segments)
  x_close_s <- .risk_d[1, index] - bar_ext
  x_close_e <- .risk_d[3, index] + bar_ext
  x_far_s <- .risk_d[4, index] - bar_ext
  x_far_e <- .risk_d[7, index] + bar_ext
  
  # Label positions stay anchored to the centers
  x_close_l <- .risk_d[2, index]
  x_far_l <- .risk_d[6, index] - 0.5
  x_week_l <- .risk_d[4, index]
  
  y_close <- .risk_d[1:3, max(risk_day)] + 0.3
  y_far <- .risk_d[4:7, max(risk_day)] + 0.3
  y_week <- .risk_d[, max(risk_day)] + 0.5
  max_y <- ifelse((y_week + 0.2) > 1, y_week + 0.2, 1)
  
  # Plot
  tmp_plot <-
    .risk_d |>
    ggplot(aes(x = index, y = risk_day)) +
    geom_col_interactive(
      fill = pal$primary,
      aes(tooltip = str_c(target_site, ", ", format(index, "%b %d"),
                          "\n",
                          "Risk: ", scales::percent(risk_day)))
    ) + 
    scale_y_continuous(limits = c(0, max_y), breaks = NULL) +
    scale_x_date(labels = \(x) format(x, "%a\n%d-%m"), breaks = "day") +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 1 * sf) +  
    geom_text(
      aes(label = scales::percent(round(risk_day, 2))),
      vjust = -0.2,
      size = 5 * sf, 
      color = "grey20"
    ) +
    labs(y = "Risk of crossing occupancy threshold") +
    facet_wrap(~site, strip.position = "top") +
    theme_minimal(base_size = 11 * sf) + 
    theme(
      plot.margin = margin(t = 5, r = 10, b = 5, l = 10, unit = "pt"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 20 * sf, color = "grey30", face = "bold"), 
      axis.text = element_text(size = 12 * sf, color = "grey30"),                     
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.placement = "inside",
      strip.text = element_text(
        size = 16 * sf, 
        face = "bold", 
        color = pal$mid, 
        hjust = 0.02
      ),
      strip.background = element_rect(fill = pal$light, color = NA)
    )
  
  if (.type == "daily risk") {
    tmp_plot
  } else {
    tmp_plot +
      geom_segment(
        aes(x = x_close_s, xend = x_close_e, y = y_close, yend = y_close), 
        color = pal$primary, 
        linewidth = 1 * sf 
      ) +  
      geom_segment(
        aes(x = x_far_s, xend = x_far_e, y = y_far, yend = y_far), 
        color = pal$primary, 
        linewidth = 1 * sf 
      ) +  
      # Week line naturally adopts the start of 'close' and end of 'far' offsets
      geom_segment(
        aes(x = x_close_s, xend = x_far_e, y = y_week, yend = y_week), 
        color = pal$primary, 
        linewidth = 1 * sf 
      ) +
      annotate(
        geom = "text",
        label = sprintf("%.0f%%", .risk_ws_close[, risk_ws] * 100),
        x = x_close_l, y = y_close, vjust = -0.2, 
        size = 5 * sf, 
        fontface = "bold"
      ) +
      annotate(
        geom = "text",
        label = sprintf("%.0f%%", .risk_ws_far[, risk_ws] * 100),
        x = x_far_l, y = y_far, vjust = -0.2, 
        size = 5 * sf, 
        fontface = "bold"
      ) +
      annotate(
        geom = "text",
        label = sprintf("%.0f%%", .risk_w[, risk_w] * 100),
        x = x_week_l, y = y_week, vjust = -0.2, 
        size = 5 * sf, 
        fontface = "bold"
      )
  }
}



get_site_palette <- function(site) {
pal <- c('#66c2a5','#bebada','#8da0cb')
pal_scale <- purrr::map(pal, ~monochromeR::generate_palette(colour = .x, modification = "go_darker", n_colours = 4)) %>%
             purrr::map(~purrr::set_names(.x, nm = c("light", "primary", "mid", "dark"))) %>%
             purrr::map(as.list)


  if (site == "BRI") {
    pal_scale[[1]]
  } else if (site == "Southmead") { 
   pal_scale[[2]]
  } else if (site == "WGH") { 
    pal_scale[[3]]
  }
}
