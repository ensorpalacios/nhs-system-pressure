#' Shiny functions
#'
#' Functions used for plotting in server
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2026-03-09

#' Forecast plot
#' Function for plotting bed occupancy historical data (2 weeks) and forecasts (1 week)
#' @param .fc Forecasts
#' @param .thr Threshold for high system pressure
#' @param .site Site to display
#' @param .hist Historical data??
plot_fc <- function(.fc, .hist, .thr, .site) {
  .fc <- .fc[site == .site]
  .hist <- .hist[site == .site]
  .thr <- .thr[site == .site, thr]

  compute_quantiles <- function(q, .data) {
    quantile(.data, p = c(q))
  }
  .fc[, occ := dist_normal(occ_mean, occ_var)]
  .fc[,
    # get percentiles
    c("10%", "25%", "75%", "90%") := lapply(
      c(0.1, 0.25, 0.75, .9),
      compute_quantiles,
      .data = .SD[, occ]
    )
  ]

  # Plot
  .fc |>
    ggplot(aes(x = index, y = mean(occ))) +
    geom_line() +
    geom_ribbon(
      aes(ymin = `25%`, ymax = `75%`, fill = "50%"),
      alpha = 0.3
    ) +
    geom_ribbon(
      aes(ymin = `10%`, ymax = `90%`, fill = "80%"),
      alpha = 0.1
    ) +
    geom_hline(
      aes(yintercept = .thr),
      colour = "red",
      linetype = "dotted",
      linewidth = 1.5
    ) +
    geom_line(data = .hist, aes(x = index, y = occ)) +
    # scale_y_continuous(name = "none") +
    scale_x_date(
      labels = \(x) {
        format(x, "%a\n%d-%m")
      },
      breaks = "2 day",
      name = "bed occupancy"
    ) +
    scale_fill_manual(
      name = "Forecast\ninterval",
      values = c("50%" = "steelblue", "80%" = "steelblue")
    ) +
    # facet_wrap(vars(site), ncol = 1, scales = "free_y") +
    theme(
      # axis.title.y = element_text(size = 14),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 14),
      # strip.text = element_text(size = 24),
      strip.text = element_blank(),
      strip.background = element_rect(fill = "white", linewidth = 0),
      legend.position = "none"
      # legend.text = element_text(size = 14),
      # legend.title = element_text(size = 14, margin = margin(b = 15)),
      # legend.margin = margin(t = 5, r = 4, b = 5, l = 4),
      # legend.background = element_rect(colour = "black", linewidth = 1)
    )
}

#' Daily risk plot
#' Function for plotting aily risk predictions
#' @param .risk_d Risk predictions
#' @param .site Site to display
plot_riskd <- function(.risk_d, .risk_ws_close, .risk_ws_far, .risk_w, .site) {
  # Get data
  .risk_d <- .risk_d[site == .site]
  .risk_ws_close <- .risk_ws_close[site == .site]
  .risk_ws_far <- .risk_ws_far[site == .site]
  .risk_w <- .risk_w[site == .site]

  # Multi-day risk lines
  x_close_s <- .risk_d[1, index]
  x_close_e <- .risk_d[3, index]
  x_far_s <- .risk_d[4, index]
  x_far_e <- .risk_d[7, index]
  x_close_l <- .risk_d[2, index]
  x_far_l <- .risk_d[6, index] - 0.5
  x_week_l <- .risk_d[4, index]
  y_close <- .risk_d[1:3, max(risk_day)] +0.3
  y_far <- .risk_d[4:7, max(risk_day)] + 0.3
  y_week <- .risk_d[, max(risk_day)] + 0.5
  max_y <- ifelse((y_week + 0.2) > 1, y_week + 0.2, 1)
 
  # Plot
  .risk_d |>
    ggplot(aes(x = index, y = risk_day, fill = risk_day)) +
    geom_col() +
    scale_y_continuous(name = NULL, limits = c(0, max_y), breaks = NULL) +
    scale_x_date(labels = \(x) format(x, "%a\n%d-%m"), breaks = "day") +
    geom_hline(yintercept = 0) + # baseline
    geom_segment(
      aes(x = x_close_s, xend = x_close_e, y = y_close, yend = y_close)
    ) + # 3 days line
    geom_segment(
      aes(x = x_far_s, xend = x_far_e, y = y_far, yend = y_far)
    ) + # 4 days line
    geom_segment(
      aes(x = x_close_s, xend = x_far_e, y = y_week, yend = y_week)
    ) + # week line
    geom_text(
      aes(label = scales::percent(round(risk_day, 2))),
      # linewidth = NA,
      # fill = "white",
      # hjust = 0.5,
      vjust = -0.2,
      size = 6
    ) +
    annotate(
      geom = "text",
      label = sprintf("%.0f%%", .risk_ws_close[, risk_ws] * 100),
      x = x_close_l,
      y = y_close,
      vjust = -0.2,
      # linewidth = NA,
      # fill = "white",
      size = 6
    ) +
    annotate(
      geom = "text",
      label = sprintf("%.0f%%", .risk_ws_far[, risk_ws] * 100),
      x = x_far_l,
      y = y_far,
      vjust = -0.2,
      # linewidth = NA,
      # fill = "text",
      size = 6
    ) +
    annotate(
      geom = "text",
      label = sprintf("%.0f%%", .risk_w[, risk_w] * 100),
      x = x_week_l,
      y = y_week,
      vjust = -0.2,
      # linewidth = NA,
      # fill = "white",
      size = 6
    ) +
    # facet_wrap(vars(site), ncol = 1) +
    scale_fill_gradient(
      low = "#56B1F7",
      high = "#132B43",
      limits = c(0, 1),
      labels = scales::percent
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 24),
      strip.background = element_rect(fill = "white", linewidth = 0),
      panel.border = element_rect(linewidth = 0),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
      # legend.text = element_text(size = 14),
      # legend.title = element_text(size = 14, margin = margin(b = 15)),
      # legend.background = element_rect(colour = "black", linewidth = 1),
      # legend.margin = margin(t = 5, r = 4, b = 10, l = 4)
    )
}