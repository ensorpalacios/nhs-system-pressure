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
#' @param .hist Historical data
plot_fc <- function(.fc, .hist, .thr, .site) {

  .fc <- .fc[site == .site]
  .hist <- .hist[site == .site]
  .thr <- .thr[site == .site, thr]

  .hist = .hist[index >= max(index) - lubridate::dweeks(2), ]
  compute_quantiles <- function(q, .data) {
    quantile(.data, p = c(q))
  }
  .fc[, occ := dist_normal(occ_mean, sqrt(occ_var))]
  .fc[,
    # get percentiles
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

# Combine for the forecast layer
.fc_connected <- bind_rows(bridge, .fc)


  # Plot
  .fc_connected |>
    ggplot() +
  geom_ribbon(data = .fc_connected,
    aes(x = index, ymin = `10%`, ymax = `90%`, fill = "80%"),
    alpha = 0.15
  ) +
  geom_ribbon(data = .fc_connected,
    aes(x = index, ymin = `25%`, ymax = `75%`, fill = "50%"),
    alpha = 0.35
  ) +
  geom_line(data = .fc_connected, 
            aes(x = index, y = mean(occ)), 
            color = "steelblue", linewidth = 1, linetype = "dashed") +
  geom_line(data = .hist, 
            aes(x = index, y = occ), 
            linewidth = 1.2, color = "grey20") +
  geom_hline(yintercept = .thr, colour = "firebrick", linetype = "dashed", linewidth = 1) +
  annotate("text", x = min(.hist$index), y = .thr, label = str_c("Threshold (", round(.thr), ")"), 
           vjust = -1, hjust = 0, color = "firebrick", fontface = "italic", size = 4) +
  scale_x_date(
    labels = \(x) format(x, "%a\n%d-%m"),
    breaks = "2 days",
    expand = expansion(mult = c(0, 0.05)) # Removes extra whitespace at start
  ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(
    values = c("50%" = "steelblue", "80%" = "steelblue4")
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 12, color = "grey30"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
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



#' Daily risk plot
#' Function for plotting aily risk predictions
#' @param .risk_d Risk predictions
#' @param .site Site to display
plot_riskd <- function(
  .risk_d,
  .risk_ws_close,
  .risk_ws_far,
  .risk_w,
  .site,
  .type
) {
  # Get data
  .risk_d <- .risk_d[site == .site, ]
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
  y_close <- .risk_d[1:3, max(risk_day)] + 0.3
  y_far <- .risk_d[4:7, max(risk_day)] + 0.3
  y_week <- .risk_d[, max(risk_day)] + 0.5
  max_y <- ifelse((y_week + 0.2) > 1, y_week + 0.2, 1)

  # Plot
  tmp_plot =
    .risk_d |>
    ggplot(aes(x = index, y = risk_day, fill = risk_day)) +
    geom_col() +
    scale_y_continuous(name = NULL, limits = c(0, max_y), breaks = NULL) +
    scale_x_date(labels = \(x) format(x, "%a\n%d-%m"), breaks = "day") +
    geom_hline(yintercept = 0) + # baseline
    scale_fill_gradient(
      low = "#56B1F7",
      high = "#132B43",
      limits = c(0, 1),
      labels = scales::percent
    ) + # week line
    geom_text(
      aes(label = scales::percent(round(risk_day, 2))),
      # linewidth = NA,
      # fill = "white",
      # hjust = 0.5,
      vjust = -0.2,
      size = 6
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
    )

  if (.type == "daily risk") {
    tmp_plot
  } else {
    tmp_plot +
      geom_segment(
        aes(x = x_close_s, xend = x_close_e, y = y_close, yend = y_close)
      ) + # 3 days line
      geom_segment(
        aes(x = x_far_s, xend = x_far_e, y = y_far, yend = y_far)
      ) + # 4 days line
      geom_segment(
        aes(x = x_close_s, xend = x_far_e, y = y_week, yend = y_week)
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
      )
  }
  # .risk_d |>
  #   ggplot(aes(x = index, y = risk_day, fill = risk_day)) +
  #   geom_col() +
  #   scale_y_continuous(name = NULL, limits = c(0, max_y), breaks = NULL) +
  #   scale_x_date(labels = \(x) format(x, "%a\n%d-%m"), breaks = "day") +
  #   geom_hline(yintercept = 0) + # baseline
  # geom_segment(
  #   aes(x = x_close_s, xend = x_close_e, y = y_close, yend = y_close)
  # ) + # 3 days line
  # geom_segment(
  #   aes(x = x_far_s, xend = x_far_e, y = y_far, yend = y_far)
  # ) + # 4 days line
  # geom_segment(
  #   aes(x = x_close_s, xend = x_far_e, y = y_week, yend = y_week)
  # ) + # week line
  # geom_text(
  #   aes(label = scales::percent(round(risk_day, 2))),
  #   # linewidth = NA,
  #   # fill = "white",
  #   # hjust = 0.5,
  #   vjust = -0.2,
  #   size = 6
  # ) +
  # annotate(
  #   geom = "text",
  #   label = sprintf("%.0f%%", .risk_ws_close[, risk_ws] * 100),
  #   x = x_close_l,
  #   y = y_close,
  #   vjust = -0.2,
  #   # linewidth = NA,
  #   # fill = "white",
  #   size = 6
  # ) +
  # annotate(
  #   geom = "text",
  #   label = sprintf("%.0f%%", .risk_ws_far[, risk_ws] * 100),
  #   x = x_far_l,
  #   y = y_far,
  #   vjust = -0.2,
  #   # linewidth = NA,
  #   # fill = "text",
  #   size = 6
  # ) +
  # annotate(
  #   geom = "text",
  #   label = sprintf("%.0f%%", .risk_w[, risk_w] * 100),
  #   x = x_week_l,
  #   y = y_week,
  #   vjust = -0.2,
  #   # linewidth = NA,
  #   # fill = "white",
  #   size = 6
  # ) +
  # scale_fill_gradient(
  #   low = "#56B1F7",
  #   high = "#132B43",
  #   limits = c(0, 1),
  #   labels = scales::percent
  # ) +
  # theme(
  #   axis.title.x = element_blank(),
  #   axis.text = element_text(size = 14),
  #   strip.text = element_text(size = 24),
  #   strip.background = element_rect(fill = "white", linewidth = 0),
  #   panel.border = element_rect(linewidth = 0),
  #   panel.background = element_rect(fill = "white"),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   legend.position = "none"
  #   # legend.text = element_text(size = 14),
  #   # legend.title = element_text(size = 14, margin = margin(b = 15)),
  #   # legend.background = element_rect(colour = "black", linewidth = 1),
  #   # legend.margin = margin(t = 5, r = 4, b = 10, l = 4)
  # )
}