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
  plot_fc <- function(.fc, .thr, .site) {
    .fc <- as.data.table(.fc)
    .fc <- .fc[site == .site]
    .thr <- .thr[site == .site, thr]

    compute_quantiles <- function(q, .data) {
      quantile(.data, p = c(q))
    }
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
      ggplot(aes(x = index, mean(occ))) +
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
        linetype = "dashed",
        linewidth = 1
      ) +
      scale_y_continuous(name = "bed occupancy") +
      scale_x_date(
        labels = \(x) {
          format(x, "%a\n%d-%m")
        },
        breaks = "day",
        name = "bed occupancy"
      ) +
      scale_fill_manual(
        name = "Forecast\ninterval",
        values = c("50%" = "steelblue", "80%" = "steelblue")
      ) +
      # facet_wrap(vars(site), ncol = 1, scales = "free_y") +
      theme(
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 24),
        strip.background = element_rect(fill = "white", linewidth = 0),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, margin = margin(b = 15)),
        legend.margin = margin(t = 5, r = 4, b = 5, l = 4),
        # panel.border = element_rect(linewidth = 0),
        legend.background = element_rect(colour = "black", linewidth = 1)
        # panel.background = element_rect(fill = "white")
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank()
      )
  }

#' Daily risk plot
#' Function for plotting aily risk predictions
#' @param .risk_d Risk predictions
#' @param .site Site to display 
  plot_riskd <- function(.risk_d, .site) {
    # Get data
    .risk_d <- .risk_d[site == .site]

    # Plot
    .risk_d |>
      ggplot(aes(x = index, y = risk_day, fill = risk_day)) +
      geom_col() +
      scale_y_continuous(name = NULL, limits = c(0, 1), breaks = NULL) +
      #scale_x_date(labels = \(x) format(x, "%a\n%d-%m"), breaks = "day") +
      geom_hline(yintercept = 0) +
      geom_label(
        aes(label = scales::percent(round(risk_day, 2))),
        linewidth = NA,
        fill = "white",
        hjust = 0.5,
        vjust = -0.5
      ) +
      # facet_wrap(vars(site), ncol = 1) +
      scale_fill_viridis_c(
        name = "Daily risk",
        option = "rocket",
        direction = -1,
        limits = c(0, 1),
        labels = scales::percent
      ) +
      # scale_fill_gradientn(
      # colors = c("#6699FF", "#", "#660000"),
      # limits = c(0, 1)
      # ) +
      theme(
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 24),
        strip.background = element_rect(fill = "white", linewidth = 0),
        panel.border = element_rect(linewidth = 0),
        panel.background = element_rect(fill = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, margin = margin(b = 15)),
        legend.background = element_rect(colour = "black", linewidth = 1),
        legend.margin = margin(t = 5, r = 4, b = 10, l = 4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }