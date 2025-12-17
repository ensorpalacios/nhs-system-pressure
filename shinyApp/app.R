library(shiny)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(bslib)
library(fontawesome)
library(ggplot2)
library(here)
library(targets)

data <- readRDS(here("target/data/output/2025-12-16.RDS"))

theme_set(theme_minimal(base_size = 24))

ui <- page_sidebar(

  # Input data start forecast
  card(
      card_header("Date input"),
      dateInput("date", "Select date", value = "2014-01-01")
    ),
  
  # # reactable formatting
  # tags$head(
  #   tags$style(HTML("
  #   .reactable {
  #     width: 150% !important;  /* or whatever width you prefer */
  #   }
  #   .reactable .rt-td {
  #     white-space: nowrap !important;
  #     overflow: hidden;
  #     text-overflow: ellipsis;
  #   }
  # "))
  # ),
  
  style = "background-color: rgb(248, 248, 248);",
  title = "Acute bed pressure forecasts",
  
  # TODO: Add custom CSS if needed
  # includeCSS("styles.css"),
  
  # sidebar = ,
  
  # useBusyIndicators(),
  
  # 🏷️ Header
  # h3(textOutput("show_title")),
  # verbatimTextOutput("show_info") |>
  #   tagAppendAttributes(style = "max-height: 100px; overflow: auto;"),
  
  # 🎯 Value boxes - First row (General info)
  layout_columns(
    fill = FALSE,
    value_box(
      "Data from:",
      value = uiOutput("report_date", inline = TRUE)
    )
  ),
  
  # 🎯 Value boxes - Site-specific rows
  uiOutput("site_value_boxes"),
  
  layout_columns(
    style = "min-height: 450px;",
    col_widths = c(12),
    
    # 📊 Plot 1
    navset_card_tab(
      # full_screen = TRUE,
      nav_panel(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Daily risk prediction",
        ),
        plotOutput("daily_risk")
      ),
      nav_panel(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Bed occupancy forecast",
          plotOutput("forecast")
        )#,
        #plotOutput("ed_queue")
      )#,
      # nav_panel(
      #   card_header(
      #     class = "d-flex justify-content-between align-items-center",
      #     "Raw model projection data",
      #   )#,
      #   #reactableOutput("table", height = "100%")
      # )
    )
  )
)

server <- function(input, output, session) {
  output$report_date <- renderUI({
    data$date %>%
      format("%A, %d/%m/%Y") %>%
      shiny::HTML()
  })

  # Create site-specific value boxes dynamically
  output$site_value_boxes <- renderUI({
    # Get unique sites
    sites <- unique(data[["fc"]]$site)

    # Create a row of value boxes for each site
    risk_w <- data[["risk"]][["risk_w"]]
    risk_ws <- data[["risk"]][["risk_ws"]]
    site_rows <- lapply(sites, function(site_name) {
      # Get data for this site
      weekly_risk <-
        risk_w[.model == "crps" & site == site_name, scales::percent(risk_w)]

      close_risk <-
        risk_ws[
          .model == "crps" & site == site_name & week_split == "close",
          scales::percent(risk_ws)
        ]

      far_risk <-
        risk_ws[
          .model == "crps" & site == site_name & week_split == "far",
          scales::percent(risk_ws)
        ]

      # Create layout_columns for this site with site name as first value box
      layout_columns(
        fill = FALSE,
        col_widths = c(3, 3, 3, 3), # Equal width columns
        value_box(
          title = "",
          value = site_name,
          theme = "primary"
        ),
        value_box(
          "7-day risk",
          value = weekly_risk
        ),
        value_box(
          "1-3 days risk",
          value = close_risk
        ),
        value_box(
          "4-7 days risk",
          value = far_risk
        )
      )
    })

    # Return all site rows
    htmltools::tagList(site_rows)
  })

  output$daily_risk <- renderPlot({
    risk_d <- data[["risk"]][["risk_d"]]
    risk_d[.model == "crps"] |>
      ggplot(aes(x = index, y = risk_day, fill = risk_day)) +
      geom_col() +
      scale_y_continuous(name = NULL, limits = c(0, 1), breaks = NULL) +
      scale_x_date(labels = \(x) format(x, "%a\n%d-%m"), breaks = "day") +
      geom_hline(yintercept = 0) +
      geom_label(
        aes(label = scales::percent(round(risk_day, 2))),
        label.size = NA,
        fill = "white",
        hjust = 0.5,
        vjust = -0.5
      ) +
      facet_wrap(vars(site), ncol = 1) +
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
        strip.text = element_text(size = 24),
        strip.background = element_rect(fill = "white", linewidth = 0),
        panel.border = element_rect(linewidth = 0),
        legend.background = element_rect(colour = "black", linewidth = 1),
        # panel.background = element_rect(fill = "white")
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })

  output$forecast <- renderPlot({
    fc <- data[["fc"]]

    fc <- # add site-specific threshold
      data[["threshold"]][
        fc,
        on = "site"
      ]

    fc <- fc[.model == "crps"] # select model

    compute_quantiles <- function(q, .data) {
      quantile(.data, p = c(q))
    }
    fc[,
      # get percentiles
      c("10%", "25%", "75%", "90%") := lapply(
        c(0.1, 0.25, 0.75, .9),
        compute_quantiles,
        .data = .SD[, occ]
      )
    ]

    fc |>
      ggplot(aes(x = index, mean(occ))) +
      geom_line() +
      geom_ribbon(aes(ymin = `25%`, ymax = `75%`, fill = "50%"), alpha = 0.3) +
      geom_ribbon(aes(ymin = `10%`, ymax = `90%`, fill = "80%"), alpha = 0.1) +
      geom_hline(aes(yintercept = thr), colour = "red", linetype = "dashed") +
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
      facet_wrap(vars(site), ncol = 1, scales = "free_y") +
      theme(
        axis.title.x = element_blank(),
        strip.text = element_text(size = 24),
        strip.background = element_rect(fill = "white", linewidth = 0),
        # panel.border = element_rect(linewidth = 0),
        # legend.background = element_rect(colour = "black", linewidth = 1),
        # panel.background = element_rect(fill = "white")
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank()
      )
  })
}

shinyApp(ui, server)