library(shiny)
library(data.table)
library(distributional)
library(dplyr)
library(stringr)
library(lubridate)
library(bslib)
library(fontawesome)
library(ggplot2)
library(here)
library(targets)
library(htmltools)

ui <- page_sidebar(
  # Theme
  theme = bs_theme(
    bg = "#f8f8f8", # Background color
    fg = "#000000" # Foreground (text) color
  ),

  title = "Bed occupancy forecast",

  # Select date
  sidebar = sidebar(
    dateInput("date", "Prediction date", value = lubridate::today())
  ),
  
  # Add custom CSS to center value box content
  tags$head(
    tags$link(
      rel = "stylesheet", 
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    ),
    tags$style(HTML("
      .bslib-value-box .value-box-value {
        text-align: center !important;
      }
      .bslib-value-box .value-box-title {
        text-align: center !important;
      }
      .bslib-value-box {
        text-align: center !important;
      }
    "))
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

  # TODO: Add custom CSS if needed
  # includeCSS("styles.css"),

  # sidebar = ,

  # useBusyIndicators(),

  # 🏷️ Header
  # h3(textOutput("show_title")),
  # verbatimTextOutput("show_info") |>
  #   tagAppendAttributes(style = "max-height: 100px; overflow: auto;"),

  # Value boxes - First row (General info)
  layout_columns(
    fill = FALSE,
    value_box(
      "Data from:",
      value = uiOutput("report_date", inline = TRUE)
    )
  ),

  # Value boxes - Site-specific rows
  div(
    style = "margin: 10px; padding: 0;",
    uiOutput("site_value_boxes")
  ),

  layout_columns(
    style = "min-height: 450px;",
    col_widths = c(12),

    # 📊 Plot 1
    navset_card_tab(
      # full_screen = TRUE,

      nav_panel(
        "Daily risk prediction",
        plotOutput("daily_risk")
      ),
      nav_panel(
        "Bed occupancy forecast",
        plotOutput("forecast")
      )
    )
  )
)

server <- function(input, output, session) {
  # Check changes in data directory
  get_dates <- reactiveFileReader(
    60000, # check every minute
    session,
    here("target/data/output"),
    function(path) {
      if (dir.exists(path)) {
        files <- list.files(path, pattern = "\\.RDS$")
        req(length(files) > 0)
        gsub(".RDS", "", files) |> as.Date()
      } else {
        NULL
      }
    }
  )

  # Update widget values when files change
  observe({
    list_dates <- get_dates()

    updateDateInput(
      session,
      "date",
      "Prediction date",
      value = max(list_dates),
      min = min(list_dates),
      max = max(list_dates)
    )
  })

  # Load data
  get_data <- reactive({
    req(input$date)
    list_dates <- get_dates() # only to create dependency on file changes

    path_data <- here("target/data/output")
    file_path <- file.path(path_data, paste0(input$date, ".RDS"))

    if (file.exists(file_path)) {
      readRDS(file_path)
    } else {
      NULL
    }
  })

  # For displaying the selected date
  output$report_date <- renderUI({
    if (!is.null(get_data())) {
      input$date |>
        format("%A, %d/%m/%Y") %>%
        shiny::HTML()
    } else {
      shiny::HTML(paste0("No forecast for ", format(input$date, "%A, %d/%m/%y")))
    }
  })

  # Create site-specific value boxes dynamically
  output$site_value_boxes <- renderUI({
    # Get data
    req(get_data()) # protect agains null data
    data <- get_data()

    # Get unique sites
    sites <- unique(data[["fc"]]$site)

    # Create a row of value boxes for each site
    thr <- data[["threshold"]]
    risk_w <- data[["risk"]][["risk_w"]]
    risk_ws <- data[["risk"]][["risk_ws"]]

    site_rows <- lapply(sites, function(site_name) {
      threshold <- #threshold
        thr[site == site_name, thr]

      weekly_risk <- # weekly risk predictions
        risk_w[.model == "crps" & site == site_name, scales::percent(risk_w)]

      close_risk <- # predictions 1-3 days ahead
        risk_ws[
          .model == "crps" & site == site_name & week_split == "close",
          scales::percent(risk_ws)
        ]

      far_risk <- # predictions 4-7 days ahead
        risk_ws[
          .model == "crps" & site == site_name & week_split == "far",
          scales::percent(risk_ws)
        ]

      # Create layout_columns for this site with site name as first value box
      layout_columns(
        fill = FALSE,
        col_widths = c(3, 2, 2, 2, 2), # Equal width columns
        value_box(
          title = "",
          value = site_name,
          theme = "primary",
          height = "90px" # Set explicit height
        ),
        tooltip(
          value_box(
            title = "threshold (ℹ)",
            value = threshold,
            height = "90px"
          ),
          "okokokok",
          placement = "top"
        ),
        # tooltip(
        #   value_box(
        #     title = HTML(
        #       'threshold <i class="fas fa-info-circle info-icon"></i>'
        #     ),
        #     value = threshold,
        #     height = "90px"
        #   ),
        #   "okokokok",
        #   placement = "right"
        # ),
        value_box(
          "7-day risk",
          value = weekly_risk,
          height = "90px" # Set explicit height
        ),
        value_box(
          "1-3 days risk",
          value = close_risk,
          height = "90px" # Set explicit height
        ),
        value_box(
          "4-7 days risk",
          value = far_risk,
          height = "90px" # Set explicit height
        )
      )
    })

    # Return all site rows
    htmltools::tagList(site_rows)
  })

  output$daily_risk <- renderPlot({
    # Get data
    req(get_data()) # protect agains null data
    data <- get_data()
    risk_d <- data[["risk"]][["risk_d"]]

    # Plot
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
  })

  output$forecast <- renderPlot({
    # Get and prepare data
    req(get_data()) # protect agains null data
    data <- get_data()
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

    # Plot
    fc |>
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
        aes(yintercept = thr),
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
      facet_wrap(vars(site), ncol = 1, scales = "free_y") +
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
  })
}


shinyApp(ui, server)