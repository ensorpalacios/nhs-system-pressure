library(shiny)
library(dplyr)
library(stringr)
library(lubridate)
library(bslib)
library(fontawesome)
library(ggplot2)
library(here)
library(targets)

data <- readRDS("../target/data/2025-12-15.RDS")

theme_set(theme_minimal(base_size = 24))

ui <- page_sidebar(

  # reactable formatting
tags$head(
  tags$style(HTML("
    .reactable {
      width: 150% !important;  /* or whatever width you prefer */
    }
    .reactable .rt-td {
      white-space: nowrap !important;
      overflow: hidden;
      text-overflow: ellipsis;
    }
  "))
),

  style = "background-color: rgb(248, 248, 248);",
  title = "Acute bed pressure forecasts",

  # TODO: Add custom CSS if needed
  # includeCSS("styles.css"),

  # sidebar = ,

  useBusyIndicators(),

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
    full_screen = TRUE,
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
      )#,
      #plotOutput("ed_queue")
    ),
    nav_panel(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Raw model projection data",
      )#,
      #reactableOutput("table", height = "100%")
    )
  )
)
)

server <- function(input, output, session) {

  output$report_date <- renderUI({
    data$date %>%
      format("%A, %d/%m/%Y") %>%
      HTML()
  })
  
  # Create site-specific value boxes dynamically
  output$site_value_boxes <- renderUI({
    # Get unique sites
    sites <- data %>%
      `[[`("risk") %>%
      `[[`("risk_w") %>%
      filter(.model == "crps") %>%
      pull(site) %>%
      unique()
    
    # Create a row of value boxes for each site
    site_rows <- lapply(sites, function(site_name) {
      # Get data for this site
      weekly_risk <- data %>%
        `[[`("risk") %>%
        `[[`("risk_w") %>%
        filter(.model == "crps", site == site_name) %>%
        pull(risk_w) %>%
        scales::percent()
      
      close_risk <- data %>%
        `[[`("risk") %>%
        `[[`("risk_ws") %>%
        filter(.model == "crps", week_split == "close", site == site_name) %>%
        pull(risk_ws) %>%
        scales::percent()
      
      far_risk <- data %>%
        `[[`("risk") %>%
        `[[`("risk_ws") %>%
        filter(.model == "crps", week_split == "far", site == site_name) %>%
        pull(risk_ws) %>%
        scales::percent()
      
      # Create layout_columns for this site with site name as first value box
      layout_columns(
        fill = FALSE,
        col_widths = c(3, 3, 3, 3),  # Equal width columns
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
    tagList(site_rows)
  })
  
  output$daily_risk <- renderPlot({
    data %>% 
  `[[`("risk") %>%
  `[[`("risk_d") %>% 
  as.data.frame() %>%
  filter(.model == "crps") %>%
  ggplot(aes(x = index, y = risk_day)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  geom_label(aes(label = scales::percent(round(risk_day, 2))), label.size = NA, hjust = 0.5, vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = scales::breaks_width(0.25)) +
  scale_x_date(labels = \(x) format(x, "%a\n%d-%m"), breaks = "day") +
  # scale_fill_viridis_c(guide = FALSE, option = "inferno", begin = 0, end = 0.5) +
  facet_wrap(vars(site), ncol = 1) +
  theme_minimal(base_size = 24) + 
  theme(strip.placement = "outside") +
  labs(y = "Daily risk of breaching bed occupacny threshold", x = "")
  
  })

}

shinyApp(ui, server)