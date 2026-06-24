library(shiny)
library(data.table)
library(distributional)
library(dplyr)
library(stringr)
library(lubridate)
library(bslib)
library(bsicons)
library(fontawesome)
library(ggplot2)
library(here)
library(targets)
library(htmltools)

source("shiny-functions.R")
source("00_prepare_data.R")

ui <- page_navbar(
  title = "Acute bed occupancy forecasts",
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#000000"
  ),

  # Master Layout: 2 Columns spanning the full width
  layout_columns(
    col_widths = c(7, 5),

    # --- LEFT COLUMN: Bed Occupancy Forecasts (Stacked) ---
    card(
      card_header(
        span(
          strong("Bed occupancy forecast "),
        )
      ),
      card_body(
        style = "display: flex; flex-direction: column; gap: 10px;",
        plotOutput("fc_bri", height = "100%"),
        plotOutput("fc_southmead", height = "100%"),
        div(
          tags$strong("Figure 1: "),
          HTML(
            "Acute bed occupancy forecast, 2 weeks bed occupancy history (black line), 1 week ahead forecast (mean, 50%, 80% prediction interval), and bed occupancy threshold (historic 90th percentile dotted line)."
          )
        )
      ),
    ),

    # --- RIGHT COLUMN: Risk Predictions (Stacked) ---
    card(
      card_header(
        span(
          strong("Risk predictions "),
        )
      ),
      card_body(
        style = "display: flex; flex-direction: column; gap: 10px;",
        plotOutput("bri_risk_a", height = "100%"),
        plotOutput("southmead_risk_a", height = "100%"),
        div(
          tags$strong("Figure 2: "),
          HTML(
            "Risk of bed occupancy forecast crossing the threshold over the next 7-days and at least once for day aggregates (first 3, last 4 days, whole week ahead)."
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  model <- "equal"

  # Get data
  risk_d <- as.data.table(model_out)[
    type == "risk_d" & .model == model,
    .(site, index, risk_day)
  ]
  risk_ws_close <- as.data.table(model_out)[
    type == "risk_ws" & .model == model & week_split == "close",
    .(site, risk_ws)
  ]
  risk_ws_far <- as.data.table(model_out)[
    type == "risk_ws" & .model == model & week_split == "far",
    .(site, risk_ws)
  ]
  risk_w <- as.data.table(model_out)[
    type == "risk_w" & .model == model,
    .(site, risk_w)
  ]
  fc <- as.data.table(model_out)[type == "forecast" & .model == model]
  thr <- as.data.table(model_out)[
    type == "risk_w" & .model == model,
    .(site, thr)
  ]
  hist <- as.data.table(historic_data)

  # Plot bed occupancy fc
  output$fc_bri <- renderPlot(
    plot_fc(fc, hist, thr, "BRI")
  )
  output$fc_southmead <- renderPlot(
    plot_fc(fc, hist, thr, "Southmead")
  )

  # Plot risk - daily + aggregate
  output$bri_risk_a <- renderPlot({
    plot_riskd(
      risk_d,
      risk_ws_close,
      risk_ws_far,
      risk_w,
      "BRI",
      "daily + aggregate"
    )
  })
  output$southmead_risk_a <- renderPlot({
    plot_riskd(
      risk_d,
      risk_ws_close,
      risk_ws_far,
      risk_w,
      "Southmead",
      "daily + aggregate"
    )
  })

  # Plot data time series
  output$data_bri <- renderPlot(
    plot_ts(hist, "BRI", input$data_ts)
  )
  output$data_southmead <- renderPlot(
    plot_ts(hist, "Southmead", input$data_ts)
  )
}

shinyApp(ui, server)