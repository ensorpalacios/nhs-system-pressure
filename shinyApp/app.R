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

source(here("shinyApp/shiny-functions.R"))

source("00_prepare_data.R")
# model_out <- readRDS(here("target/data/output/2026-02-18.RDS"))
# model_out$hist <- 
#   as.data.table(
#     list(
#       "site"=rep(c("BRI", "Southmead"), each=14),
#       "index" = seq.Date(as.Date("2025-11-02")-14, as.Date("2025-11-02")-1),
#       "occ" = 
#         c(mean(mean(as.data.table(model_out$fc)[site == "BRI" & .model == "arima_dadp_rec", occ])) + rnorm(14, sd=5),
#         mean(mean(as.data.table(model_out$fc)[site == "Southmead" & .model == "arima_dadp_rec", occ])) + rnorm(14, sd=5))
#     )
#   )

ui <- page_fillable(
  # Theme
  title = "",
  theme = bs_theme(
    bg = "#f8f8f8", # Background color
    fg = "#000000" # Foreground (text) color
  ),


  titlePanel("7 days ahead risk predictions"),

  layout_columns(
    col_widths = c(7, 5),
    div(
      strong("Bed occupancy forecast"),
      tooltip(
        bs_icon("info-circle"),
        "2 weeks bed occupancy history (black line), 1 week ahead forecast 
        (mean, 50%, 80% prediction interval), and bed occupancy threshold 
        (dotted line)",
      ) #,
      # style = "margin-bottom: 10px; margin-top: 10px; font-size: 16px;"
    ),
    div(
      strong("Risk predictions"),
      tooltip(
        bs_icon("info-circle"),
        "probability of bed occupancy forecast crossing the threshold
        by days and at least once for day aggregates (first 3, last 4 days, 
        whole week ahead)"
      ) #,
      # style = "margin-bottom: 10px; margin-top: 10px; font-size: 16px;"
    ),
  ),
    # BRI card
    card(
      card_header(strong("BRI")),
      card_body(
        layout_columns(
          col_widths = c(7, 5),
          plotOutput("fc_bri", height = "300px"),
          plotOutput("bri_risk", height = "300px")
        )
      )
    ),

  # Southmead card
  card(
    card_header(strong("Southmead")),
    card_body(
      layout_columns(
        col_widths = c(7, 5),
        plotOutput("fc_southmead", height = "300px"),
        plotOutput("southmead_risk", height = "300px")
      )
    )
  )
)

server <- function(input, output) {
  # Choose model
  model <- "crps"

  # Get data
  risk_d <- as.data.table(model_out)[type == "risk_d" & .model == model, .(site, index, risk_day)]
  risk_ws_close <- as.data.table(model_out)[type == "risk_ws" & .model == model & week_split == "close", .(site, risk_ws)]
  risk_ws_far <- as.data.table(model_out)[type == "risk_ws" & .model == model & week_split == "far", .(site, risk_ws)]
  risk_w <- as.data.table(model_out)[type == "risk_w" & .model == model, .(site, risk_w)]
  fc <- as.data.table(model_out)[type == "forecast" & .model == model]
  thr <- as.data.table(model_out)[type == "risk_w" & .model == model, .(site, thr)]
  hist <- as.data.table(historic_data)

  # Plot risk
  output$bri_risk <- renderPlot({
    plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "BRI")
  })

  output$southmead_risk <- renderPlot({
    plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "Southmead")
  })

  # Plot bed occupancy
  output$fc_bri <- renderPlot(
    plot_fc(fc, hist, thr, "BRI")
  )

  output$fc_southmead <- renderPlot(
    plot_fc(fc, hist, thr, "Southmead")
  )
}


shinyApp(ui, server)