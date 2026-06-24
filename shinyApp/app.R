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
library(ggh4x)
library(patchwork)

source("shiny-functions.R")
source("00_prepare_data.R")

# ==========================================
# 🎛️ RELATIVE LAYOUT & SCALING PARAMETERS
# ==========================================
DASHBOARD_MAX_WIDTH  <- "95vw"   # Max horizontal width (95% of viewport width)
VERTICAL_MARGIN      <- "2vh"    # Spacing at the top and bottom of the page
PLOT_GAP             <- "2.5rem" # Gap between the upper and lower charts inside a card

ui <- page_sidebar(
  title = "Acute bed occupancy forecasts", # Dashboard title is back!
  sidebar = NULL,                         # Kept hidden so plots take full width
  fillable = TRUE,                        # Forces the layout to fill the screen vertical space
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#000000"
  ),
  
  # Injecting flexible CSS properties to force vertical expansion underneath the header
  tags$head(
    tags$style(HTML(sprintf("
      /* Center the dashboard container, constrain width, and pad under title */
      .bslib-page-main {
        max-width: %s !important;
        margin-left: auto !important;
        margin-right: auto !important;
        padding-top: %s !important;
        padding-bottom: %s !important;
      }
      
      /* Force bslib card structures to fill vertical height completely */
      .card {
        flex: 1 1 auto !important;
        display: flex !important;
        flex-direction: column !important;
      }
      
      /* Turn card bodies into true flex columns that expand */
      .custom-card-body {
        display: flex !important;
        flex-direction: column !important;
        gap: %s !important;
        flex: 1 1 auto !important; 
        overflow: hidden !important;
      }
      
      /* Force the internal shiny container divs to expand equally */
      .custom-card-body .shiny-plot-output {
        flex: 1 1 0%% !important;
        min-height: 0 !important;
        height: 100%% !important; 
      }
      
      /* Keep the captions from shrinking */
      .custom-caption {
        font-size: 0.9rem;
        line-height: 1.4;
        flex: 0 0 auto !important;
        margin-top: 5px;
      }
    ", DASHBOARD_MAX_WIDTH, VERTICAL_MARGIN, VERTICAL_MARGIN, PLOT_GAP)))
  ),

  # Master 2-Column Layout
  layout_columns(
    col_widths = c(7, 5),

    # --- LEFT COLUMN: Bed Occupancy Forecasts (Stacked) ---
    card(
      # card_header(span(strong("Bed occupancy forecast"))),
      card_body(
        class = "custom-card-body",
        plotOutput("fc"),
        div(
          class = "custom-caption",
          tags$strong("Figure 1: "),
          HTML("Acute bed occupancy forecast with 2 weeks bed occupancy history (black line), 1 week ahead forecast (mean, 50%, 80% prediction interval), and bed occupancy threshold (historic 90th percentile dotted line).")
        )
      )
    ),

    # --- RIGHT COLUMN: Risk Predictions (Stacked) ---
    card(
      # card_header(span(strong("Risk predictions"))),
      card_body(
        class = "custom-card-body",
        plotOutput("risk"),
        div(
          class = "custom-caption",
          tags$strong("Figure 2: "),
          HTML("Risk of bed occupancy forecast crossing the threshold over the next 7-days and at least once for day aggregates (first 3, last 4 days, whole week ahead).")
        )
      )
    )
  )
)

server <- function(input, output) {
  model <- "equal"

  # Get data
  risk_d <- as.data.table(model_out)[type == "risk_d" & .model == model, .(site, index, risk_day)]
  risk_ws_close <- as.data.table(model_out)[type == "risk_ws" & .model == model & week_split == "close", .(site, risk_ws)]
  risk_ws_far <- as.data.table(model_out)[type == "risk_ws" & .model == model & week_split == "far", .(site, risk_ws)]
  risk_w <- as.data.table(model_out)[type == "risk_w" & .model == model, .(site, risk_w)]
  fc <- as.data.table(model_out)[type == "forecast" & .model == model]
  thr <- as.data.table(model_out)[type == "risk_w" & .model == model, .(site, thr)]
  hist <- as.data.table(historic_data)

  # Plot bed occupancy fc

  output$fc <- renderPlot({
    fc_bri <- plot_fc(fc, hist, thr, "BRI")
    fc_nbt <- plot_fc(fc, hist, thr, "Southmead")
    (fc_bri/fc_nbt/fc_nbt) + plot_layout(axes = "collect_y")
  })


  # Plot risk - daily + aggregate

  output$risk <- renderPlot({
    risk_bri <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "BRI", "daily + aggregate")
    risk_nbt <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "Southmead", "daily + aggregate")
    (risk_bri/risk_nbt/risk_nbt) + plot_layout(axes = "collect_y")
  })


  # Plot data time series
  output$data_bri <- renderPlot(plot_ts(hist, "BRI", input$data_ts))
  output$data_southmead <- renderPlot(plot_ts(hist, "Southmead", input$data_ts))
}

shinyApp(ui, server)