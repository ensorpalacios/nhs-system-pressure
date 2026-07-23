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
library(ggiraph)

source("shiny-functions.R")
source("00_prepare_data.R")

# ==========================================
# RELATIVE LAYOUT & SCALING PARAMETERS
# ==========================================
DASHBOARD_MAX_WIDTH  <- "90vw"   # Max horizontal width (95% of viewport width)
VERTICAL_MARGIN      <- "2px"    # Spacing at the top and bottom of the page
PLOT_GAP             <- "0.75rem" # Gap between the upper and lower charts inside a card

ui <- page_navbar(
  title = "Acute bed occupancy forecasts",
  fillable = TRUE,
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#000000"
  ),

tags$head(
    tags$style(HTML(sprintf(
      "
      /* Constrain, center, and force the grid to take up ALL remaining vertical space */
      .main-dashboard-grid {
        max-width: %s !important;
        margin-left: auto !important;
        margin-right: auto !important;
        width: 100%% !important;
        padding-top: %s !important;
        padding-bottom: %s !important;
        
        /* Dynamically consumes 100%% of leftover space instantly on load */
        flex: 1 1 auto !important;
        min-height: 0 !important; 
        height: auto !important;
      }
      
      /* 💡 THE FIX: Target ONLY the cards inside the plot grid, not the header card */
      .main-dashboard-grid .card {
        flex: 1 1 auto !important;
        display: flex !important;
        flex-direction: column !important;
        height: 100%% !important;
      }
      
      /* Turn card bodies into true flex columns */
      .custom-card-body {
        display: flex !important;
        flex-direction: column !important;
        gap: %s !important;
        flex: 1 1 auto !important; 
        overflow: hidden !important;
      }
      
      /* Allow plots to perfectly absorb 100%% of the allocated card body space */
      .custom-card-body .shiny-plot-output {
        flex: 1 1 0%% !important;
        min-height: 0 !important;
        height: 100%% !important; 
      }
      
      .tab-pane {
        padding-top: 8px !important;
        padding-bottom: 8px !important;
      }
    ",
      DASHBOARD_MAX_WIDTH,
      VERTICAL_MARGIN,
      VERTICAL_MARGIN,
      PLOT_GAP
    )))
  ),

  # --- TAB 1: FORECASTS & RISK ---
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),
    # Header text
    card(
      fill = FALSE,
      style = "background-color: #F4F6F9;
         /*border: 1px solid #e2e8f0;*/
         box-shadow: none;
         flex: 0 0 auto !important;",

      card_body(
        fill = FALSE,
        style = "padding: 0.75rem 1rem;", # Snug padding (Top/Bottom, Left/Right)

        tags$p(
          style = "margin: 0; font-size: 14px; color: #334155; line-height: 1.5;",
          tags$strong("Acute bed occupancy:"),
          " features a 2-week history (solid line with points), a 1-week ahead forecast (mean, 50% and 80% prediction intervals shown via dashed line and ribbons), and a high-occupancy threshold (historic 90th percentile, solid red line) and the current core bed-stock open (solid blue line)."
        ),
        tags$p(
          style = "margin: 0; font-size: 14px; color: #334155; line-height: 1.5;",
          tags$strong("Risk of high bed occupancy threshold crossing:"),
          " indicate the probability of the bed occupancy forecast crossing the threshold over the next 7 days, as well as day aggregates (the first 3 days, the last 4 days, and the full week ahead)."
        )
      )
    ),

    # Master 2-Column Layout with custom class hooked into our styling rules
    layout_columns(
      class = "main-dashboard-grid",
      col_widths = c(7, 5),

      # --- LEFT COLUMN: Bed Occupancy Forecasts (Stacked) ---
      card(
        card_body(
          class = "custom-card-body",
          plotOutput("fc") #,
          # div(
          #   class = "custom-caption",
          #   tags$strong("Figure 1: "),
          #   HTML(
          #     "Acute bed occupancy forecast with 2 weeks bed occupancy history (black line), 1 week ahead forecast (mean, 50%, 80% prediction interval), and bed occupancy threshold (historic 90th percentile solid red line)."
          #   )
          # )
        )
      ),

      # --- RIGHT COLUMN: Risk Predictions (Stacked) ---
      card(
        card_body(
          class = "custom-card-body",
          plotOutput("risk") #,
          # div(
          #   class = "custom-caption",
          #   tags$strong("Figure 2: "),
          #   HTML(
          #     "Risk of bed occupancy forecast crossing the threshold over the next 7-days and at least once for day aggregates (first 3, last 4 days, whole week ahead)."
          #   )
          # )
        )
      )
    )
  ),

  # --- TAB 2: ABOUT / DOCUMENTATION ---
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    div(
      style = "max-width: 800px; margin: 0 auto; padding: 40px 20px;",
      tags$h3(
        "About the dashboard",
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$p(
        "Periods of high bed occupancy in hospitals increase pressure on staff and resources and may impact patient care without appropriate and timely mitigations. This dashboard provides short-term forecasts of bed occupancy and estimates of the risk of an upcoming high-pressure period. Forecasts are generated using an ensemble of statistical and machine learning models trained on routinely collected hospital data. Risks are expressed as the chance that bed occupancy will exceed a hospital-specific threshold over the next week.",
        style = "font-size: 1.1rem; line-height: 1.6;"
      ),
      tags$p(
        "The dashboard is intended to inform short-term operational decisions around capacity, staffing, and service delivery and should be interpreted alongside local knowledge. The dashboard was developed in collaboration with researchers at the Universities of Bath and Bristol. An article describing the methods is under review at The International Journal of Medical Informatics. The submitted manuscript can be viewed [add url once preprint uploaded]",
        style = "font-size: 1.1rem; line-height: 1.6;"
      ),
      tags$hr(),
      tags$h5(
        "Modelling summary",
        style = "font-weight: bold; margin-top: 20px;"
      ),
      tags$p(
        "The outcome measure is the daily number of occupied beds with no distinction between ordinary and escalation beds. Forecasts are based on routinely collected, aggregated hospital-level data, including admissions and discharges, counts of patients with long stays (3+ weeks), and paediatric A&E activity, together with environmental variables such as temperature and day of the week indicators. These variables were selected to reflect key drivers of short-term variation in hospital demand and capacity. The modelling framework allows for inclusion of lagged predictors, reflecting that future bed occupancy may be driven by delayed or cumulative effects of changes in admissions, discharges, and other factors.",
        style = "font-size: 1.1rem; line-height: 1.6;"
      ),
      tags$p(
        "Six base models are selected according to their predictive performance using cross-validation, with accuracy assessed using the continuous ranked probability score (CRPS). The ensemble forecast is then produced by averaging the outputs of the six models, which improves the stability of predictions and captures different aspects of hospital dynamics. The risk of crossing a hospital-specific capacity threshold is then derived from the full predictive distribution, rather than a point forecast, and is reported for both individual days and aggregated periods (e.g. 1-3 days and 4-7 days), allowing assessment across different operational timescales. Currently the threshold is set at the 90th percentile of observed bed occupancy over the period September 2022 to December 2024, but this can be changed to meet operational needs."
      ),
      style = "font-size: 1.1rem; line-height: 1.6;",
      tags$p(
        "Forecasts are updated daily using a rolling training window to adapt to recent trends and potential changes in hospital dynamics. However, the accuracy of the forecasts may be affected by changes in activity, data quality, or factors not explicitly captured in the modelling framework.",
        style = "font-size: 1.1rem; line-height: 1.6;"
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
    fc_wgh <- plot_fc(fc, hist, thr, "WGH")
    (fc_bri/fc_nbt/fc_wgh) + plot_layout(axes = "collect_y")
     # (fc_nbt/fc_bri) + plot_layout(axes = "collect_y")
  })

  # Plot risk - daily + aggregate
  output$risk <- renderPlot({
    risk_bri <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "BRI", "daily + aggregate")
    risk_nbt <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "Southmead", "daily + aggregate")
    risk_wgh <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "WGH", "daily + aggregate")
    (risk_bri/risk_nbt/risk_wgh) + plot_layout(axes = "collect_y")
    # (risk_nbt/risk_bri) + plot_layout(axes = "collect_y") 
  })

  # Plot data time series
  output$data_bri <- renderPlot(plot_ts(hist, "BRI", input$data_ts))
  output$data_southmead <- renderPlot(plot_ts(hist, "Southmead", input$data_ts))
}

shinyApp(ui, server)