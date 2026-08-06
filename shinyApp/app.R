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
# CONSTANTS
# ==========================================
tooltip_css <- "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:1rem;box-shadow:0 2px 8px rgba(0,0,0,0.15);border:1px solid #e9ecef;"

ui <- page_navbar(
  title = "Acute bed occupancy forecasts",
  fillable = TRUE,
  theme = bs_theme(bg = "#FFFFFF", fg = "#000000"),
  
  tags$head(
    tags$style(HTML("
      /* 1. Force the card body to be a full-height flex column with no internal scrolling */
      .content-card-body {
          padding: 1rem !important; 
          display: flex !important; 
          flex-direction: column !important; 
          justify-content: center !important; 
          overflow: hidden !important; 
          height: 100% !important;
      }

      /* 2. CRITICAL GGIRAPH GRAPHIC SCALE OVERRIDES */
      .html-widget.girafe > div {
          padding-top: 0 !important;
          height: 100% !important;
          width: 100% !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
      }

      .html-widget.girafe svg {
          max-width: 100% !important;
          max-height: 100% !important; /* Scale safely into remaining flexbox space */
          width: 100% !important;
          height: auto !important;
          object-fit: contain !important; /* Protects layout aspect ratio safely */
      }
    "))
  ),
  
  # --- TAB 1: FORECASTS & RISK ---
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),
    
    # Header Info Card
    card(
      fill = FALSE,
      style = "background-color: #F4F6F9; box-shadow: none; flex-shrink: 0; margin-bottom: 0.5rem;",
      card_body(
        fill = FALSE,
        style = "padding: 0.75rem 1rem;", 
        tags$p(
          style = "margin: 0; font-size: 14px; color: #334155; line-height: 1.5;",
          tags$strong("Acute bed occupancy:"),
          " features a 2-week history (solid line with points), a 1-week ahead forecast (mean, 50% and 80% prediction intervals shown via dashed line and ribbons), and a high-occupancy threshold (historic 90th percentile, solid red line) and the current core bed-stock open (solid blue line)."
        ),
        tags$p(
          style = "margin: 0; font-size: 14px; color: #334155; line-height: 1.5; margin-top: 5px;",
          tags$strong("Risk of high bed occupancy threshold crossing:"),
          " indicate the probability of the bed occupancy forecast crossing the threshold over the next 7 days, as well as day aggregates (the first 3 days, the last 4 days, and the full week ahead)."
        )
      )
    ),
    
    # Main grid wrapping the charts
    layout_columns(
      col_widths = c(7, 5),
      
      # CARD 1: FORECASTS
      card(
        full_screen = TRUE,
        card_body(
          class = "d-flex flex-column align-items-stretch content-card-body",
          style = "overflow: hidden !important; padding: 0.5rem; min-height: 0 !important;",
          div(
            # Using flex 1 1 auto to let it consume space, but overflow hidden prevents breaking the card bounds
            style = "flex: 1 1 auto; width: 100%; height: 100%; overflow: hidden; display: flex;",
            girafeOutput("fc", width = "100%", height = "100%")
          )
        )
      ),
      
      # CARD 2: RISK
      card(
        full_screen = TRUE,
        card_body(
          class = "d-flex flex-column align-items-stretch content-card-body",
          style = "overflow: hidden !important; padding: 0.5rem; min-height: 0 !important;",
          div(
            style = "flex: 1 1 auto; width: 100%; height: 100%; overflow: hidden; display: flex;",
            girafeOutput("risk", width = "100%", height = "100%")
          )
        )
      )
    )
  ),
  
  # --- TAB 2: ABOUT / DOCUMENTATION ---
  nav_panel(
    title = "About",
    icon = icon("info-circle")
    # Content left as original ...
  )
)

server <- function(input, output) {
  model <- "equal"
  
  # Get data (using your existing variables)
  risk_d <- as.data.table(model_out)[type == "risk_d" & .model == model, .(site, index, risk_day)]
  risk_ws_close <- as.data.table(model_out)[type == "risk_ws" & .model == model & week_split == "close", .(site, risk_ws)]
  risk_ws_far <- as.data.table(model_out)[type == "risk_ws" & .model == model & week_split == "far", .(site, risk_ws)]
  risk_w <- as.data.table(model_out)[type == "risk_w" & .model == model, .(site, risk_w)]
  fc <- as.data.table(model_out)[type == "forecast" & .model == model]
  thr <- as.data.table(model_out)[type == "risk_w" & .model == model, .(site, thr)]
  hist <- as.data.table(historic_data)
  
  # Plot bed occupancy fc
  output$fc <- renderGirafe({
    fc_bri <- plot_fc(fc, hist, thr, "BRI")
    fc_nbt <- plot_fc(fc, hist, thr, "Southmead")
    fc_wgh <- plot_fc(fc, hist, thr, "WGH")
    p <- (fc_bri / fc_nbt / fc_wgh) + plot_layout(axes = "collect_y")
    
    girafe(
      ggobj = p,
      # You can tweak these SVG dimensions to change the internal rendering aspect ratio. 
      # The CSS object-fit: contain will handle fitting it to the screen cleanly.
      width_svg = 12,
      height_svg = 9, 
      options = list(
        opts_tooltip(css = tooltip_css),
        opts_hover(css = "fill: #93c5fd; cursor: pointer;"),
        opts_toolbar(hidden = c('lasso_select', 'lasso_deselect', 'zoom_onoff', 'zoom_rect', 'zoom_reset', 'fullscreen')),
        opts_sizing(rescale = TRUE, width = 1) 
      )
    )
  })
  
  # Plot risk - daily + aggregate
  output$risk <- renderGirafe({
    risk_bri <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "BRI", "daily + aggregate")
    risk_nbt <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "Southmead", "daily + aggregate")
    risk_wgh <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "WGH", "daily + aggregate")
    p <- (risk_bri / risk_nbt / risk_wgh) + plot_layout(axes = "collect_y")
    
    girafe(
      ggobj = p,
      width_svg = 8,
      height_svg = 9,
      options = list(
        opts_tooltip(css = tooltip_css),
        opts_hover(css = "fill: #93c5fd; cursor: pointer;"),
        opts_toolbar(hidden = c('lasso_select', 'lasso_deselect', 'zoom_onoff', 'zoom_rect', 'zoom_reset', 'fullscreen')),
        opts_sizing(rescale = TRUE, width = 1) 
      )
    )
  })
}

shinyApp(ui, server)