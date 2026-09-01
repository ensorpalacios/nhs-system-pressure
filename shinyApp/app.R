library(shiny)
library(data.table)
library(distributional)
library(dplyr)
library(dbplyr)
library(tibble)
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
library(shinyWidgets) # Required for vertical sliders

source("shiny-functions.R")
source("00_prepare_data.R")

# ==========================================
# GLOBAL THEME & CONSTANTS
# ==========================================
theme_set(theme_minimal(base_size = 13) + 
            theme(
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 13, face = "bold"),
              strip.text = element_text(size = 13, face = "bold")
            ))

tooltip_css <- "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:1rem;box-shadow:0 2px 8px rgba(0,0,0,0.15);border:1px solid #e9ecef;"

ui <- page_navbar(
  title = "Acute bed occupancy forecasts",
  fillable = TRUE,
  theme = bs_theme(bg = "#FFFFFF", fg = "#000000"),
  
  tags$head(
    tags$style(HTML("
      /* 1. Force full-height flex column with zero wasted internal space */
      .content-card-body {
          padding: 0 !important; 
          display: flex !important; 
          flex-direction: column !important; 
          justify-content: center !important; 
          overflow: hidden !important; 
          height: 100% !important;
          min-width: 0;
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
          max-height: 100% !important; 
          width: 100% !important;
          height: auto !important;
          object-fit: contain !important; 
      }

      /* 3. Grid layout: Widened slider column to 100px */
      .dashboard-grid {
          display: grid !important;
          grid-template-columns: 1fr 100px 1fr;
          column-gap: 1.5rem;
          height: 100%;
          width: 100%;
          align-items: stretch;
      }

      /* 4. Slider column layouts */
      .slider-column-master {
          position: relative; /* Crucial for absolute title positioning */
          display: flex;
          flex-direction: column;
          height: 100%;
      }
      
      .slider-title {
          position: absolute;
          top: 0; /* Pins title to top without affecting flexbox heights */
          left: 0;
          right: 0;
          text-align: center;
          font-size: 0.75rem;
          font-weight: 700;
          color: #64748b;
          text-transform: uppercase;
          line-height: 1.2;
          z-index: 10;
      }

      .slider-column-inner {
          display: flex !important;
          flex-direction: column !important;
          justify-content: space-evenly !important;
          align-items: center !important;
          height: 100% !important;
          width: 100%;
      }

      .slider-wrapper {
          flex: 0 0 auto;
          display: flex;
          align-items: center;
          justify-content: center;
          width: 100%;
      }

      /* 5. Custom Visuals for Sliders: Red styling to match the threshold line */
      .noUi-handle {
          box-shadow: none !important;
          border: 2px solid #dc2626 !important;
          cursor: pointer;
      }

      /* Tooltip positioned on the right */
      .noUi-vertical .noUi-tooltip {
          left: 115% !important; 
          top: 50% !important;
          transform: translateY(-50%) !important;
          right: auto !important;
          background-color: transparent !important;
          border: none !important;
          color: #dc2626 !important;
          font-weight: 700 !important;
          font-size: 1rem !important;
          box-shadow: none !important;
          padding: 0 !important;
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
      style = "background-color: #F4F6F9; box-shadow: none; flex-shrink: 0; margin-bottom: 0.25rem;",
      card_body(
        fill = FALSE,
        style = "padding: 0.4rem 0.75rem;", 
        tags$p(
          style = "margin: 0; font-size: 13px; color: #334155; line-height: 1.4;",
          tags$strong("Acute bed occupancy:"),
          " features a 2-week history (solid line with points), a 1-week ahead forecast (mean, 50% and 80% prediction intervals), and a high-occupancy threshold (editable below, historic 90th percentile default, solid red line) alongside core bed-stock open (solid blue line)."
        ),
        tags$p(
          style = "margin: 0; font-size: 13px; color: #334155; line-height: 1.4; margin-top: 2px;",
          tags$strong("Risk of high bed occupancy threshold crossing:"),
          " indicates the probability of crossing the threshold over the next 7 days, plus day aggregates (first 3 days, last 4 days, full week ahead)."
        )
      )
    ),
    
    # Master Layout
    card(
      fill = TRUE,
      full_screen = TRUE,
      style = "padding: 0.6rem 0.9rem; margin-bottom: 0;",  
      
      div(
        class = "dashboard-grid",
        
        # Column 1: Forecasts
        div(
          style = "min-width: 0; display: flex; flex-direction: column;",
          class = "content-card-body",
          girafeOutput("fc", width = "100%", height = "100%")
        ),
        
        # Column 2: Sliders
        div(
          class = "slider-column-master",
          
          # Title is now absolutely positioned, escaping the flex layout
          div(class = "slider-title", "Adjust", tags$br(), "Threshold"),
          
          div(
            class = "slider-column-inner",
            
            div(
              class = "slider-wrapper",
              shinyWidgets::noUiSliderInput("thr_bri", label = NULL, min = 620, max = 740, step = 1,
                                            value = thr_default[site == "BRI", thr], orientation = "vertical", direction = "rtl",
                                            tooltips = TRUE, format = wNumbFormat(decimals = 0), color = "#dc2626", height = "12vh")
            ),
            div(
              class = "slider-wrapper",
              shinyWidgets::noUiSliderInput("thr_nbt", label = NULL, min = 900, max = 1060, step = 1,
                                            value = thr_default[site == "Southmead", thr], orientation = "vertical", direction = "rtl",
                                            tooltips = TRUE, format = wNumbFormat(decimals = 0), color = "#dc2626", height = "12vh")
            ),
            div(
              class = "slider-wrapper",
              shinyWidgets::noUiSliderInput("thr_wgh", label = NULL, min = 240, max = 300, step = 1,
                                            value = thr_default[site == "WGH", thr], orientation = "vertical", direction = "rtl",
                                            tooltips = TRUE, format = wNumbFormat(decimals = 0), color = "#dc2626", height = "12vh")
            )
          )
        ),
        
        # Column 3: Risk
        div(
          style = "min-width: 0; display: flex; flex-direction: column;",
          class = "content-card-body",
          girafeOutput("risk", width = "100%", height = "100%")
        )
      )
    )
  ),
  
  # --- TAB 2: ABOUT / DOCUMENTATION ---
  nav_panel(
    title = "About",
    icon = icon("info-circle")
  )
)

server <- function(input, output) {
  model <- "equal"
  
  # Forecast for the selected ensemble model.
  fc <- as.data.table(model_out)[.model == model]
  hist <- as.data.table(historic_data)
  
  # Per-site threshold: reactive to the vertical sliders
  thr <- reactive({
    data.table(
      site = c("BRI", "Southmead", "WGH"),
      thr = c(input$thr_bri, input$thr_nbt, input$thr_wgh)
    )
  })
  
  # Risk computation
  risk <- reactive({
    compute_risk(fc, thr())
  })
  
  # Forecast plot output 
  output$fc <- renderGirafe({
    fc_bri <- plot_fc(fc, hist, core_stock, thr, "BRI")
    fc_nbt <- plot_fc(fc, hist, core_stock, thr, "Southmead")
    fc_wgh <- plot_fc(fc, hist, core_stock, thr, "WGH")
    p <- (fc_bri / fc_nbt / fc_wgh) + plot_layout(axes = "collect_y")
    
    girafe(
      ggobj = p,
      width_svg = 12, height_svg = 10, 
      options = list(
        opts_tooltip(css = tooltip_css),
        opts_hover(css = "fill: #93c5fd; cursor: pointer;"),
        opts_toolbar(hidden = c('lasso_select', 'lasso_deselect', 'zoom_onoff', 'zoom_rect', 'zoom_reset', 'fullscreen')),
        opts_sizing(rescale = TRUE, width = 1) 
      )
    )
  })
  
  # Risk plot output 
  output$risk <- renderGirafe({
    req(risk())
    risk_d <- risk()$risk_d[, .(site, index, risk_day)]
    risk_ws_close <- risk()$risk_ws[week_split == "close", .(site, risk_ws)]
    risk_ws_far <- risk()$risk_ws[week_split == "far", .(site, risk_ws)]
    risk_w <- risk()$risk_w[, .(site, risk_w)]
    
    risk_bri <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "BRI", "daily + aggregate")
    risk_nbt <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "Southmead", "daily + aggregate")
    risk_wgh <- plot_riskd(risk_d, risk_ws_close, risk_ws_far, risk_w, "WGH", "daily + aggregate")
    
    p <- (risk_bri / risk_nbt / risk_wgh) + plot_layout(axes = "collect_y")
    
    girafe(
      ggobj = p,
      width_svg = 12, height_svg = 10,
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