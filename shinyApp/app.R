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
#source(here("shinyApp/shiny-functions.R"))
source("shiny-functions.R")


source("00_prepare_data.R")
# source("shinyApp/00_prepare_data.R")

ui <- page_sidebar(
  # Sidebar panel for inputs
  sidebar = sidebar(
    # Main navigation
    radioButtons(
      "main_nav",
      "View",
      choices = list("Risk" = "risk", "Data" = "data"),
      selected = "risk"
    )
  ),

  # Theme
  title = "",
  theme = bs_theme(
    bg = "#f8f8f8", # Background color
    fg = "#000000" # Foreground (text) color
  ),

  conditionalPanel(
    condition = "input.main_nav == 'risk'",
   layout_columns(
      col_widths = c(8, 4),
      titlePanel("7 days ahead risk predictions"),
      div(
        style = "margin-top: 20px;",
        selectInput(
          "select_risk",
          "Risk type:",
          choices = list(
            "Daily risk" = "daily risk",
            "Daily & aggregate risk" = "daily + aggregate"
          ),
          selected = "daily risk",
          width = "100%"
        )
      )
    ), 

    layout_columns(
      col_widths = c(7, 5),
      div(
        strong("Bed occupancy forecast"),
        tooltip(
          bs_icon("info-circle"),
          "2 weeks bed occupancy history (black line), 1 week ahead forecast 
        (mean, 50%, 80% prediction interval), and bed occupancy threshold 
        (dotted line)"
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
      )
    ),
    # BRI card
    card(
      card_header(strong("BRI")),
      card_body(
        layout_columns(
          col_widths = c(7, 5),
          div(
            style = "height: 300px;",
            plotOutput("fc_bri", height = "300px")
          ),
          div(
            style = "height: 300px;",
            conditionalPanel(
              condition = "input.select_risk == 'daily risk'",
              plotOutput("bri_risk_d", height = "300px")
            ),
            conditionalPanel(
              condition = "input.select_risk == 'daily + aggregate'",
              plotOutput("bri_risk_a", height = "300px")
            )
          )
        )
      )
    ),

    # Southmead card
    card(
      card_header(strong("Southmead")),
      card_body(
        layout_columns(
          col_widths = c(7, 5),
          div(
            style = "height: 300px;",
            plotOutput("fc_southmead", height = "300px")
          ),
          div(
            style = "height: 300px;",
            conditionalPanel(
              condition = "input.select_risk == 'daily risk'",
              plotOutput("southmead_risk_d", height = "300px")
            ),
            conditionalPanel(
              condition = "input.select_risk == 'daily + aggregate'",
              plotOutput("southmead_risk_a", height = "300px")
            )
          )
        )
      )
    )
  ),

  # Data Page
  conditionalPanel(
    condition = "input.main_nav == 'data'",
   layout_columns(
      col_widths = c(8, 4),
      titlePanel("Data View"),
      div(
        style = "margin-top: 20px;",
        selectInput(
          "data_ts",
          "Variable:",
          choices = list(
            "Bed occupancy" = "occ",
            "Admission and discharges" = "ad_diff",
            "Paediatric A&E" = "paed",
            "Length of stay" = "los",
            "Temperature" = "temp"
          ),
          selected = "occ",
          width = "100%"
        )
      )
    ), 

    card(
      card_header(strong("BRI")),
      card_body(
          plotOutput("data_bri", height = "300px")
      ),
      card_header(strong("Southmead")),
      card_body(
          plotOutput("data_southmead", height = "300px")
      )
    )
  )
)

server <- function(input, output) {
  # Choose model
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

  # Plot risk - daily
  output$bri_risk_d <- renderPlot({
    plot_riskd(
      risk_d, 
      risk_ws_close, 
      risk_ws_far, 
      risk_w, 
      "BRI", 
      "daily risk"
    )
  })
  output$southmead_risk_d <- renderPlot({
    plot_riskd(
      risk_d,
      risk_ws_close,
      risk_ws_far,
      risk_w,
      "Southmead",
      "daily risk"
    )
  })

  # plot risk - daily + aggregate
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

  # Plot bed occupancy fc
  output$fc_bri <- renderPlot(
    plot_fc(fc, hist, thr, "BRI")
  )

  output$fc_southmead <- renderPlot(
    plot_fc(fc, hist, thr, "Southmead")
  )

  # Plot data time series
  output$data_bri <- renderPlot(
    plot_ts(hist, "BRI", input$data_ts)
  )
  output$data_southmead <- renderPlot(
    plot_ts(hist, "Southmead", input$data_ts)
  )
}


shinyApp(ui, server)