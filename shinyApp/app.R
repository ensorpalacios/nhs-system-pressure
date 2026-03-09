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

source(here("shinyApp/shiny-functions.R"))

# source("00_prepare_data.R")
model_out <- readRDS(here("target/data/output/2025-12-16.RDS"))

ui <- page_fillable(
  # Theme
  title = "",
  theme = bs_theme(
    bg = "#f8f8f8", # Background color
    fg = "#000000" # Foreground (text) color
  ),

  # theme = bs_theme(bootswatch = "flatly"),

  titlePanel("Risk predictions"),

  p(
    "Displaying 2 weeks of actual data + 7 day forecast with confidence intervals"
  ),

  # Group 1
  layout_columns(
    col_widths = c(8, 4),
    # Time series
    card(
      card_header(strong("Occupancy time series")),
      card_body(
        h5("BRI"),
        plotOutput("fc_bri", height = "300px"),
        hr(),
        h5("Southmead"),
        plotOutput("fc_southmead", height = "300px"),
        hr()
      )
    ),

    # Risk predictions
    # BRI
    card(
      card_header(strong("Risk")),
      card_body(
        h5("BRI"),
        selectInput(
          "risk_metric_bri",
          "Select Risk Metric:",
          choices = c(
            "Daily Risk" = "risk_d",
            "Weekly Risk (Close)" = "risk_ws_close",
            "Weekly Risk (Far)" = "risk_ws_far",
            "Weekly Risk" = "risk_w"
          ),
          selected = "risk_d"
        ),
        # div(
        #   style = "text-align: center; padding: 20px;",
        #   h2(textOutput("risk_value_2"), style = "color: #e74c3c; margin: 0;"),
        #   p("Risk Score (%)", style = "color: #7f8c8d; margin-top: 5px;")
        # )
        uiOutput("bri_risk")
      ),
      hr(),
      h5("Southmead"),
      selectInput(
        "risk_metric_southmead",
        "risk window:",
        choices = c(
          "Daily Risk" = "risk_d",
          "Weekly Risk (Close)" = "risk_ws_close",
          "Weekly Risk (Far)" = "risk_ws_far",
          "Weekly Risk" = "risk_w"
        ),
        selected = "daily"
      ),
      uiOutput("southmead_risk")
      # div(
      #   style = "text-align: center; padding: 20px;",
      #   h2(textOutput("risk_value_2"), style = "color: #e74c3c; margin: 0;"),
      #   p("Risk Score (%)", style = "color: #7f8c8d; margin-top: 5px;")
      # )
    )
  )
)

server <- function(input, output) {
  # Choose model
  model <- "crps"

  # Check changes in data directory
  risk_d <- model_out$risk$risk_d[.model == model]
  risk_ws_close <- model_out$risk$risk_ws[
    .model == model & week_split == "close"
  ]
  risk_ws_far <- model_out$risk$risk_ws[.model == model & week_split == "far"]
  risk_w <- model_out$risk$risk_w[.model == model]
  fc <- model_out$fc |> filter(.model == model)
  thr <- model_out$threshold

  # Render plots
  # Plot risk
  output$bri_risk <- renderUI({
    switch(
      input$risk_metric_bri,
      risk_d = plotOutput("plot_bri_risk", height = "200px"),
      risk_ws_close = div(
        style = "text-align: center; padding: 20px;",
        h2(textOutput("text_bri_risk"), style = "color: #e74c3c;")
      ),
      risk_ws_far = div(
        style = "text-align: center; padding: 20px;",
        h2(textOutput("text_bri_risk"), style = "color: #e74c3c;")
      ),
      risk_w = div(
        style = "text-align: center; padding: 20px;",
        h2(textOutput("text_bri_risk"), style = "color: #e74c3c;")
      )
    )
  })

  # Only render plot when it's actually being displayed
  output$plot_bri_risk <- renderPlot({
    req(input$risk_metric_bri == "risk_d") # Only run if risk_d is selected
    plot_riskd(risk_d, "BRI")
  })

  # Only render text when it's being displayed
  output$text_bri_risk <- renderText({
    req(input$risk_metric_bri %in% c("risk_ws_close", "risk_ws_far", "risk_w"))
    switch(
      input$risk_metric_bri,
      risk_ws_close = sprintf(
        "%.2f%%",
        risk_ws_close[site == "BRI", risk_ws]
      ),
      risk_ws_far = sprintf(
        "%.2f%%",
        risk_ws_far[site == "BRI", risk_ws]
      ),
      risk_w = sprintf("%.2f%%", risk_w[site == "BRI", risk_w])
    )
  })

  # Southmead
  output$southmead_risk <- renderUI({
    switch(
      input$risk_metric_southmead,
      risk_d = plotOutput("plot_southmead_risk", height = "200px"),
      risk_ws_close = div(
        style = "text-align: center; padding: 20px;",
        h2(textOutput("text_southmead_risk"), style = "color: #e74c3c;")
      ),
      risk_ws_far = div(
        style = "text-align: center; padding: 20px;",
        h2(textOutput("text_southmead_risk"), style = "color: #e74c3c;")
      ),
      risk_w = div(
        style = "text-align: center; padding: 20px;",
        h2(textOutput("text_southmead_risk"), style = "color: #e74c3c;")
      )
    )
  })

  # Only render plot when it's actually being displayed
  output$plot_southmead_risk <- renderPlot({
    req(input$risk_metric_southmead == "risk_d") # Only run if risk_d is selected
    plot_riskd(risk_d, "Southmead")
  })

  # Only render text when it's actually being displayed
  output$text_southmead_risk <- renderText({
    req(input$risk_metric_southmead %in% c("risk_ws_close", "risk_ws_far", "risk_w"))
    switch(
      input$risk_metric_southmead,
      risk_ws_close = sprintf(
        "%.2f%%",
        risk_ws_close[site == "Southmead", risk_ws]
      ),
      risk_ws_far = sprintf(
        "%.2f%%",
        risk_ws_far[site == "Southmead", risk_ws]
      ),
      risk_w = sprintf("%.2f%%", risk_w[site == "Southmead", risk_w])
    )
  })

  # Plot bed occupancy
  output$fc_bri <- renderPlot(
    plot_fc(fc, thr, "BRI")
  )

  output$fc_southmead <- renderPlot(
    plot_fc(fc, thr, "Southmead")
  )
}


shinyApp(ui, server)