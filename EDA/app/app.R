# app.R — Shiny app for Australian Wines forecasting

library(shiny)
library(bslib)
library(fpp3)
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(ggplot2)
library(gt)
library(lubridate)

# Load and clean data
AustralianWines <- read_csv(
  here::here("EDA/AustralianWines.csv"),
  na = "*",
  col_types = cols(Rose = col_number()),
  show_col_types = FALSE
) |> 
  fill(Rose, .direction = "down") |> 
  mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth())


# Pivot to long format and convert to tsibble
wines_long <- AustralianWines |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month, key = Varietal)

 

# Helpers for date inputs
min_month <- min(wines_long$Month)
max_month <- max(wines_long$Month)
min_date  <- as.Date(min_month)
max_date  <- as.Date(max_month)

ui <- page_sidebar(
  title = "Australian Wines Forecasting",
  sidebar = sidebar(
    # Varietal selection
    selectInput(
      "varietals",
      "Select varietals:",
      choices = sort(unique(wines_long$Varietal)),
      selected = sort(unique(wines_long$Varietal)),
      multiple = TRUE
    ),
    # Date range selection
    dateRangeInput(
      "daterange",
      "Date range:",
      start = min_date,
      end   = max_date,
      min   = min_date,
      max   = max_date
    ),
    # Training cutoff
    dateInput(
      "train_end",
      "Training end date:",
      value = as.Date("1992-12-31"),
      min = min_date,
      max = max_date
    ),
    # Forecast horizon
    numericInput(
      "h",
      "Forecast horizon (months):",
      value = 12, min = 1, max = 60, step = 1
    ),
    # Model selection (improvement for user experience)
    checkboxGroupInput(
      "models_to_fit",
      "Models to fit:",
      choices = c("TSLM" = "TSLM", "ETS" = "ETS", "ARIMA" = "ARIMA"),
      selected = c("TSLM", "ETS", "ARIMA")
    ),
    # Download forecasts
    downloadButton("download_fc", "Download forecasts (CSV)", class = "btn-primary")
  ),
  navset_card_tab(
    nav_panel("Overview",
             card(
               card_header("Wine Sales Overview"),
               plotOutput("overview_plot"),
               card_footer(uiOutput("overview_note"))
             )),
    nav_panel("Decomposition", 
             card(
               card_header("STL Decomposition"),
               plotOutput("decomp_plot")
             )),
    nav_panel("Model Specifications", 
             card(
               card_header("Model Details"),
               tableOutput("model_specs")
             )),
    nav_panel("Performance Metrics",
             card(
               card_header("Model Accuracy"),
               h4("Training accuracy"),
               gt_output("train_gt"),
               br(),
               h4("Validation accuracy"),
               gt_output("val_gt")
             )),
    nav_panel("Forecasts", 
             card(
               card_header("Forecast Visualizations"),
               plotOutput("forecast_plot")
             )),
    nav_panel("Model Comparison",
             card(
               card_header("Best Model Selection"),
               gt_output("best_models_gt"),
               br(),
               plotOutput("residuals_plot")
             ))
  )
)

server <- function(input, output, session) {

  
  filtered <- reactive({
    req(input$varietals, input$daterange)
    
    # Convert to proper yearmonth boundaries
start_yM <- yearmonth(input$daterange[1])
end_yM   <- yearmonth(input$daterange[2])

    wines_long |>
      filter(Varietal %in% input$varietals) |>
      filter(Month >= start_yM, Month <= end_yM)  

  })

  training_long <- reactive({
    req(filtered(), input$train_end)
    cutoff <- yearmonth(format(input$train_end, "%Y-%m"))
    filtered() |> filter(Month <= cutoff)
  })

  validation_long <- reactive({
    req(filtered(), input$train_end)
    cutoff <- yearmonth(format(input$train_end, "%Y-%m"))
    result <- filtered() |> filter(Month > cutoff)
    if(nrow(result) == 0) NULL else result
  })

  # Models per varietal with user selection
  models <- reactive({
    req(training_long(), input$models_to_fit)
    validate(
      need(nrow(training_long()) > 24, "Need at least 24 observations for modeling"),
      need(length(input$models_to_fit) > 0, "Please select at least one model")
    )
    
    model_list <- list()
    if("TSLM" %in% input$models_to_fit) {
      model_list[["TSLM"]] <- TSLM(Sales ~ trend() + season())
    }
    if("ETS" %in% input$models_to_fit) {
      model_list[["ETS"]] <- ETS(Sales)
    }
    if("ARIMA" %in% input$models_to_fit) {
      model_list[["ARIMA"]] <- ARIMA(Sales)
    }
    
    training_long() |> model(!!!model_list)
  })

  # Forecasts
  fc_h <- reactive({
    req(models(), input$h)
    models() |> forecast(h = input$h)
  })

  fc_val <- reactive({
    req(models(), validation_long())
    if(!is.null(validation_long()) && nrow(validation_long()) > 0) {
      models() |> forecast(new_data = validation_long())
    } else {
      NULL
    }
  })

  # Overview plot
  output$overview_plot <- renderPlot({
    req(filtered())
    autoplot(filtered(), Sales) +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(title = "Wine Sales by Varietal",
           x = "Month", y = "Sales (thousands of liters)") +
      theme_minimal() +
      theme(strip.text = element_text(size = 10))
  })

  output$overview_note <- renderUI({
    req(filtered())
    train_end <- yearmonth(format(input$train_end, "%Y-%m"))
    HTML(paste0(
      "<p><strong>Selected varietals:</strong> ",
      paste(sort(unique(filtered()$Varietal)), collapse = ", "),
      "<br><strong>Date range:</strong> ",
      format(as.Date(min(filtered()$Month)), "%Y-%m"),
      " to ",
      format(as.Date(max(filtered()$Month)), "%Y-%m"),
      "<br><strong>Training period ends:</strong> ",
      format(as.Date(train_end), "%Y-%m"),
      "</p>"
    ))
  })

  # STL decomposition
  output$decomp_plot <- renderPlot({
    req(filtered())
      filtered() |>
        model(STL = STL(Sales ~ season(window = "periodic"))) |>
        components() |>
        autoplot() +
        labs(title = "STL Decomposition") +
        theme_minimal() +
        facet_wrap(~ Varietal, scales = "free_y")
  })

  # Model specifications
  output$model_specs <- renderTable({
    req(models())
    tryCatch({
      specs <- glance(models()) |>
        select(any_of(c("Varietal", ".model", "method", "order", "seasonal_order", "sigma2", "AIC", "BIC")))
      
      # Clean up the display
      if("method" %in% names(specs)) {
        specs <- specs |> mutate(method = as.character(method))
      }
      if("order" %in% names(specs)) {
        specs <- specs |> mutate(order = as.character(order))
      }
      if("seasonal_order" %in% names(specs)) {
        specs <- specs |> mutate(seasonal_order = as.character(seasonal_order))
      }
      
      specs
    }, error = function(e) {
      data.frame(Message = "Model specifications not available")
    })
  })

  # Training accuracy
  output$train_gt <- render_gt({
    req(models())
    tryCatch({
      acc <- accuracy(models()) |>
        select(any_of(c("Varietal", ".model", "RMSE", "MAE", "MAPE")))

      gt(acc) |>
        tab_header(title = md("**Training Accuracy**")) |>
        fmt_number(columns = any_of(c("RMSE", "MAE", "MAPE")), decimals = 2) |>
        tab_style(
          style = cell_fill(color = "lightblue"),
          locations = cells_column_labels()
        )
    }, error = function(e) {
      gt(data.frame(Message = "Training accuracy not available")) |>
        tab_header(title = md("**Training Accuracy**"))
    })
  })

  # Validation accuracy
  output$val_gt <- render_gt({
    tryCatch({
      if(!is.null(fc_val()) && !is.null(validation_long())) {
        acc <- accuracy(fc_val(), validation_long()) |>
select(any_of(c("Varietal", ".model", "RMSE", "MAE", "MAPE")))

        gt(acc) |>
          tab_header(title = md("**Validation Accuracy**")) |>
          fmt_number(columns = any_of(c("RMSE", "MAE", "MAPE")), decimals = 2) |>
          tab_style(
            style = cell_fill(color = "mistyrose"),
            locations = cells_column_labels()
          )
      } else {
        gt(data.frame(Message = "No validation data available")) |>
          tab_header(title = md("**Validation Accuracy**"))
      }
    }, error = function(e) {
      gt(data.frame(Message = "Validation accuracy not available")) |>
        tab_header(title = md("**Validation Accuracy**"))
    })
  })

output$forecast_plot <- renderPlot({
  req(fc_h())
  autoplot(fc_h(), level = 95) +
    facet_wrap(~ Varietal, scales = "free_y") +
    labs(title = "Forecasts (future months only)",
         x = "Month", y = "Sales (thousands of liters)") +
    theme_minimal()
})

  # Download forecasts
  output$download_fc <- downloadHandler(
    filename = function() {
      paste0("forecasts_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(fc_h())
      fc_tbl <- fc_h() |>
        hilo(level = c(80, 95)) |>
        as_tibble() |>
        mutate(
          `80%_lower` = map_dbl(`80%`, ~ .x$lower),
          `80%_upper` = map_dbl(`80%`, ~ .x$upper),
          `95%_lower` = map_dbl(`95%`, ~ .x$lower),
          `95%_upper` = map_dbl(`95%`, ~ .x$upper)
        ) |>
        select(Month, Varietal, .model, Sales = .mean,
               `80%_lower`, `80%_upper`, `95%_lower`, `95%_upper`)
      write_csv(fc_tbl, file)
    }
  )

  # Best model selection (lowest RMSE)
  output$best_models_gt <- render_gt({
    req(models())
    tryCatch({
      acc <- accuracy(models()) |>
        group_by(Varietal) |>
        slice_min(RMSE, n = 1) |>
        ungroup() |>
        select(Varietal, .model, RMSE, MAE, MAPE)

      gt(acc) |>
        tab_header(title = md("**Best Model per Varietal (Lowest RMSE)**")) |>
        fmt_number(columns = c("RMSE", "MAE", "MAPE"), decimals = 2)
    }, error = function(e) {
      gt(data.frame(Message = "Best model summary not available")) |>
        tab_header(title = md("**Best Model per Varietal**"))
    })
  })

  # Residual plot (bonus visualization)
  output$residuals_plot <- renderPlot({
    req(models())
    tryCatch({
      residuals <- augment(models()) |>
        filter(.model %in% input$models_to_fit)

      ggplot(residuals, aes(x = Month, y = .resid)) +
        geom_line() +
        facet_wrap(~ Varietal + .model, scales = "free_y") +
        labs(title = "Model Residuals Over Time",
             x = "Month", y = "Residuals") +
        theme_minimal()
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Residual plot not available") +
        theme_void()
    })
  })
}

shinyApp(ui, server)