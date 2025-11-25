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
library(urca)

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
      selected = c("ETS")
    )
  ),
  navset_card_tab(
    nav_panel("Overview",
             card(
               card_header("Wine Sales Overview"),
               plotOutput("overview_plot"),
               card_footer(uiOutput("overview_note"))
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
               gt_output("best_models_gt")
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

  # ...existing code...



# 2) User-facing model overview table: Varietal × (TSLM / ETS(component) / ARIMA(order))
output$model_overview <- renderTable({
  req(models())
  tryCatch({
    m <- models()

    key_cols <- intersect(c("Varietal"), names(m))
    model_cols <- setdiff(names(m), key_cols)
    if (length(model_cols) == 0) return(data.frame(Message = "No fitted model columns found"))

    long <- m |>
      pivot_longer(cols = all_of(model_cols), names_to = ".model", values_to = "fit")

    safe_report_txt <- function(fit_obj) {
      tryCatch(paste(capture.output(report(fit_obj)), collapse = "\n"), error = function(e) NA_character_)
    }

    long2 <- long |>
      mutate(
        report_txt = map_chr(fit, ~ safe_report_txt(.x)),
        ETS_text   = if_else(.model == "ETS", str_extract(report_txt, "ETS\\([^\\)]+\\)"), NA_character_),
        ARIMA_text = if_else(.model == "ARIMA", str_extract(report_txt, "ARIMA\\([^\\)]+\\)"), NA_character_),
        TSLM_text  = if_else(.model == "TSLM", "<TSLM>", NA_character_),
        ARIMA_text = if_else(.model == "ARIMA" & str_detect(report_txt, regex("drift", ignore_case = TRUE)),
                             paste0(ARIMA_text, " w/ drift"), ARIMA_text)
      )

    wide <- long2 |>
      pivot_wider(
        id_cols = Varietal,
        names_from = .model,
        values_from = c(TSLM_text, ETS_text, ARIMA_text),
        values_fn = list(~ coalesce(.x[1], NA_character_))
      )

    display <- wide |>
      transmute(
        Varietal,
        TSLM  = coalesce(TSLM_text_TSLM, "<TSLM>"),
        ETS   = coalesce(ETS_text_ETS, NA_character_),
        ARIMA = coalesce(ARIMA_text_ARIMA, NA_character_)
      )

    as.data.frame(display)
  }, error = function(e) {
    data.frame(Message = "Model specifications not available")
  })
})
# ...existing server code...

models <- reactive({
  req(training_long(), input$models_to_fit)
  validate(
    need(nrow(training_long()) > 24, "Need at least 24 observations for modeling"),
    need(length(input$models_to_fit) > 0, "Please select at least one model")
  )

  model_list <- list()
  if ("TSLM" %in% input$models_to_fit) model_list[["TSLM"]] <- TSLM(Sales ~ trend() + season())
  if ("ETS"  %in% input$models_to_fit) model_list[["ETS"]]  <- ETS(Sales)
  if ("ARIMA" %in% input$models_to_fit) model_list[["ARIMA"]] <- ARIMA(Sales)

  withProgress(message = "Fitting models", value = 0, {
    n_steps <- max(2, length(model_list) + 1)
    incProgress(1 / n_steps, detail = "Preparing data...")
    # Heavy work performed once (fable will fit per key internally)
    res <- training_long() |> model(!!!model_list)
    incProgress((n_steps - 1) / n_steps, detail = "Finalizing models...")
    res
  })
})

fc_h <- reactive({
  req(models(), input$h)
  withProgress(message = "Generating forecasts", value = 0, {
    incProgress(0.2, detail = "Preparing models...")
    res <- models() |> forecast(h = input$h)
    incProgress(0.8, detail = "Assembling results...")
    res
  })
})

fc_val <- reactive({
  req(models(), validation_long())
  if (is.null(validation_long())) return(NULL)

  withProgress(message = "Generating validation forecasts", value = 0, {
    incProgress(0.2, detail = "Preparing validation data...")
    if (nrow(validation_long()) > 0) {
      res <- models() |> forecast(new_data = validation_long())
      incProgress(0.8, detail = "Done")
      res
    } else {
      incProgress(1, detail = "No validation rows")
      NULL
    }
  })
})
# ...existing code...

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


output$model_specs <- renderTable({
  req(models())
  
  df <- models() |> as_tibble()
  
  # Conditionally mutate only if the column exists
  if ("ARIMA" %in% names(df)) {
    df <- df |> mutate(ARIMA = format(ARIMA))
  }
  if ("ETS" %in% names(df)) {
    df <- df |> mutate(ETS = format(ETS))
  }
  if ("TSLM" %in% names(df)) {
    df <- df |> mutate(TSLM = "Sales ~ trend() + season()")
  }
  
  # Select only the columns that exist
  df |> select(any_of(c("Varietal", "ARIMA", "ETS", "TSLM")))
}, striped = TRUE)

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
# Extract forecast months
fc_months <- fc_h() %>%
  as_tibble() %>%
  pull(Month) %>%
  unique()
  # Forecasts (with intervals)
  p <- autoplot(fc_h(), level = 95) +
    # Add actual data
    geom_line(
      data = wines_long |> filter(Month %in% fc_months),
      aes(x = Month, y = Sales, colour = "Actual"),
      inherit.aes = FALSE
    ) +
    facet_wrap(~ Varietal, scales = "free_y") +
     scale_fill_discrete(name = "Model and Confidence Interval") +  # Rename ribbon legend
    #scale_colour_discrete(name = "Model") +
    guides(colour = "none") +
    theme_minimal()

  p
})

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

}

shinyApp(ui, server)