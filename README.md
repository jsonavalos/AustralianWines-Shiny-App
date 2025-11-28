# Australian Wines Forecasting — Shiny App

This project provides an interactive **Shiny application** for exploring, modeling, and forecasting monthly Australian wine sales by varietal.  
It leverages the [`fpp3`](https://fpp3-package.netlify.app/) ecosystem for time series analysis and integrates multiple forecasting models (ETS, ARIMA, TSLM) with user-friendly visualizations and accuracy metrics.

---

## ✨ Features

- **Data exploration**
  - Interactive filtering by wine varietals and date ranges
  - Overview plots of monthly wine sales

- **Modeling workflow**
  - Flexible training/validation split via user-selected cutoff date
  - Forecast horizon control (1–60 months)
  - Choice of models: ETS, ARIMA, TSLM

- **Model insights**
  - Model specification tables (ETS components, ARIMA orders, TSLM formula)
  - Training and validation accuracy metrics (RMSE, MAE, MAPE)
  - Best model selection per varietal (lowest RMSE)

- **Forecast visualization**
  - Forecast plots with confidence intervals
  - Overlay of actual sales for comparison

---

## 📂 Project Structure
├── app.R # Main Shiny app 
├── EDA/ │ 
└── AustralianWines.csv # Wine sales dataset 
└── README.md # Project documentation


---

## ⚙️ Requirements

### R Packages
The app depends on the following R packages:

- shiny, bslib (UI framework)
- tidyverse, lubridate (data wrangling)
- tsibble, fpp3 (time series structures)
- fable, fabletools (forecasting models)
- ggplot2 (visualization)
- gt (tables)
- urca (unit root tests)
- here (file paths)

Install them with:

```r
install.packages(c(
  "shiny", "bslib", "tidyverse", "lubridate",
  "tsibble", "fpp3", "fable", "fabletools",
  "ggplot2", "gt", "urca", "here"
))
```

## 📊 Data

The dataset **AustralianWines.csv** contains monthly wine sales (in thousands of liters) by varietal.  
Columns include:

- `Month` (date)
- `Dry white`, `Red`, `Sweet white`, `Rose`, etc.

Missing values in `Rose` are forward-filled for continuity.

---

## 🧮 Models

- **TSLM**: Linear regression with trend and seasonality  
- **ETS**: Exponential smoothing state space models  
- **ARIMA**: Autoregressive integrated moving average (with drift detection)  

Each model is fit per varietal, with forecasts generated for the user-specified horizon.

---

## 📈 Outputs

- **Overview**: Time series plots by varietal  
- **Model Specifications**: Tabular summary of fitted models  
- **Performance Metrics**: Training and validation accuracy tables  
- **Forecasts**: Forecast plots with confidence intervals  
- **Model Comparison**: Best model per varietal (lowest RMSE)  

---

## 🚀 Future Improvements

- Add more model families (e.g., Prophet, Neural Networks)  
- Enable export of forecast tables/plots  
- Enhance UI with interactive plotly charts  
- Automate dataset updates  

---

## 👤 Author

Developed by Jason — focused on reproducible workflows, transparent modeling, and empowering users with clear forecasting tools.
