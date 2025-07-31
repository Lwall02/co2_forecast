# 🌍 CO₂ Emissions Forecasting with Time Series Regression

This project applies time series modeling to forecast global CO₂ emissions using annual population data from 1950 to 2023. The dataset comes from [Our World in Data](https://ourworldindata.org/co2-and-greenhouse-gas-emissions), which compiles CO₂ emissions (in metric tonnes, excluding land-use change) and global population estimates from various sources.

#### 📊 Model Description

The forecasting framework employs a **linear regression model with ARIMA errors**, specifically:

![equation](https://latex.codecogs.com/svg.image?%5Clog(%5Ctext%7BCO2%7D_t)=%5Cbeta_0+%5Cbeta_1%5Ccdot%5Ctext%7BPopulation%7Dt+%5Cbeta_2%5Ccdot%5Cmathbb%7BI%7D_%7B%5Ctext%7BCOVID%20years%7D%7D(t)+x_t,%5Cquad%20x_t%5Csim%5Ctext%7BARIMA%7D(p,d,q))

-   **Regression component:** Models log-transformed CO₂ emissions as a function of annual population and a COVID-19 indicator (2020–2021) to account for the structural break during the pandemic.
-   **ARIMA error component:** Residuals are modeled as ARIMA(0,2,1) or ARIMA(0,2,2), depending on the training window, capturing autocorrelation in the error structure.

#### 🧪 Model Evaluation

Three models were trained using expanding windows ending in 2007, 2014, and 2021. The final model (trained through 2021 with a COVID-19 indicator) delivered the most accurate forecasts for 2022 and 2023, confirming the model's ability to incorporate exogenous shocks and recent emission slowdowns.

#### 🔍 Key Takeaways

-   Forecasting CO₂ directly from population offers interpretable short-term predictions without relying on uncertain economic or policy-based predictors.
-   Including a COVID-19 shock variable prevented overfitting to the pandemic drop and significantly improved out-of-sample forecast accuracy.
-   This model structure is particularly useful for near-term climate trajectory analysis when global policy responses are uncertain or nonlinear.

📁 Full report and forecasting code are available in this repository: [github.com/Lwall02/co2_forecast](https://github.com/Lwall02/co2_forecast)
