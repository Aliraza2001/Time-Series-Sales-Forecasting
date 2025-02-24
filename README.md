# Time-Series-Sales-Forecasting
This project focuses on analyzing and forecasting sales data for different product categories using Time Series Analysis techniques. It involves data preprocessing, exploratory data analysis (EDA), anomaly detection, and predictive modeling using ARIMA and VAR.

Table of Contents:
Overview
Installation
Usage
Technologies Used
Preprocessing
Exploratory Data Analysis
Model Implementation
Results
Contributors
License


Overview:
This project aims to analyze and forecast sales data for five product categories: 1) Poultry 2) Meats 3) Liquor 4) Prepared Foods 5) Frozen Foods. The dataset consists of daily sales records for each category, and the goal is to identify trends, seasonality, and external factors that influence sales. Predictive models are implemented to forecast future sales.


Technologies Used:
Data Processing: NumPy, Pandas
Visualization: Matplotlib, Seaborn
Time Series Modeling: Statsmodels, pmdarima
Forecasting Models: ARIMA, VAR


Preprocessing:
Handling Missing Values:
Removed rows with NA values.
Applied log normalization to stabilize variance.
Anomaly Detection:
Used STL decomposition to detect sales anomalies.
Applied Simple Moving Average (SMA) smoothing.


Exploratory Data Analysis:
Seasonality Analysis:
Identified repeating sales patterns.
Explored effects of external events (e.g., bi-weekly wages, earthquakes).
Change Point Analysis:
Detected significant shifts in sales trends.
Autocorrelation & Partial Autocorrelation (ACF/PACF):
Determined time series stationarity.


Model Implementation:
ARIMA Model
Applied Auto ARIMA to determine the best model parameters.
Forecasted sales for 15 days.
Evaluated results with MAE, MAPE, and RMSE.
VAR Model
Used Vector Autoregression (VAR) for multivariate forecasting.
Optimized lag selection using AIC.
Compared forecasts with actual test data.


Results:
Model	Poultry	Meats	Liquor	Prepared Foods	Frozen Foods
ARIMA MAPE	55.41%	55.83%	6.88%	38.30%	125.87%
VAR MAPE	53.78%	50.12%	15.20%	36.42%	140.25%
Liquor sales were best predicted by ARIMA.
VAR performed better than ARIMA for meats and poultry.
Both models struggled with frozen foods due to irregular sales patterns.


Contributor:
Raza Mehdi
