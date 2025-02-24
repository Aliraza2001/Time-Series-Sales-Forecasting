# Yash Vijay (7589244), Sohaib Iftikhar (7023686), Musa Khan (7053757), Syed Raza (7597964)

install.packages("tidyverse")
install.packages("rlang")
install.packages("ggplot2")
install.packages("forecast")
install.packages("lubridate")
install.packages("changepoint")
install.packages("strucchange")
install.packages("tseries")
install.packages("vars")

library(vars)
library(tseries)
library(lubridate)
library(strucchange)
library(changepoint)
library(tidyverse)
library(forecast)
library(ggplot2)

#1. Preparing Data Time Series Frame ----
#1.1 Loading CSV Files into Data Frame
poultry_df = read.csv("sample1_sales_timeseries.csv", header = TRUE)
meats_df = read.csv("sample2_sales_timeseries.csv", header = TRUE)
liquor_df = read.csv("sample3_sales_timeseries.csv", header = TRUE)
prep_foods_df = read.csv("sample4_sales_timeseries.csv", header = TRUE)
frozen_foods_df = read.csv("sample5_sales_timeseries.csv", header = TRUE)

View(prep_foods_df)

#1.2 Grouping By Date and Getting Total Sales
grouped_poultry_df <- poultry_df %>% 
  group_by(date) %>%
  summarise(poultry = sum(sales))

grouped_meats_df <- meats_df %>% 
  group_by(date) %>%
  summarise(meats= sum(sales))

grouped_liquor_df <- liquor_df %>% 
  group_by(date) %>%
  summarise(liquor = sum(sales))

grouped_prep_foods_df <- prep_foods_df %>% 
  group_by(date) %>%
  summarise(prep_foods = sum(sales))

grouped_frozen_foods_df <- frozen_foods_df %>% 
  group_by(date) %>%
  summarise(frozen_foods = sum(sales))

#1.3 Joining Data Frames
?join

df_12 <- grouped_poultry_df %>% full_join(grouped_meats_df)
head(df_12)

df_34 <- grouped_liquor_df %>% full_join(grouped_prep_foods_df)
head(df_34)

df_1234 <- df_12 %>% full_join(df_34)
head(df_1234)

df_final <- df_1234 %>% full_join(grouped_frozen_foods_df)

# Converting column "date" from type "chr" to "date"
df_final$date <- as.Date(df_final$date)

str(df_final)
summary(df_final)

View(df_final)

#1.4 Creating Time Series Data Frame
ts_data <- msts(df_final[, -1], seasonal.periods = c(15, 365.25)) #excluding date column
ts_data

#1.5 Checking "ts_data" information
summary(ts_data)
str(ts_data)

start(ts_data) #1 1
end(ts_data) #5 224
frequency(ts_data) #365
class(ts_data) #"msts" "ts" 

time(ts_data) #includes start, end, and frequency all in one
cycle(ts_data)

#2. EDA ----
#2.1 Basic Exploration of "df_final" [lab 2 Plots] ----
ggplot(data = df_final, aes(x = date, y = liquor)) +
  geom_line(colour="purple")+
  geom_smooth()+
  theme_minimal()

ggplot(data = df_final) +
  geom_line(aes(x = date, y = poultry, colour = "poultry")) +
  geom_smooth(aes(x = date, y = poultry, colour = "poultry")) +
  geom_line(aes(x = date, y = meats, colour = "meats")) +
  geom_smooth(aes(x = date, y = meats, colour = "meats")) +
  labs(title = "Comparison of Poultry and Meats Sales Over Time",
       x = "Date",
       y = "Total Sales") +
  scale_colour_manual(name = "Category", values = c("poultry" = "blue", "meats" = "red")) +
  theme_minimal()

# Using Esquisse
ggplot(df_final) +
  aes(x = date, y = frozen_foods, colour = frozen_foods) +
  geom_line() +
  scale_color_viridis_c(option = "magma", direction = 1) +
  labs(
    x = "Date",
    y = "Sales",
    title = "Frozen Foods Sales",
    color = "Sales "
  ) +
  theme_classic()

ggplot(df_final) +
  aes(x = date, y = prep_foods) +
  geom_point(
    shape = "triangle",
    size = 1.5,
    colour = "#1B5E70"
  ) +
  geom_smooth(span = 0.75) +
  labs(
    x = "Date",
    y = "Sales",
    title = "Prepped Foods Sales"
  ) +
  theme_classic()


#2.2 Exploration of Time Series Data ----
plot(ts_data, 
     xlab="Date", 
     ylab="Total Sales",
     main="Total Sales Over Time"
)

#2.2.1 Decomposing ----
# Decomposing to see trend, residuals, and seasonality
categories <- colnames(df_final)[-1]
categories

# Function to Decompose and Plot: decomposes ts dataset and plots the trend, seasonality, and residuals
decompose_and_plot <- function(ts, category_name) {
  decomp <- decompose(ts)
  plot(decomp)
  mtext(category_name, side = 1, line = 4.25) # to print only the category name at the bottom
}

# Loops through each category, decomposes the time series, and plots the components.
for (category in categories) {
  category_ts <- ts(ts_data[, category], frequency = 365.25)
  decompose_and_plot(category_ts, category)
}

#2.3 Exploring Specific Events ----
# Adding columns to mark bi-weekly wage payment dates 
df_final$payment_day <- ifelse(day(df_final$date) == 15 | day(df_final$date) == days_in_month(df_final$date), "Payment Day", "Other")
View(df_final)

# Plotting sales data with annotations for bi-weekly wage payments
ggplot(data = df_final, aes(x = date, y = liquor)) +
  geom_line(colour = "purple") +
  geom_smooth() +
  geom_vline(data = df_final %>% filter(payment_day == "Payment Day"), aes(xintercept = as.numeric(date)), linetype = "dashed", colour = "red") +
  theme_minimal() +
  labs(title = "Liquor Sales with Bi-Weekly Wage Payment Days", x = "Date", y = "Sales")

# Plotting sales data with annotations for earthquake impact
ggplot(data = df_final, aes(x = date, y = poultry)) +
  geom_line(colour = "light blue") +
  geom_smooth() +
  geom_vline(xintercept = as.numeric(as.Date("2016-04-16")), linetype = "solid", colour = "red") +
  theme_minimal() +
  labs(title = "Poultry Sales with Earthquake Impact Period", x = "Date", y = "Sales")


#2.4 Change Point Analysis ----
# Performing change point analysis on liquor sales based on "variance" 
cpt_liquor_var <- cpt.var(df_final$liquor, method = "PELT", test.stat = "Normal")
# Plottinh the change points
plot(cpt_liquor_var, cpt.width = 3, main = "Change Points for Liquor Sales (Variance)", xlab = "Date", ylab = "Sales")
# Convert change points to dates for display
change_points <- cpts(cpt_liquor_var)
change_points_dates <- df_final$date[change_points]
print(change_points_dates)

# Change point on liquor but using "Mean"
cpt_liquor_mean <- cpt.mean(df_final$liquor, method = "PELT", test.stat = "Normal")
plot(cpt_liquor_mean, cpt.width = 3, main = "Change Points for Liquor Sales (Mean)", xlab = "Date", ylab = "Sales")
change_points <- cpts(cpt_liquor_mean)
change_points_dates <- df_final$date[change_points]
print(change_points_dates)


# Function to perform change point analysis and plot the results for all categories
perform_cpt_analysis <- function(data, category, method, test_stat) {
  cpt_result <- cpt.meanvar(data[[category]], method = method, test.stat = test_stat)
  
  # Plotting the change points
  plot(cpt_result, cpt.width = 3, main = paste("Change Points for", category), xlab = "Date", ylab = "Sales")
  
  change_points <- cpts(cpt_result)
  abline(v = change_points, col = "blue", lty = 1, lwd = 2) #adding the lines
  
  # Displaying change points
  change_points_dates <- data$date[change_points]
  print(paste("Change points for", category, ":", change_points_dates))
  
  return(change_points_dates)
}

categories <- colnames(df_final)[2:6]  

# Perform change point analysis for each category based on mean and variance
for (category in categories) {
  print(paste("Performing change point analysis for:", category))
  
  mean_change_points <- perform_cpt_analysis(df_final, category, method = "PELT", test_stat = "Normal")
}

#2.5 ACF and PACF ----
# Converting all categories to single time series objects
ts_liquor <- ts(df_final$liquor)
ts_poultry <- ts(df_final$poultry)
ts_meats <- ts(df_final$meats)
ts_frozen_foods <- ts(df_final$frozen_foods)
ts_prep_foods <- ts(df_final$prep_foods)

# Creating a function to plot ACF and PACF for each single category
plot_acf_pacf <- function(ts_data, category_name) {
  par(mfrow=c(1,2))  # helps w/ placements of both plots side-by-side
  acf(ts_data, main=paste("Autocorrelation Function (ACF) of", category_name, "Sales"))
  pacf(ts_data, main=paste("Partial Autocorrelation Function (PACF) of", category_name, "Sales"))
  par(mfrow=c(1,1))  # Resetting the placement
}

# calling func. for each category
plot_acf_pacf(ts_liquor, "Liquor")
plot_acf_pacf(ts_poultry, "Poultry")
plot_acf_pacf(ts_meats, "Meats")
plot_acf_pacf(ts_frozen_foods, "Frozen Foods")
plot_acf_pacf(ts_prep_foods, "Prepared Foods")

#2.6 ADF Test (Checking Stationary) ---- 
#2.6.1 Differencing ----
ts_liquor_diff <- diff(ts_liquor)
ts_poultry_diff <- diff(ts_poultry)
ts_meats_diff <- diff(ts_meats)
ts_frozen_foods_diff <- diff(ts_frozen_foods)
ts_prep_foods_diff <- diff(ts_prep_foods)

#2.6.2 Checking stationary for differenced data ----
# Function to perform the ADF test for a category
new_perform_adf_test <- function(ts_data, category_name) {
  test_result <- adf.test(ts_data, alternative = "stationary")
  print(paste("ADF Test for", category_name, ":", test_result$p.value))
  
  if(test_result$p.value < 0.05) {
    cat("The time series", category_name, "is stationary at 5% level of significance.\n")
  } else {
    cat("The time series", category_name, "is not stationary at 5% level of significance.\n")
  }
}

# Calling above func. on each category
new_perform_adf_test(ts_liquor_diff, "Liquor")
new_perform_adf_test(ts_poultry_diff, "Poultry")
new_perform_adf_test(ts_meats_diff, "Meats")
new_perform_adf_test(ts_frozen_foods_diff, "Frozen Foods")
new_perform_adf_test(ts_prep_foods_diff, "Prepared Foods")

#3. Preprocessing ----

#3.1 Normalization ----
# Log Normalization Function
log_normalize <- function(df, columns) {
   df[columns] <- log(df[columns] + 1)  # Adding 1 to avoid log(0)
   return(df)
}

# Standardization Function
standardize <- function(df, columns) {
  df[columns] <- scale(df[columns])
  return(df)
}

# Printing original sales data, before normalization and standardization
print(head(df_final[categories]))

df_final_log <- df_final
for (category in categories) {
  # Checking for "0" values and adjusting them slightly 
  small_constant <- 0.001
  df_final_log[[category]] <- ifelse(df_final[[category]] <= 0, df_final[[category]] + small_constant, df_final[[category]])
  
  # Applying log normalization to each category
  df_final_log[[category]] <- log_normalize(df_final_log[[category]])
}

# Printing data after log-normalization
print(head(df_final_log[categories]))

# Applying standardization to each category
df_final_standardized <- df_final_log
for (category in categories) {
  df_final_standardized[[category]] <- standardize(df_final_log[[category]])
}

# Printing data after standardization
print(head(df_final_standardized[categories]))

# Checking mean and standard deviation to ensure results are correct
means <- sapply(df_final_standardized[categories], mean, na.rm = TRUE)
sds <- sapply(df_final_standardized[categories], sd, na.rm = TRUE)

print("Means of Standardized Columns:")
print(means)

print("Standard Deviations of Standardized Columns:")
print(sds)

View(df_final_standardized) # date column intact
str(df_final_standardized) # date column in Date format too

#3.2 Detecting Anomalies using STL decomposition ----
detect_anomalies <- function(series) {
  # Checking if the series is long enough for STL decomposition (must be >= 2 periods)
  if (length(series) < 2 * frequency(series)) {
    stop("The series is not periodic or has less than two periods")
  }
  
  # STL decomposition of ts dara
  stl_decomp <- stl(series, s.window = "periodic")
  observed <- series
  seasonal <- stl_decomp$time.series[, "seasonal"]
  trend <- stl_decomp$time.series[, "trend"]
  remainder <- stl_decomp$time.series[, "remainder"]
  
  # Defining an anomaly as "points where the remainder exceeds a threshold" (found online)
  # Threshold = 3x the standard deviation of the remainder (residual)
  threshold <- 3 * sd(remainder)
  anomalies <- which(abs(remainder) > threshold)
  
  list(anomalies = anomalies, stl_decomp = stl_decomp, threshold = threshold, observed = observed)
}

date_col <- df_final_standardized$date #keeping a separate column so date doesnt change

ts_poultry <- ts(df_final_standardized$poultry, frequency = 365.25)
ts_meats <- ts(df_final_standardized$meats, frequency = 365.25)
ts_liquor <- ts(df_final_standardized$liquor, frequency = 365.25)
ts_prep_foods <- ts(df_final_standardized$prep_foods, frequency = 365.25)
ts_frozen_foods <- ts(df_final_standardized$frozen_foods, frequency = 365.25)

# Applying to each separate category
poultry_anomalies <- detect_anomalies(ts_poultry)
meats_anomalies <- detect_anomalies(ts_meats)
liquor_anomalies <- detect_anomalies(ts_liquor)
prep_foods_anomalies <- detect_anomalies(ts_prep_foods)
frozen_foods_anomalies <- detect_anomalies(ts_frozen_foods)

# Function to count the no. of anomalies for a category
count_anomalies <- function(anomalies) {
  length(anomalies$anomalies)
}

# APplying the count func. to each cat.
poultry_count <- count_anomalies(poultry_anomalies)
meats_count <- count_anomalies(meats_anomalies)
liquor_count <- count_anomalies(liquor_anomalies)
prep_foods_count <- count_anomalies(prep_foods_anomalies)
frozen_foods_count <- count_anomalies(frozen_foods_anomalies)

# Summarizing the categorical anomaly counts
anomaly_summary <- data.frame(
  Category = c("Poultry", "Meats", "Liquor", "Prepared Foods", "Frozen Foods"),
  Anomalies = c(poultry_count, meats_count, liquor_count, prep_foods_count, frozen_foods_count)
)

print(anomaly_summary)

# Function to plot the time series with anomalies highlighted
plot_anomalies <- function(anomalies, title) {
  observed <- anomalies$observed
  anomaly_indices <- anomalies$anomalies
  anomaly_values <- observed[anomaly_indices]
  
  data <- data.frame(
    Date = time(observed),
    Observed = as.numeric(observed)
  )
  
  ggplot(data, aes(x = Date, y = Observed)) +
    geom_line(color = "blue") +
    geom_point(data = data[anomaly_indices, ], aes(x = Date, y = Observed), color = "red", size = 2) +
    labs(title = title, x = "Date", y = "Sales") +
    theme_minimal()
}

# Plotting for each cat.
print(plot_anomalies(poultry_anomalies, "Poultry Sales with Anomalies"))
print(plot_anomalies(meats_anomalies, "Meats Sales with Anomalies"))
print(plot_anomalies(liquor_anomalies, "Liquor Sales with Anomalies"))
print(plot_anomalies(prep_foods_anomalies, "Prepared Foods Sales with Anomalies"))
print(plot_anomalies(frozen_foods_anomalies, "Frozen Foods Sales with Anomalies"))


#3.3 Dealing with Anomalies and SMA smoothing ----
# Function to average out anomalies (targeting anomalies only)
average_out_anomalies <- function(series, anomalies) {
  series_cleaned <- series
  for (index in anomalies) {
    if (index > 1 && index < length(series)) {
      series_cleaned[index] <- mean(c(series[index - 1], series[index + 1]), na.rm = TRUE)
    } else if (index == 1) {
      series_cleaned[index] <- series[index + 1]
    } else if (index == length(series)) {
      series_cleaned[index] <- series[index - 1]
    }
  }
  return(series_cleaned)
}

# Function to apply Simple Moving Average (SMA) smoothing (targeting overall data)
smooth_with_sma <- function(series, window_size) {
  stats::filter(series, rep(1/window_size, window_size), sides = 2)
}

# Applying the avg. out func. to each category
poultry_smoothed <- average_out_anomalies(ts_poultry, poultry_anomalies$anomalies)
meats_smoothed <- average_out_anomalies(ts_meats, meats_anomalies$anomalies)
liquor_smoothed <- average_out_anomalies(ts_liquor, liquor_anomalies$anomalies)
prep_foods_smoothed <- average_out_anomalies(ts_prep_foods, prep_foods_anomalies$anomalies)
frozen_foods_smoothed <- average_out_anomalies(ts_frozen_foods, frozen_foods_anomalies$anomalies)

# Then applying SMA smoothing to each category
poultry_smoothed <- smooth_with_sma(poultry_smoothed, window_size = 7)
meats_smoothed <- smooth_with_sma(meats_smoothed, window_size = 7)
liquor_smoothed <- smooth_with_sma(liquor_smoothed, window_size = 7)
prep_foods_smoothed <- smooth_with_sma(prep_foods_smoothed, window_size = 7)
frozen_foods_smoothed <- smooth_with_sma(frozen_foods_smoothed, window_size = 7)

# Combining all cleaned categories into one data frame
cleaned_and_smoothed_data <- data.frame(
  Date = date_col,
  Poultry = poultry_smoothed,
  Meats = meats_smoothed,
  Liquor = liquor_smoothed,
  Prepared_Foods = prep_foods_smoothed,
  Frozen_Foods = frozen_foods_smoothed
)

View(cleaned_and_smoothed_data) 
str(cleaned_and_smoothed_data) # Date Column and format still intact



# Plotting the cleaned and smoothed time series
ggplot(cleaned_and_smoothed_data, aes(x = Date)) +
  geom_line(aes(y = Poultry, color = "Poultry")) +
  geom_line(aes(y = Meats, color = "Meats")) +
  geom_line(aes(y = Liquor, color = "Liquor")) +
  geom_line(aes(y = Prepared_Foods, color = "Prepared Foods")) +
  geom_line(aes(y = Frozen_Foods, color = "Frozen Foods")) +
  labs(title = "Smoothed Sales Data After Handling Anomalies", x = "Date", y = "Sales") +
  scale_color_manual(values = c("Poultry" = "blue", "Meats" = "red", "Liquor" = "green", "Prepared Foods" = "purple", "Frozen Foods" = "orange")) +
  theme_minimal()


#4. Model Implementation ----
#4.1 Train-Test Split ----

# Removing rows with NA values formed by SMA method
cleaned_and_smoothed_data <- cleaned_and_smoothed_data %>% drop_na()
View(cleaned_and_smoothed_data)

# Checking for any missing values
missing <- colSums(is.na(cleaned_and_smoothed_data))
print(missing)

# Setting test length (15 days)
test_length <- 15

train_data <- head(cleaned_and_smoothed_data, -test_length)
test_data <- tail(cleaned_and_smoothed_data, test_length)

# Extracting the time series columns
train_ts <- train_data %>% select(-Date)
test_ts <- test_data %>% select(-Date)

view(test_data)
view(train_data)

#4.2 ARIMA ----

#4.2.1 Fitting and Forecasting model to each category ----
# Function to fit ARIMA model and forecast
fit_arima_and_forecast <- function(train_series, test_length) {
  # Fit ARIMA model
  arima_model <- auto.arima(train_series)
  
  # Forecast the next 'test_length' days
  forecast_result <- forecast(arima_model, h = test_length)
  
  return(forecast_result)
}

# Fit ARIMA and forecast for each category
poultry_forecast <- fit_arima_and_forecast(ts(train_data$Poultry), test_length)
meats_forecast <- fit_arima_and_forecast(ts(train_data$Meats), test_length)
liquor_forecast <- fit_arima_and_forecast(ts(train_data$Liquor), test_length)
prep_foods_forecast <- fit_arima_and_forecast(ts(train_data$Prepared_Foods), test_length)
frozen_foods_forecast <- fit_arima_and_forecast(ts(train_data$Frozen_Foods), test_length)

# Extracting the forecasted (mean) values
forecast_values_arima <- data.frame(
  Date = seq.Date(from = max(train_data$Date) + 1, by = "day", length.out = test_length),
  Poultry = as.numeric(poultry_forecast$mean),
  Meats = as.numeric(meats_forecast$mean),
  Liquor = as.numeric(liquor_forecast$mean),
  Prepared_Foods = as.numeric(prep_foods_forecast$mean),
  Frozen_Foods = as.numeric(frozen_foods_forecast$mean)
)

print(forecast_values_arima)

#4.2.2 Evaluating Forecasts ----
# Comparing the forecasted values with the actual test data
comparison_arima <- data.frame(
  Date = test_data$Date,
  Actual_Poultry = test_data$Poultry,
  Forecasted_Poultry = forecast_values_arima$Poultry,
  Actual_Meats = test_data$Meats,
  Forecasted_Meats = forecast_values_arima$Meats,
  Actual_Liquor = test_data$Liquor,
  Forecasted_Liquor = forecast_values_arima$Liquor,
  Actual_Prepared_Foods = test_data$Prepared_Foods,
  Forecasted_Prepared_Foods = forecast_values_arima$Prepared_Foods,
  Actual_Frozen_Foods = test_data$Frozen_Foods,
  Forecasted_Frozen_Foods = forecast_values_arima$Frozen_Foods
)
print(comparison_arima)

# Plotting the actual vs forecasted values
ggplot(comparison_arima, aes(x = Date)) +
  geom_line(aes(y = Actual_Poultry, color = "Actual Poultry")) +
  geom_line(aes(y = Forecasted_Poultry, color = "Forecasted Poultry"), linetype = "dashed") +
  geom_line(aes(y = Actual_Meats, color = "Actual Meats")) +
  geom_line(aes(y = Forecasted_Meats, color = "Forecasted Meats"), linetype = "dashed") +
  geom_line(aes(y = Actual_Liquor, color = "Actual Liquor")) +
  geom_line(aes(y = Forecasted_Liquor, color = "Forecasted Liquor"), linetype = "dashed") +
  geom_line(aes(y = Actual_Prepared_Foods, color = "Actual Prepared Foods")) +
  geom_line(aes(y = Forecasted_Prepared_Foods, color = "Forecasted Prepared Foods"), linetype = "dashed") +
  geom_line(aes(y = Actual_Frozen_Foods, color = "Actual Frozen Foods")) +
  geom_line(aes(y = Forecasted_Frozen_Foods, color = "Forecasted Frozen Foods"), linetype = "dashed") +
  labs(title = "Actual vs Forecasted Sales (ARIMA)", x = "Date", y = "Sales") +
  scale_color_manual(values = c(
    "Actual Poultry" = "blue", "Forecasted Poultry" = "blue",
    "Actual Meats" = "red", "Forecasted Meats" = "red",
    "Actual Liquor" = "green", "Forecasted Liquor" = "green",
    "Actual Prepared Foods" = "purple", "Forecasted Prepared Foods" = "purple",
    "Actual Frozen Foods" = "orange", "Forecasted Frozen Foods" = "orange"
  )) +
  theme_minimal()

#4.2.3 Accuracy Metrics ----
# Function to calculate accuracy metrics
calculate_accuracy_metrics <- function(actual, forecasted) {
  mae <- mean(abs(actual - forecasted))
  mape <- mean(abs((actual - forecasted) / actual)) * 100
  rmse <- sqrt(mean((actual - forecasted)^2))
  return(c(MAE = mae, MAPE = mape, RMSE = rmse))
}

# Applying the function to each category
poultry_metrics <- calculate_accuracy_metrics(test_data$Poultry, forecast_values_arima$Poultry)
meats_metrics <- calculate_accuracy_metrics(test_data$Meats, forecast_values_arima$Meats)
liquor_metrics <- calculate_accuracy_metrics(test_data$Liquor, forecast_values_arima$Liquor)
prep_foods_metrics <- calculate_accuracy_metrics(test_data$Prepared_Foods, forecast_values_arima$Prepared_Foods)
frozen_foods_metrics <- calculate_accuracy_metrics(test_data$Frozen_Foods, forecast_values_arima$Frozen_Foods)

# Combining metrics into a data frame for display
accuracy_metrics <- data.frame(
  Category = c("Poultry", "Meats", "Liquor", "Prepared Foods", "Frozen Foods"),
  MAE = c(poultry_metrics["MAE"], meats_metrics["MAE"], liquor_metrics["MAE"], prep_foods_metrics["MAE"], frozen_foods_metrics["MAE"]),
  MAPE = c(poultry_metrics["MAPE"], meats_metrics["MAPE"], liquor_metrics["MAPE"], prep_foods_metrics["MAPE"], frozen_foods_metrics["MAPE"]),
  RMSE = c(poultry_metrics["RMSE"], meats_metrics["RMSE"], liquor_metrics["RMSE"], prep_foods_metrics["RMSE"], frozen_foods_metrics["RMSE"])
)

print(accuracy_metrics)

#4.3 VAR ----
#4.3.1 Fitting and Forecasting model to each category ----
# Determining the optimal lags based on  AIC 
var_selection <- VARselect(train_ts, lag.max = 15, type = "const")
optimal_lags <- var_selection$selection

# Fitting the VAR model
var_model <- VAR(train_ts, p = optimal_lags["AIC(n)"], type = "const") # choosing 'p' based on 'optimal_lags' 

# Forecasting the next 15 days
var_forecast <- predict(var_model, n.ahead = test_length)

# Extracting forecasted values
forecast_values_var <- data.frame(
  Date = seq.Date(from = max(train_data$Date) + 1, by = "day", length.out = test_length),
  Poultry = var_forecast$fcst$Poultry[, 1],
  Meats = var_forecast$fcst$Meats[, 1],
  Liquor = var_forecast$fcst$Liquor[, 1],
  Prepared_Foods = var_forecast$fcst$Prepared_Foods[, 1],
  Frozen_Foods = var_forecast$fcst$Frozen_Foods[, 1]
)

print(forecast_values_var)

#4.3.2 Evaluating Forecasts ----
# Comparing the forecasted values with the actual test data
comparison_var <- data.frame(
  Date = test_data$Date,
  Actual_Poultry = test_data$Poultry,
  Forecasted_Poultry = forecast_values_var$Poultry,
  Actual_Meats = test_data$Meats,
  Forecasted_Meats = forecast_values_var$Meats,
  Actual_Liquor = test_data$Liquor,
  Forecasted_Liquor = forecast_values_var$Liquor,
  Actual_Prepared_Foods = test_data$Prepared_Foods,
  Forecasted_Prepared_Foods = forecast_values_var$Prepared_Foods,
  Actual_Frozen_Foods = test_data$Frozen_Foods,
  Forecasted_Frozen_Foods = forecast_values_var$Frozen_Foods
)

print(comparison_var)


ggplot(comparison_var, aes(x = Date)) +
  geom_line(aes(y = Actual_Poultry, color = "Actual Poultry")) +
  geom_line(aes(y = Forecasted_Poultry, color = "Forecasted Poultry"), linetype = "dashed") +
  geom_line(aes(y = Actual_Meats, color = "Actual Meats")) +
  geom_line(aes(y = Forecasted_Meats, color = "Forecasted Meats"), linetype = "dashed") +
  geom_line(aes(y = Actual_Liquor, color = "Actual Liquor")) +
  geom_line(aes(y = Forecasted_Liquor, color = "Forecasted Liquor"), linetype = "dashed") +
  geom_line(aes(y = Actual_Prepared_Foods, color = "Actual Prepared Foods")) +
  geom_line(aes(y = Forecasted_Prepared_Foods, color = "Forecasted Prepared Foods"), linetype = "dashed") +
  geom_line(aes(y = Actual_Frozen_Foods, color = "Actual Frozen Foods")) +
  geom_line(aes(y = Forecasted_Frozen_Foods, color = "Forecasted Frozen Foods"), linetype = "dashed") +
  labs(title = "Actual vs Forecasted Sales (VAR)", x = "Date", y = "Sales") +
  scale_color_manual(values = c(
    "Actual Poultry" = "blue", "Forecasted Poultry" = "blue",
    "Actual Meats" = "red", "Forecasted Meats" = "red",
    "Actual Liquor" = "green", "Forecasted Liquor" = "green",
    "Actual Prepared Foods" = "purple", "Forecasted Prepared Foods" = "purple",
    "Actual Frozen Foods" = "orange", "Forecasted Frozen Foods" = "orange"
  )) +
  theme_minimal()

#4.3.3 Accuracy Metrics ----
# Applying the function (same one for ARIMA also) to each category for the VAR model
poultry_metrics_var <- calculate_accuracy_metrics(test_data$Poultry, forecast_values_var$Poultry)
meats_metrics_var <- calculate_accuracy_metrics(test_data$Meats, forecast_values_var$Meats)
liquor_metrics_var <- calculate_accuracy_metrics(test_data$Liquor, forecast_values_var$Liquor)
prep_foods_metrics_var <- calculate_accuracy_metrics(test_data$Prepared_Foods, forecast_values_var$Prepared_Foods)
frozen_foods_metrics_var <- calculate_accuracy_metrics(test_data$Frozen_Foods, forecast_values_var$Frozen_Foods)

# Combining metrics into a data frame to display
accuracy_metrics_var <- data.frame(
  Category = c("Poultry", "Meats", "Liquor", "Prepared Foods", "Frozen Foods"),
  MAE = c(poultry_metrics_var["MAE"], meats_metrics_var["MAE"], liquor_metrics_var["MAE"], prep_foods_metrics_var["MAE"], frozen_foods_metrics_var["MAE"]),
  MAPE = c(poultry_metrics_var["MAPE"], meats_metrics_var["MAPE"], liquor_metrics_var["MAPE"], prep_foods_metrics_var["MAPE"], frozen_foods_metrics_var["MAPE"]),
  RMSE = c(poultry_metrics_var["RMSE"], meats_metrics_var["RMSE"], liquor_metrics_var["RMSE"], prep_foods_metrics_var["RMSE"], frozen_foods_metrics_var["RMSE"])
)

print(accuracy_metrics_var)

# Comparing ARIMA and VAR model results
accuracy_metrics_comparison <- data.frame(
  Category = accuracy_metrics$Category,
  ARIMA_MAE = accuracy_metrics$MAE,
  VAR_MAE = accuracy_metrics_var$MAE,
  ARIMA_MAPE = accuracy_metrics$MAPE,
  VAR_MAPE = accuracy_metrics_var$MAPE,
  ARIMA_RMSE = accuracy_metrics$RMSE,
  VAR_RMSE = accuracy_metrics_var$RMSE
)

print(accuracy_metrics_comparison)
