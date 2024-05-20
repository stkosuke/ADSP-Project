# 1. Loading the Data

# We use four data sets with different frequencies (from hourly to monthly).

# library packages
library(readr)
library(fpp3)
library(fable.prophet)

# load the data from the github repository
repo_path <- "https://raw.githubusercontent.com/stkosuke/ADSP-Project/main"

df_drug_hourly <- read_csv(file.path(repo_path, "saleshourly.csv"))
df_drug_daily <- read_csv(file.path(repo_path, "salesdaily.csv"))
df_drug_weekly <- read_csv(file.path(repo_path, "salesweekly.csv"))
df_drug_monthly <- read_csv(file.path(repo_path, "salesmonthly.csv"))

# convert them to long tsibble objects
df_drug_hourly <- df_drug_hourly |> 
  mutate(Datetime = mdy_hm(datum)) |>
  select(-datum, -Year, -Month, -Hour, -`Weekday Name`) |> 
  as_tsibble(index = Datetime)　|> 
  pivot_longer(cols = -Datetime, names_to = "Drug", values_to = "Sales")

df_drug_daily <- df_drug_daily |> 
  mutate(Date = mdy(datum)) |>
  select(-datum, -Year, -Month, -Hour, -`Weekday Name`) |> 
  as_tsibble(index = Date) |> 
  pivot_longer(cols = -Date, names_to = "Drug", values_to = "Sales")

df_drug_weekly <- df_drug_weekly |> 
  mutate(Week = yearweek(datum)) |>
  select(-datum) |> 
  as_tsibble(index = Week) |> 
  pivot_longer(cols = -Week, names_to = "Drug", values_to = "Sales")

df_drug_monthly <- df_drug_monthly |> 
  mutate(Month = yearmonth(datum)) |>
  select(-datum) |> 
  as_tsibble(index = Month) |> 
  pivot_longer(cols = -Month, names_to = "Drug", values_to = "Sales")

# Summarize the sales data by month
df_drug_monthly_fixed <- df_drug_daily |> 
  as.tibble() |> 
  mutate(Month = yearmonth(Date)) |> 
  group_by(Month, Drug) |> 
  summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop') |> 
  as_tsibble(index = Month, key = Drug) 


# 2. Exploratory Data Analysis
## a. Comparison among the data sets
### Monthly data
# The following features can be seen in the figure below.     \par
# - Seasonality of the one-year period can be observed in some drugs (especially in N02BE, R03, and R06).   \par
# - Each drug has different cycles and there are no uniform seasonality.   \par
# - There is a significant drop in sales in January 2017 and October 2019.

# time plot
df_drug_monthly_fixed |> 
  autoplot(Sales) + 
  labs(title = "Monthly sales of drugs") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) +
  theme(legend.position = "none")

# Seasonal subseries plots
df_drug_monthly_fixed |> 
  gg_subseries(Sales) + 
  labs(title = "Monthly sales of drugs") +
  theme(legend.position = "none")

df_drug_monthly_fixed |> 
  ggplot(aes(x = as.factor(month(Month)), y = Sales)) +
  geom_boxplot() +
  facet_wrap(~ Drug, scales = "free_y", ncol = 2)

df_drug_monthly_fixed |> 
  gg_season(Sales)

### Weekly data
# The following features can be seen in the figure below.   \par
# - Seasonality of the one-year period can also be observed in some drugs (especially in N02BE, R03, and R06) as we can see from the monthly data.  \par
# - There is no significant drop in sales in January 2017 and October 2019, which means there may be some problems with aggregation. \par

# time plot
df_drug_weekly |> 
  autoplot(Sales) + 
  labs(title = "weekly sales of drugs") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) +
  theme(legend.position = "none")

# Seasonal subseries plots
df_drug_weekly |> 
  gg_subseries(Sales) + 
  labs(title = "weekly sales of drugs") +
  theme(legend.position = "none")

df_drug_weekly |> 
  ggplot(aes(x = as.factor(week(Week)), y = Sales)) +
  geom_boxplot() +
  facet_wrap(~ Drug, scales = "free_y", ncol = 1)

df_drug_weekly |> 
  gg_season(Sales)

### Daily data
# The following features can be seen in the figure below.   \par
# - We cannot observe any weekly seasonality from the daily data.

# time plot
df_drug_daily |> 
  autoplot(Sales) + 
  labs(title = "daily sales of drugs") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) +
  theme(legend.position = "none")

# Seasonal subseries plots
df_drug_daily |> 
  gg_subseries(Sales, period = "week") + 
  labs(title = "daily sales of drugs") +
  theme(legend.position = "none")

df_drug_daily |> 
  ggplot(aes(x = weekdays(Date), y = Sales)) +
  geom_boxplot() +
  facet_wrap(~ Drug, scales = "free_y", ncol = 2)

df_drug_daily |> 
  gg_season(Sales, period = "week")

df_drug_daily |> 
  gg_season(Sales, period = "month")

df_drug_daily |> 
  gg_season(Sales, period = "year")

### Hourly data
# The following features can be seen in the figure below. \par
# - We can observe daily seasonality from the data however it can be estimated that it just comes from the business hours.

# time plot
df_drug_hourly |> 
  autoplot(Sales) + 
  labs(title = "hourly sales of drugs") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 1) +
  theme(legend.position = "none")

# Seasonal subseries plots
df_drug_hourly |> 
  gg_subseries(Sales, period = "day") + 
  labs(title = "daily sales of drugs") +
  theme(legend.position = "none")

df_drug_hourly |> 
  ggplot(aes(x = as.factor(hour(Datetime)), y = Sales)) +
  geom_boxplot() +
  facet_wrap(~ Drug, scales = "free_y", ncol = 1)

df_drug_hourly |> 
  gg_season(Sales, period = "day")


# From the above, it can be estimated that this data has a one-year seasonality, and therefore, weekly or monthly data are candidates for data that can be utilized in the forecast. Since there are some missing data of unknown reason in the monthly data, it is appropriate to use the weekly data in this analysis.

## b. Moving average smoothing and Decomposition
# In the following, monthly data is used to perform Moving average smoothing and STL Decomposition.

# remove the data of the last month
df_drug_monthly_fixed <- df_drug_monthly_fixed |> 
  filter(as.character(Month) != "2019 Oct")

## Moving average smoothing

# add the 12-MA and 12x2-MA to the data set
df_drug_monthly_fixed <- df_drug_monthly_fixed |> 
  group_by(Drug) |> 
  mutate(
    MA_12 = slider::slide_dbl(Sales, mean, 
                              .before = 5, .after = 6, .complete = TRUE),
    MA_12x2 = slider::slide_dbl(MA_12, mean, 
                                .before = 1, .after = 0, .complete = TRUE)
  ) |> 
  ungroup()

# plot 12x2-MA
df_drug_monthly_fixed |> 
  autoplot(Sales) +
  geom_line(aes(y = MA_12x2, color = "12-month Moving Average"), size =1) +
  labs(title = "Monthly Sales of Drugs with 12-month Moving Average") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) +
  theme(legend.position = "bottom")

# plot standardized moving average 
df_drug_monthly_fixed <- df_drug_monthly_fixed |> 
  group_by(Drug) |> 
  mutate(stdl_MA = MA_12x2 / MA_12x2[as.character(Month) == "2014 Jul"]) |> 
  ungroup()

# all in one plot
df_drug_monthly_fixed |> 
  autoplot(stdl_MA, size =1) +
  labs(title = "Standardized 12-month Moving Average",
       y = "Standardized Monthly Sales") +
  theme(legend.position = "bottom")

# M01AB, N02BA, M01AE and N02BE
df_drug_monthly_fixed |> 
  autoplot(stdl_MA, size =0.1) +
  labs(title = "Standardized 12-month Moving Average",
       y = "Standardized Monthly Sales") +
  theme(legend.position = "bottom") +
  geom_line(data = df_drug_monthly_fixed |>  
              filter(Drug %in% c("M01AB", "N02BA", "M01AE", "N02BE")),
            aes(x = Month, y = stdl_MA, color = Drug), size = 1)

# N05B, N05C, R03 and R06
df_drug_monthly_fixed |> 
  autoplot(stdl_MA, size =0.1) +
  labs(title = "Standardized 12-month Moving Average",
       y = "Standardized Monthly Sales") +
  theme(legend.position = "bottom") +
  geom_line(data = df_drug_monthly_fixed |>  
              filter(Drug %in% c("N05B", "N05C", "R03", "R06")),
            aes(x = Month, y = stdl_MA, color = Drug), size = 1)


## Decomposition

# function for Classical additive decomposition
CL_dcmp <- function(df, var, code) {
  df |>
    filter(get(var) == code) |>
    model(
      classical_decomposition(Sales, type = "additive")
    ) |>
    components() |>
    autoplot() +
    ggtitle(paste("Classical additive decomposition:", code))
}

# apply Classical additive to all unique drug code
lapply(unique(df_drug_monthly_fixed$Drug),
       function(code) {
         CL_dcmp(df=df_drug_monthly_fixed, var="Drug", code=code)
         }
       )

# function for STL decomposition
STL_dcmp <- function(df, var, code) {
  df |>
    filter(get(var) == code) |>
    model(
      STL(Sales ~ trend(window = 13) +
            season(window = 21),
          robust = TRUE)
    ) |>
    components() |>
    autoplot() +
    ggtitle(paste("STL Decomposition:", code))
}

# apply STL decomposition to all unique drug code
lapply(unique(df_drug_monthly_fixed$Drug),
       function(code) {
         STL_dcmp(df=df_drug_monthly_fixed, var="Drug", code=code)
       }
)


# <span style="color:red;">Add some implications</span> from the decomposition.

## c. Stationary Analysis
# In the following, weekly data is used.

## ACF and PACF
# ACF plot
df_drug_monthly_fixed |> 
  ACF(Sales) |> 
  autoplot() +
  labs(title = "ACF for the monthly sales") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) 

# PACF plot
df_drug_monthly_fixed |> 
  PACF(Sales) |> 
  autoplot() +
  labs(title = "PACF for the monthly sales") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) 

# <span style="color:red;">Add some implications</span> .

## d. ADF test

# function for ADF test
ADF_test <- function(drug_code) {
  print(drug_code)
  df_drug_monthly_fixed |>
    filter(Drug == drug_code) |>
    pull(Sales) |> 
    tseries::adf.test() |> 
    print()
}

# apply ADF test to all unique drug code
for (drug_code in unique(df_drug_monthly_fixed$Drug)) {
  ADF_test(drug_code)
}

# make them stationary

# function for differencing
Diff_ts <- function(drug_code) {
  # differencing and ADF test
  df_drug_monthly_fixed |>
    filter(Drug == drug_code) |>
    pull(Sales) |> 
    diff() |> 
    tseries::adf.test() |> 
    print()
  
  # plot the differenced data
  df_drug_monthly_fixed |> 
    filter(Drug == drug_code) |>
    mutate(Diff = Sales - lag(Sales)) |> 
    filter(!is.na(Diff)) |> 
    ggplot(aes(x = Month, y = Diff)) +
    geom_line() +
    labs(title = paste("Monthly Sales Difference for", drug_code))
}

# M01AB (differenced)
Diff_ts("M01AB")

# M01AE (differenced)
Diff_ts("M01AE")

# N02BA (differenced)
Diff_ts("N02BA")

# N05B(differenced)
Diff_ts("N05B")

# N05B(differenced)
Diff_ts("N05C")

# 3. Forecasting

## Forecasting
# use 12 months as a test set  
train <- df_drug_monthly_fixed |>
  filter_index("2014 Jan" ~ "2018 Sep")

test <- df_drug_monthly_fixed |>
  filter_index("2018 Oct" ~ "2019 Sep")

## a. Fit the models
### 1. Baseline Models (Mean, Naïve and Seasonal naïve)

# Fit the models
base_fit <- train |>
  model(
    Mean = MEAN(Sales),
    `Naïve` = NAIVE(Sales),
    `Seasonal naïve` = SNAIVE(Sales)
  )

# Generate forecasts for 12 months
base_fc <- base_fit |> forecast(h = 12)

# Plot forecasts against actual values
base_fc |>
  autoplot(test, size=1, level = NULL) +
  autolayer(train, colour = "black") +
  labs(
    y = "Sales",
    title = "Base Forecasts for monthly Drug Sales"
  ) +
  guides(colour = guide_legend(title = "Forecast")) +
  theme(legend.position = "bottom") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2)

# Mean including confidence interval
base_fc |>
  filter(.model == "Mean") |> 
  autoplot(test, size=1,  colour = "red", level = c(85,90)) +
  autolayer(train, colour = "black") +
  labs(
    y = "Sales",
    title = "Mean Forecasts for monthly Drug Sales (including interval)"
  ) +
  guides(colour = guide_legend(title = "Forecast")) +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) 

# Naive including confidence interval
base_fc |>
  filter(.model == "Naïve") |> 
  autoplot(test, size=1, colour = "#006400", level = c(85,90)) +
  autolayer(train, colour = "black") +
  labs(
    y = "Sales",
    title = "Naïve Forecasts for monthly Drug Sales (including interval)"
  ) +
  guides(colour = guide_legend(title = "Forecast")) +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2)

# Seasonal naïve including confidence interval
base_fc |>
  filter(.model == "Seasonal naïve") |> 
  autoplot(test, size=1, level = c(85,90)) +
  autolayer(train, colour = "black") +
  labs(
    y = "Sales",
    title = "Seasonal naïve Forecasts for monthly Drug Sales (including interval)"
  ) +
  guides(colour = guide_legend(title = "Forecast")) +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2)

# calculate accuracy
base_ac <- accuracy(base_fc, test)

# select RMSE and MAPE and convert it to wide data
base_rmse <- base_ac |> 
  select(.model, Drug, RMSE) |> 
  pivot_wider(names_from = Drug, values_from = RMSE)

base_rmse

base_mape <- base_ac |> 
  select(.model, Drug, MAPE) |> 
  pivot_wider(names_from = Drug, values_from = MAPE)

base_mape


### 2. ARIMA Models 

## ARIMA Models

# Fit the models for stationary data
arima_fit_sta <- train |>
  filter(Drug %in% c("N02BE", "R03", "R06")) |> 
  model(ARIMA(Sales))

arima_fit_sta

# Fit the models for non-stationary data
arima_fit_non_sta <- train |>
  filter(!Drug %in% c("N02BE", "R03", "R06")) |> 
  model(ARIMA(Sales ~ pdq(d=1:2)))

arima_fit_non_sta

# combine the result
names(arima_fit_non_sta) <- names(arima_fit_sta)

arima_fit <- 
  rbind(arima_fit_sta, arima_fit_non_sta) |> 
  arrange(Drug)

arima_fit

# Generate forecasts for 12 months
arima_fc <- arima_fit |> forecast(h = 12)

# Plot forecasts against actual values
arima_fc |>
  autoplot(test, size=1, level = c(85,90)) +
  autolayer(train, colour = "black") +
  labs(
    y = "Sales",
    title = "ARIMA Forecasts for monthly Drug Sales"
  ) +
  guides(colour = guide_legend(title = "Forecast")) +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2)

# calculate accuracy
arima_ac <- accuracy(arima_fc, test)

# select RMSE and MAPE and convert it to wide data
arima_rmse <- arima_ac |> 
  select(.model, Drug, RMSE) |> 
  pivot_wider(names_from = Drug, values_from = RMSE)

arima_rmse

arima_mape <- arima_ac |> 
  select(.model, Drug, MAPE) |> 
  pivot_wider(names_from = Drug, values_from = MAPE)

arima_mape

### 3. ETS Models 

## ETS Models

# Fit the models
ets_fit <- train |>
  model(ETS(Sales))

ets_fit

# Generate forecasts for 12 months
ets_fc <- ets_fit |> forecast(h = 12)

# Plot forecasts against actual values
ets_fc |>
  autoplot(test, size=1, level = c(85,90)) +
  autolayer(train, colour = "black") +
  labs(
    y = "Sales",
    title = "ETS Forecasts for monthly Drug Sales (for test data)"
  ) +
  guides(colour = guide_legend(title = "Forecast")) +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2)


# calculate accuracy
ets_ac <- accuracy(ets_fc, test)

# select RMSE and MAPE and convert it to wide data
ets_rmse <- ets_ac |> 
  select(.model, Drug, RMSE) |> 
  pivot_wider(names_from = Drug, values_from = RMSE)

ets_rmse

ets_mape <- ets_ac |> 
  select(.model, Drug, MAPE) |> 
  pivot_wider(names_from = Drug, values_from = MAPE)

ets_mape

### 4. Prophet Models 

## Prophet Models

# Fit the models
prophet_fit <- train |>
  model(prophet(Sales))

prophet_fit

# Generate forecasts for 12 months
prophet_fc <- prophet_fit |> forecast(h = 12)

# Plot forecasts against actual values
prophet_fc |>
  autoplot(test, size=1, level = c(85,90)) +
  autolayer(train, colour = "black") +
  labs(
    y = "Sales",
    title = "Prophet Forecasts for monthly Drug Sales (for test data)"
  ) +
  guides(colour = guide_legend(title = "Forecast")) +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2)

# calculate accuracy
prophet_ac <- accuracy(prophet_fc, test)

# select RMSE and MAPE and convert it to wide data
prophet_rmse <- prophet_ac |> 
  select(.model, Drug, RMSE) |> 
  pivot_wider(names_from = Drug, values_from = RMSE)

prophet_rmse

prophet_mape <- prophet_ac |> 
  select(.model, Drug, MAPE) |> 
  pivot_wider(names_from = Drug, values_from = MAPE)

prophet_mape

### 5. LSTM

library(keras)
library(tensorflow)

prepare_data_for_lstm <- function(df, look_back = 1) {
  df <- df %>%
    group_by(Drug) %>%
    mutate(Sales_scaled = scale(Sales)) %>%
    ungroup()
  
  data_list <- list()
  
  for (drug in unique(df$Drug)) {
    drug_data <- df %>% filter(Drug == drug)
    sales_scaled <- drug_data$Sales_scaled
    
    x <- list()
    y <- list()
    
    for (i in seq(look_back, length(sales_scaled) - 1)) {
      x[[i - look_back + 1]] <- sales_scaled[(i - look_back + 1):i]
      y[[i - look_back + 1]] <- sales_scaled[i + 1]
    }
    
    data_list[[drug]] <- list(x = array(unlist(x), dim = c(length(x), look_back, 1)),
                              y = array(unlist(y), dim = c(length(y), 1)))
  }
  
  return(data_list)
}

calculate_mse <- function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}

calculate_mape <- function(y_true, y_pred) {
  mean(abs((y_true - y_pred) / y_true)) * 100
}

train_lstm <- function(data, epochs = 100, batch_size = 32, look_back = 1) {
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(look_back, 1)) %>%
    layer_lstm(units = 50) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = 'adam'
  )
  
  history <- model %>% fit(
    x = data$x,
    y = data$y,
    epochs = epochs,
    batch_size = batch_size,
    validation_split = 0.2,
    verbose = 2
  )
  
  predictions <- model %>% predict(data$x)
  
  mse <- calculate_mse(data$y, predictions)
  mape <- calculate_mape(data$y, predictions)
  
  return(list(model = model, history = history, mse = mse, mape = mape))
}

data_list <- prepare_data_for_lstm(df_drug_weekly)

models <- list()
for (drug in names(data_list)) {
  cat("Training model for drug:", drug, "\n")
  models[[drug]] <- train_lstm(data_list[[drug]])
}

for (drug in names(models)) {
  cat("Drug:", drug, "\n")
  cat("MSE:", models[[drug]]$mse, "\n")
  cat("MAPE:", models[[drug]]$mape, "%\n\n")
}

for (selected_drug in names(data_list)) {
  actual_data <- data_list[[selected_drug]]
  if (!is.null(models[[selected_drug]])) {
    model <- models[[selected_drug]]$model
    predictions <- predict(model, actual_data$x)
    
    results_df <- data.frame(
      Time = 1:length(actual_data$y),
      Actual = actual_data$y,
      Predicted = predictions
    )
    
    p <- ggplot(results_df, aes(x = Time)) +
      geom_line(aes(y = Actual, colour = "Actual"), size = 1.2) +
      geom_line(aes(y = Predicted, colour = "Predicted"), size = 1.2, linetype = "dashed") +
      labs(title = paste("Actual vs Predicted Values for", selected_drug),
           x = "Time", y = "Value") +
      scale_colour_manual(values = c("Actual" = "blue", "Predicted" = "red"),
                          name = "",
                          labels = c("Actual", "Predicted")) +
      theme_minimal()
    
    print(p)
  } else {
    print(paste("No model available for", selected_drug))
  }
}

## b. Model Comparison

## comparison
rmse_all <- rbind(base_rmse, arima_rmse, ets_rmse, prophet_rmse)
rmse_all

mape_all <- rbind(base_mape, arima_mape, ets_mape, prophet_mape)
mape_all

