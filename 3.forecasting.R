library(fable.prophet)

## Forecasting

# use 52 weeks as a test set  
train <- df_drug_weekly |>
  filter_index("2014 W01" ~ "2018 W41")

test <- df_drug_weekly |>
  filter_index("2018 W42" ~ "2019 W41")

## Baseline Models

# Fit the models
base_fit <- train |>
  model(
    Mean = MEAN(Sales),
    `Naïve` = NAIVE(Sales),
    `Seasonal naïve` = SNAIVE(Sales)
  )

# Generate forecasts for 52 weeks
base_fc <- base_fit |> forecast(h = 52)

# Plot forecasts against actual values
base_fc |>
  autoplot(test, level = NULL) +
  autolayer(
    filter_index(df_drug_weekly, "2018 W42" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Sales",
    title = "BAse Forecasts for weekly Drug Sales (for test data)"
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


## ARIMA Models

# Fit the models
arima_fit <- train |>
  model(ARIMA(Sales))

arima_fit

# Generate forecasts for 52 weeks
arima_fc <- arima_fit |> forecast(h = 52)

# Plot forecasts against actual values
arima_fc |>
  autoplot(test, level = NULL) +
  autolayer(
    filter_index(df_drug_weekly, "2018 W42" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Sales",
    title = "ARIMA Forecasts for weekly Drug Sales (for test data)"
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

## ARIMA Models

# Fit the models
arima_fit <- train |>
  model(ARIMA(Sales))

arima_fit

# Generate forecasts for 52 weeks
arima_fc <- arima_fit |> forecast(h = 52)

# Plot forecasts against actual values
arima_fc |>
  autoplot(test, level = NULL) +
  autolayer(
    filter_index(df_drug_weekly, "2018 W42" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Sales",
    title = "ARIMA Forecasts for weekly Drug Sales (for test data)"
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

## ETS Models

# Fit the models
ets_fit <- train |>
  model(ETS(Sales))

ets_fit

# Generate forecasts for 52 weeks
ets_fc <- ets_fit |> forecast(h = 52)

# Plot forecasts against actual values
ets_fc |>
  autoplot(test, level = NULL) +
  autolayer(
    filter_index(df_drug_weekly, "2018 W42" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Sales",
    title = "ETS Forecasts for weekly Drug Sales (for test data)"
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

## Prophet Models

# Fit the models
prophet_fit <- train |>
  model(prophet(Sales))

prophet_fit

# Generate forecasts for 52 weeks
prophet_fc <- prophet_fit |> forecast(h = 52)

# Plot forecasts against actual values
prophet_fc |>
  autoplot(test, level = NULL) +
  autolayer(
    filter_index(df_drug_weekly, "2018 W42" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Sales",
    title = "Prophet Forecasts for weekly Drug Sales (for test data)"
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

## comparison
rmse_all <- rbind(base_rmse, arima_rmse, ets_rmse, prophet_rmse)
rmse_all

mape_all <- rbind(base_mape, arima_mape, ets_mape, prophet_mape)
mape_all


