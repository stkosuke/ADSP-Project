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
# In the following, weekly data is used to perform Moving average smoothing and STL Decomposition.

## Moving average smoothing

# add the 52-MA and 52x2-MA to the data set
df_drug_monthly_fixed <- df_drug_monthly_fixed |> 
  group_by(Drug) |> 
  mutate(
    MA_12 = slider::slide_dbl(Sales, mean, 
                              .before = 5, .after = 6, .complete = TRUE),
    MA_12x2 = slider::slide_dbl(MA_12, mean, 
                                .before = 1, .after = 0, .complete = TRUE)
  ) |> 
  ungroup()

# plot 52x2-MA
df_drug_monthly_fixed |> 
  autoplot(Sales) +
  geom_line(aes(y = MA_12x2, color = "12-month Moving Average"), size =1) +
  labs(title = "Weekly Sales of Drugs with 12-month Moving Average") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) +
  theme(legend.position = "bottom")


# <span style="color:red;">Add some implications</span> from the moving average smoothing.

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

STL_dcmp(df_drug_monthly_fixed, "Drug", "R06")

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
df_drug_weekly |> 
  ACF(Sales) |> 
  autoplot() +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) 

# PACF plot
df_drug_weekly |> 
  PACF(Sales) |> 
  autoplot() +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) 

# <span style="color:red;">Add some implications</span> .

## d. ADF test

# function for ADF test
ADF_test <- function(drug_code) {
  print(drug_code)
  df_drug_weekly |>
    filter(Drug == drug_code) |>
    pull(Sales) |> 
    tseries::adf.test() |> 
    print()
}

# apply ADF test to all unique drug code
for (drug_code in unique(df_drug_weekly$Drug)) {
  ADF_test(drug_code)
}

# <span style="color:red;">Add some implications</span> .

# 3. Forecasting

## Forecasting
# use 52 weeks as a test set  
train <- df_drug_weekly |>
  filter_index("2014 W01" ~ "2018 W41")

test <- df_drug_weekly |>
  filter_index("2018 W42" ~ "2019 W41")


## a. Fit the models
### 1. Baseline Models (Mean, Naïve and Seasonal naïve)

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
    title = "Base Forecasts for weekly Drug Sales (for test data)"
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

### 3. ETS Models 

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

### 4. Prophet Models 

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


## b. Model Comparison

## comparison
rmse_all <- rbind(base_rmse, arima_rmse, ets_rmse, prophet_rmse)
rmse_all

mape_all <- rbind(base_mape, arima_mape, ets_mape, prophet_mape)
mape_all
