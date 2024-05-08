library(zoo)

### monthly data

# time plot
df_drug_monthly |> 
  autoplot(Sales) + 
  labs(title = "Monthly sales of drugs") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) +
  theme(legend.position = "none")

# Seasonal subseries plots
df_drug_monthly |> 
  gg_subseries(Sales) + 
  labs(title = "Monthly sales of drugs") +
  theme(legend.position = "none")

df_drug_monthly |> 
  ggplot(aes(x = as.factor(month(Month)), y = Sales)) +
  geom_boxplot() +
  facet_wrap(~ Drug, scales = "free_y", ncol = 2)

df_drug_monthly |> 
  gg_season(Sales)

### weekly data

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

### daily data

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
  ggplot(aes(x = as.factor(month(Date)), y = Sales)) +
  geom_boxplot() +
  facet_wrap(~ Drug, scales = "free_y", ncol = 2)

df_drug_daily |> 
  gg_season(Sales, period = "week")

df_drug_daily |> 
  gg_season(Sales, period = "month")

df_drug_daily |> 
  gg_season(Sales, period = "year")

### hourly data

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

## Moving average smoothing

# add the 52-MA and 52x2-MA to the data set
df_drug_weekly <- df_drug_weekly |> 
  group_by(Drug) |> 
  mutate(
    MA_52 = slider::slide_dbl(Sales, mean, 
                              .before = 25, .after = 26, .complete = TRUE),
    MA_52x2 = slider::slide_dbl(MA_52, mean, 
                              .before = 1, .after = 0, .complete = TRUE)
    ) |> 
  ungroup()

# plot the 52-MA and 52x2-MA
df_drug_weekly |> 
  autoplot(Sales) +
  geom_line(aes(y = MA_52x2, color = "52-week Moving Average"), size =1) +
  labs(title = "Weekly Sales of Drugs with 52-week Moving Average") +
  facet_wrap(vars(Drug), scales = "free_y", ncol = 2) +
  theme(legend.position = "bottom")

## Decomposition

# function for Classical additive decomposition
CL_dcmp <- function(drug_code) {
  df_drug_weekly |>
    filter(Drug == drug_code) |>
    model(
      classical_decomposition(Sales, type = "additive")
    ) |>
    components() |>
    autoplot() +
    ggtitle(paste("Classical additive decomposition for Drug:", drug_code))
}

# apply Classical additive to all unique drug code
lapply(unique(df_drug_weekly$Drug), CL_dcmp)

# function for STL decomposition
STL_dcmp <- function(drug_code) {
  df_drug_weekly |>
    filter(Drug == drug_code) |>
    model(
      STL(Sales ~ trend(window = 7) +
            season(window = "periodic"),
          robust = TRUE)
    ) |>
    components() |>
    autoplot() +
    ggtitle(paste("STL Decomposition for Drug:", drug_code))
}

# apply STL decomposition to all unique drug code
lapply(unique(df_drug_weekly$Drug), STL_dcmp)

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

## ADF test

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