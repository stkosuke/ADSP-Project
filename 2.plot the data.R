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
  gg_season(Sales, period = "week")

df_drug_daily |> 
  gg_season(Sales, period = "month")

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
