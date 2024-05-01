# library packages
library(readr)
library(fpp3)

# load the data
repo_path <- "https://raw.githubusercontent.com/stkosuke/ADSP-Project/main"

df_drug_hourly <- read_csv(file.path(repo_path, "saleshourly.csv"))
df_drug_daily <- read_csv(file.path(repo_path, "salesdaily.csv"))
df_drug_weekly <- read_csv(file.path(repo_path, "salesweekly.csv"))
df_drug_monthly <- read_csv(file.path(repo_path, "salesmonthly.csv"))

# convert them to tsibble object
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

