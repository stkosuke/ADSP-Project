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
  select(-datum) |> 
  as_tsibble(index = Datetime)

df_drug_daily <- df_drug_daily |> 
  mutate(Date = mdy(datum)) |>
  select(-datum) |> 
  as_tsibble(index = Date)

df_drug_weekly <- df_drug_weekly |> 
  mutate(Week = yearweek(datum)) |>
  select(-datum) |> 
  as_tsibble(index = Week)

df_drug_monthly <- df_drug_monthly |> 
  mutate(Month = yearmonth(datum)) |>
  select(-datum) |> 
  as_tsibble(index = Month)
  

