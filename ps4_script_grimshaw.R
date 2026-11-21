# EC 422 - Problem Set 4
# Meredith Grimshaw

# Load required packages
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(lubridate)
library(BVAR)

# 2.1a
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date),
            sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# 2.1b
liquor |>
  autoplot(sales) +
  labs(
    title = "Monthly Retail Sales at Beer, Wine, and Liquor Stores",
    x = "Year",
    y = "Sales (Millions of Dollars)"
  )

liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Seasonal Pattern in Log Liquor Sales",
    x = "Month",
    y = "Log(Sales)"
  )

# 2.2a
liquor_fit <- liquor |>
  model(
    arima = ARIMA(log(sales)),
    ets = ETS(log(sales))
  )

liquor_fit |> select(arima) |> report()

liquor_fit |> select(ets) |> report()

# 2.2b
liquor_fit |>
  select(arima) |>
  gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

# 2.3a
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(
    snaive = SNAIVE(sales),
    ets = ETS(log(sales)),
    arima = ARIMA(log(sales))
  )
saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

# 2.3b
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)
liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
liquor_cv_fc |>
  summarise(
    first_forecast = min(Month),
    last_forecast = max(Month)
  )
liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  knitr::kable(digits = 2)

# 2.4
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(
    as_tibble(liquor) |>
      select(Month, actual = sales),
    by = "Month"
  ) |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(
  liquor_errs$arima,
  liquor_errs$ets,
  h = 1,
  power = 2
)

forecast::dm.test(
  liquor_errs$arima,
  liquor_errs$snaive,
  h = 1,
  power = 2
)

# 3a
liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

# 3b
cor(liquor_errs$arima, liquor_errs$ets)

# 4.1a
qd_raw <- read_csv(
  "data/fredqd_2026-09.csv",
  show_col_types = FALSE
) |>
  slice(-(1:2))

# 4.1b
growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(
    date,
    gdp = growth(GDPC1),
    cons = growth(PCECC96),
    inv = growth(GPDIC1),
    emp = growth(PAYEMS),
    infl = growth(GDPCTPI),
    unrate = difference(UNRATE),
    ffr = difference(FEDFUNDS),
    gs10 = difference(GS10)
  ) |>
  filter(
    date >= as.Date("1960-03-01"),
    date <= as.Date("2019-12-01")
  )
y <- qd |>
  select(-date) |>
  as.matrix()
nrow(y)
head(y)

# 4.1c
N <- nrow(y)

origins <- 120:(N - 1)

actual <- y[121:N, ]

# 4.2
var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)
  M <- ncol(z)
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))
  x %*% B
}
fc_var1 <- NULL
fc_var4 <- NULL

for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)