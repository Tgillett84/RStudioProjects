library(tidyverse)
library(Metrics)
library(xlsx)

# In my RStudio project I have a folder called `data` into
# which I put your excel file
# we can read it with the `readxl` package
# which is automatically installed with the tidyverse
# but not loaded by default:
raw_data <- readxl::read_xlsx("./data/InventoryAccountingWeek_1_exercise_1.7.xlsx",
                              sheet = 2)

# The first thing we do is transform the data into a tidy
# represenation to make it easier to work with:
# see <https://r4ds.had.co.nz/tidy-data.html>
# or my lecture <https://jmbuhr.de/dataintro/tidy-data.html> if you prefer a video
# I also convert the months to a factor so that we don't loose the order
data <- raw_data |>
  pivot_longer(-Month, names_to = "month", values_to = "value") |>
  pivot_wider(names_from = Month, values_from = value) |>
  mutate(month = factor(month))

# The Mean Average Error:
data |>
  summarise(mae = mae(actual = Demand, predicted = Forecast))

# subtract the mean forecast error from each month's forecast to make it unbiased.
unbiased_data <- data |>
  mutate(Forecast = Forecast - mae(Demand, Forecast))

MAD <- function(x) {
  sum(abs(x - mean(x))) / length(x)
}

forecast_mad <- function(x,y) {
  sum(abs(x - y)) / length(x)
}

unbiased_data |>
  summarise(
    mad = forecast_mad(Forecast, Demand)
  )

unbiased_data |>
  summarise(
    mad = mean(abs(Forecast - Demand))
  )

