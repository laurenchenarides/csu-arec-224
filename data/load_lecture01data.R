# Load required libraries
library(tidyverse)
library(lubridate)

theme_set(theme_minimal())

# Data
df <- readr::read_csv("../data/lecture01data.csv") |>
  mutate(Date = make_date(year = Year, month = Month, day = 1))

# (Optional) scale the secondary axis so lines share a similar range
rng1  <- range(df$NumBusinesses, na.rm = TRUE)
rng2  <- range(c(df$COAgBus, df$CORetailBus), na.rm = TRUE)
scale <- diff(rng1) / diff(rng2)

glimpse(df)

# (Optional) scale the secondary axis so lines share a similar range
rng1  <- range(df$NumBusinesses, na.rm = TRUE)
rng2  <- range(c(df$COAgBus, df$CORetailBus), na.rm = TRUE)
scale <- diff(rng1) / diff(rng2)

# Plot
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = NumBusinesses, color = "NumBusinesses"), linewidth = 1) +
  geom_line(aes(y = COAgBus * scale, color = "COAgBus"), linewidth = 0.9, linetype = "dashed") +
  geom_line(aes(y = CORetailBus * scale, color = "CORetailBus"), linewidth = 0.9, linetype = "dotdash") +
  scale_y_continuous(
    name = "NumBusinesses",
    sec.axis = sec_axis(~ . / scale, name = "COAgBus / CORetailBus")
  ) +
  labs(x = "Month", color = NULL, title = "Businesses in Colorado Over Time") +
  guides(color = guide_legend(override.aes = list(linetype = c("solid","dashed","dotdash"))))