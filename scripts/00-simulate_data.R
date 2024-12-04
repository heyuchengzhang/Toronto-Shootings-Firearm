#### Preamble ####
# Purpose: Simulates Data
# Author: Heyucheng Zhang
# Date: 30 November 2024
# Contact: heyucheng.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Other Information: Code is appropriately styled using styler


#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
# Simulate raw data for shootings

# Set a seed for reproducible results
set.seed(123)

# Define simulation parameters
years <- 2004:2023
neighbourhoods <- c(
  "Glenfield-Jane Heights (25)", "Black Creek (24)",
  "Mount Olive-Silverstone-Jamestown (2)", "West Humber-Clairville (1)",
  "York University Heights (27)", "Englemount-Lawrence (32)",
  "Humber Summit (21)", "Yorkdale-Glen Park (31)",
  "Malvern East (146)", "Regent Park (72)"
)
num_neighbourhoods <- length(neighbourhoods)

# Generate random dates
dates <- seq.Date(
  from = as.Date("2004-01-01"),
  to = as.Date("2023-12-31"),
  by = "day"
)
num_incidents <- 5000
random_dates <- sample(dates, num_incidents, replace = TRUE)

# Generate random injuries and deaths
random_injuries <- rpois(num_incidents, lambda = 1)
random_deaths <- rbinom(num_incidents, size = 1, prob = 0.1)

# Generate random neighbourhoods
random_neighbourhoods <- sample(neighbourhoods, num_incidents, replace = TRUE)

# Combine into raw data
simulated_raw_data <- tibble(
  REPORT_DATE = random_dates,
  NEIGHBOURHOOD_158 = random_neighbourhoods,
  INJURIES = random_injuries,
  DEATH = random_deaths
) |>
  mutate(REPORT_YEAR = year(REPORT_DATE), REPORT_MONTH = month(REPORT_DATE, label = TRUE, abbr = FALSE))

#### Clean data ####

# Select columns for analysis
simulated_cleaned_data <- simulated_raw_data |>
  dplyr::select(REPORT_DATE, NEIGHBOURHOOD_158, INJURIES, DEATH)

# Calculate shootings and firearm-related injuries/deaths per neighbourhood
simulated_neighbourhood_counts <- simulated_raw_data |>
  filter(NEIGHBOURHOOD_158 != "NSA") |>
  group_by(NEIGHBOURHOOD_158) |>
  summarise(
    Total_Shootings = n(),
    Total_Injuries = sum(INJURIES),
    Total_Deaths = sum(DEATH)
  ) |>
  arrange(desc(Total_Deaths))

# Calculate yearly shootings and firearm-related injuries/deaths
simulated_yearly_counts <- simulated_raw_data |>
  group_by(REPORT_YEAR) |>
  summarise(
    Total_Shootings = n(),
    Total_Injuries = sum(INJURIES),
    Total_Deaths = sum(DEATH)
  )

# Calculate monthly shootings and firearm-related injuries/deaths
month_map <- setNames(1:12, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
simulated_monthly_counts <- simulated_raw_data |>
  mutate(Month_Number = month_map[REPORT_MONTH], Month = REPORT_MONTH) |>
  group_by(REPORT_YEAR, Month, Month_Number) |>
  summarise(
    Shootings = n(),
    Total_Injuries = sum(INJURIES),
    Total_Deaths = sum(DEATH),
    .groups = "drop"
  ) |>
  arrange(REPORT_YEAR, Month_Number) |>
  dplyr::select(-Month_Number)

#### Save data ####

# Save cleaned and aggregated data
write_csv(simulated_cleaned_data, "data/00-simulated_data/simulated_cleaned_data.csv")
write_csv(simulated_neighbourhood_counts, "data/00-simulated_data/simulated_neighbourhood_counts.csv")
write_csv(simulated_yearly_counts, "data/00-simulated_data/simulated_yearly_counts.csv")
write_csv(simulated_monthly_counts, "data/00-simulated_data/simulated_monthly_counts.csv")
