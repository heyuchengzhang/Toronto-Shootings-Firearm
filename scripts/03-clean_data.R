#### Preamble ####
# Purpose: Cleans the raw data
# Author: Heyucheng Zhang
# Date: 30 November 2024
# Contact: heyucheng.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 00-simulate_data.R and 02-download_data.R
# Other Information: Code is appropriately styled using styler

#### Workspace setup ####
library(tidyverse)
library(arrow)
library(dplyr)
library(sf)

#### Clean data ####
# Read data
raw_data <- read_csv("data/01-raw_data/raw_data.csv")

# Select columns for analysis
cleaned_data <- raw_data |>
  dplyr::select(OCC_DATE, DEATH, INJURIES, NEIGHBOURHOOD_158, LONG_WGS84, LAT_WGS84)

# Filter out data with invalid coordinates
filtered_data <- raw_data |>
  filter(LONG_WGS84 != 0 & LAT_WGS84 != 0)
locations <- st_as_sf(filtered_data, coords = c("LONG_WGS84", "LAT_WGS84"), crs = 4326)

# Calculate shootings and firearm injuries/deaths per neighbourhood
neighbourhood_counts <- raw_data |>
  group_by(NEIGHBOURHOOD_158) |>
  summarise(
    Total_Shootings = n(),
    Total_Injuries = sum(INJURIES, na.rm = TRUE),
    Total_Deaths = sum(!is.na(DEATH))
  ) |>
  arrange(desc(Total_Shootings))

# Map months to numeric values for sorting
month_map <- c(
  "January" = 1, "February" = 2, "March" = 3, "April" = 4,
  "May" = 5, "June" = 6, "July" = 7, "August" = 8,
  "September" = 9, "October" = 10, "November" = 11, "December" = 12
)

# Calculate total shootings and firearm injuries/deaths per month
monthly_counts <- raw_data |>
  mutate(Month_Number = month_map[OCC_MONTH]) |>
  group_by(OCC_YEAR, OCC_MONTH, Month_Number) |>
  summarise(
    Total_Shootings = n(),
    Total_Injuries = sum(INJURIES, na.rm = TRUE),
    Total_Deaths = sum(!is.na(DEATH)),
    .groups = "drop"
  ) |>
  arrange(OCC_YEAR, Month_Number) |>
  select(-Month_Number)

# Calculate total shootings and firearm injuries/deaths per year
yearly_counts <- raw_data |>
  group_by(OCC_YEAR) |>
  summarise(
    Total_Shootings = n(),
    Total_Injuries = sum(INJURIES, na.rm = TRUE),
    Total_Deaths = sum(!is.na(DEATH))
  ) |>
  arrange(OCC_YEAR)

#### Save data ####
write_parquet(cleaned_data, "data/02-analysis_data/cleaned_data.parquet")
write_parquet(neighbourhood_counts, "data/02-analysis_data/neighbourhood_counts.parquet")
write_parquet(yearly_counts, "data/02-analysis_data/yearly_counts.parquet")
write_parquet(monthly_counts, "data/02-analysis_data/monthly_counts.parquet")
st_write(locations, "data/02-analysis_data/locations.geojson", driver = "GeoJSON")
