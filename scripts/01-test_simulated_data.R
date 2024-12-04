#### Preamble ####
# Purpose: Tests the Simulated Data
# Author: Heyucheng Zhang
# Date: 30 November 2024
# Contact: heyucheng.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 00-simulate_data.R
# Other Information: Code is appropriately styled using styler

#### Workspace setup ####
library(tidyverse)
library(testthat)

# Load simulated data
simulated_cleaned_data <- read_csv("data/00-simulated_data/simulated_cleaned_data.csv")
simulated_neighbourhood_counts <- read_csv("data/00-simulated_data/simulated_neighbourhood_counts.csv")
simulated_yearly_counts <- read_csv("data/00-simulated_data/simulated_yearly_counts.csv")
simulated_monthly_counts <- read_csv("data/00-simulated_data/simulated_monthly_counts.csv")

#### Test data ####

# Test that the simulated_cleaned_data contains the expected columns
test_that("simulated_cleaned_data has the correct columns", {
  expect_true(all(c("REPORT_DATE", "NEIGHBOURHOOD_158", "INJURIES", "DEATH") %in% colnames(simulated_cleaned_data)))
})

# Test that the simulated_neighbourhood_counts contains the expected columns
test_that("simulated_neighbourhood_counts has the correct columns", {
  expect_true(all(c("NEIGHBOURHOOD_158", "Total_Shootings", "Total_Injuries", "Total_Deaths") %in% colnames(simulated_neighbourhood_counts)))
})

# Test that the simulated_yearly_counts contains the expected columns
test_that("simulated_yearly_counts has the correct columns", {
  expect_true(all(c("REPORT_YEAR", "Total_Shootings", "Total_Injuries", "Total_Deaths") %in% colnames(simulated_yearly_counts)))
})

# Test that the simulated_monthly_counts contains the expected columns
test_that("simulated_monthly_counts has the correct columns", {
  expect_true(all(c("REPORT_YEAR", "Month", "Shootings", "Total_Injuries", "Total_Deaths") %in% colnames(simulated_monthly_counts)))
})

# Test that the simulated_neighbourhood_counts is sorted by Total_Deaths in descending order
test_that("simulated_neighbourhood_counts is sorted by Total_Deaths descending", {
  expect_true(all(simulated_neighbourhood_counts$Total_Deaths == sort(simulated_neighbourhood_counts$Total_Deaths, decreasing = TRUE)))
})

# Test that there are no missing values in all datasets
datasets <- list(simulated_cleaned_data, simulated_neighbourhood_counts, simulated_yearly_counts, simulated_monthly_counts)
test_that("all datasets have no missing values", {
  for (dataset in datasets) {
    expect_true(all(!is.na(dataset)))
  }
})

# Test that simulated_neighbourhood_counts has unique neighbourhoods
test_that("simulated_neighbourhood_counts has unique neighbourhoods", {
  expect_equal(nrow(simulated_neighbourhood_counts), length(unique(simulated_neighbourhood_counts$NEIGHBOURHOOD_158)))
})

# Test that Total_Shootings, Total_Injuries, and Total_Deaths are non-negative in all datasets
test_that("all counts are non-negative", {
  expect_true(all(simulated_neighbourhood_counts$Total_Shootings >= 0 & simulated_neighbourhood_counts$Total_Injuries >= 0 & simulated_neighbourhood_counts$Total_Deaths >= 0))
  expect_true(all(simulated_yearly_counts$Total_Shootings >= 0 & simulated_yearly_counts$Total_Injuries >= 0 & simulated_yearly_counts$Total_Deaths >= 0))
  expect_true(all(simulated_monthly_counts$Shootings >= 0 & simulated_monthly_counts$Total_Injuries >= 0 & simulated_monthly_counts$Total_Deaths >= 0))
})

# Test that simulated_neighbourhood_counts matches the unique neighbourhoods in simulated_cleaned_data
test_that("simulated_neighbourhood_counts matches unique neighbourhoods in simulated_cleaned_data", {
  unique_neighbourhoods <- unique(simulated_cleaned_data$NEIGHBOURHOOD_158)
  expect_true(all(unique_neighbourhoods %in% simulated_neighbourhood_counts$NEIGHBOURHOOD_158))
})

# Test that Total_Deaths in simulated_neighbourhood_counts matches data in simulated_cleaned_data
test_that("simulated_neighbourhood_counts Total_Deaths matches data in simulated_cleaned_data", {
  computed_totals <- simulated_cleaned_data %>%
    group_by(NEIGHBOURHOOD_158) %>%
    summarise(Total_Deaths = sum(DEATH, na.rm = TRUE), .groups = "drop")
  merged <- merge(simulated_neighbourhood_counts, computed_totals, by = "NEIGHBOURHOOD_158", suffixes = c("_expected", "_computed"))
  expect_equal(merged$Total_Deaths_expected, merged$Total_Deaths_computed)
})

# Test that all date columns in simulated_cleaned_data are valid dates
test_that("REPORT_DATE in simulated_cleaned_data contains valid dates", {
  expect_true(all(!is.na(as.Date(simulated_cleaned_data$REPORT_DATE))))
})

# Test that REPORT_YEAR in simulated_yearly_counts matches years in simulated_cleaned_data
test_that("simulated_yearly_counts REPORT_YEAR matches years in simulated_cleaned_data", {
  unique_years <- unique(simulated_cleaned_data %>% mutate(REPORT_YEAR = year(REPORT_DATE)) %>% pull(REPORT_YEAR))
  expect_true(all(simulated_yearly_counts$REPORT_YEAR %in% unique_years))
})
