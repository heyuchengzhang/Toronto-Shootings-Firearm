#### Preamble ####
# Purpose: Tests the analysis data
# Author: Heyucheng Zhang
# Date: 30 November 2024
# Contact: heyucheng.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 00-simulate_data.R, 02-download_data.R and 03-clean_data.R
# Other Information: Code is appropriately styled using styler


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

# Read data
cleaned_data <- read_parquet("data/02-analysis_data/cleaned_data.parquet")
neighbourhood_counts <- read_parquet("data/02-analysis_data/neighbourhood_counts.parquet")
yearly_counts <- read_parquet("data/02-analysis_data/yearly_counts.parquet")
monthly_counts <- read_parquet("data/02-analysis_data/monthly_counts.parquet")


#### Test data ####
# Test that the cleaned data contains the expected columns
test_that("cleaned_data has the correct columns", {
  expect_true(all(c("OCC_DATE", "DEATH", "INJURIES", "NEIGHBOURHOOD_158", "LONG_WGS84", "LAT_WGS84") %in% colnames(cleaned_data)))
})

# Test that neighbourhood_counts contains Total_Injuries and Total_Deaths columns
test_that("neighbourhood_counts has the correct columns", {
  expect_true(all(c("NEIGHBOURHOOD_158", "Total_Injuries", "Total_Deaths") %in% colnames(neighbourhood_counts)))
})

# Test that neighbourhood_counts is sorted by Total_Deaths in descending order
test_that("neighbourhood_counts is sorted by Total_Deaths descending", {
  expect_true(all(neighbourhood_counts$Total_Deaths == sort(neighbourhood_counts$Total_Deaths, decreasing = TRUE)))
})

# Test that yearly_counts contains Total_Injuries and Total_Deaths columns
test_that("yearly_counts has the correct columns", {
  expect_true(all(c("OCC_YEAR", "Total_Injuries", "Total_Deaths") %in% colnames(yearly_counts)))
})

# Test that yearly_counts is sorted by OCC_YEAR
test_that("yearly_counts is sorted by OCC_YEAR", {
  expect_true(all(yearly_counts$OCC_YEAR == sort(yearly_counts$OCC_YEAR)))
})

# Test that monthly_counts contains Total_Injuries and Total_Deaths columns
test_that("monthly_counts has the correct columns", {
  expect_true(all(c("OCC_YEAR", "OCC_MONTH", "Total_Injuries", "Total_Deaths") %in% colnames(monthly_counts)))
})

# Test that there are no missing values in the datasets
datasets <- list(neighbourhood_counts, yearly_counts, monthly_counts)
test_that("datasets have no missing values", {
  for (dataset in datasets) {
    expect_true(all(!is.na(dataset)))
  }
})

# Test that neighbourhood_counts does not contain duplicate neighbourhoods
test_that("neighbourhood_counts has unique neighbourhoods", {
  expect_equal(nrow(neighbourhood_counts), length(unique(neighbourhood_counts$NEIGHBOURHOOD_158)))
})

# Test that the datasets were saved in the expected format
test_that("datasets are saved as parquet files", {
  expect_true(file.exists("data/02-analysis_data/cleaned_data.parquet"))
  expect_true(file.exists("data/02-analysis_data/neighbourhood_counts.parquet"))
  expect_true(file.exists("data/02-analysis_data/yearly_counts.parquet"))
  expect_true(file.exists("data/02-analysis_data/monthly_counts.parquet"))
})

# Test that Total_Injuries and Total_Deaths are non-negative in all datasets
test_that("All counts of injuries and deaths are non-negative", {
  expect_true(all(neighbourhood_counts$Total_Injuries >= 0 & neighbourhood_counts$Total_Deaths >= 0))
  expect_true(all(yearly_counts$Total_Injuries >= 0 & yearly_counts$Total_Deaths >= 0))
  expect_true(all(monthly_counts$Total_Injuries >= 0 & monthly_counts$Total_Deaths >= 0))
})

# Test that the number of neighbourhoods in neighbourhood_counts matches unique values in cleaned_data
test_that("neighbourhood_counts matches the unique neighbourhoods in cleaned_data", {
  unique_neighbourhoods <- unique(cleaned_data$NEIGHBOURHOOD_158)
  expect_true(all(unique_neighbourhoods %in% neighbourhood_counts$NEIGHBOURHOOD_158))
})

# Test that Total_Deaths in neighbourhood_counts is consistent with cleaned_data
test_that("neighbourhood_counts Total_Deaths matches data in cleaned_data", {
  computed_totals <- cleaned_data %>%
    group_by(NEIGHBOURHOOD_158) %>%
    summarise(Total_Deaths = sum(!is.na(DEATH), na.rm = TRUE), .groups = "drop")
  merged <- merge(neighbourhood_counts, computed_totals, by = "NEIGHBOURHOOD_158", suffixes = c("_expected", "_computed"))
  expect_equal(merged$Total_Deaths_expected, merged$Total_Deaths_computed)
})

# Test that all columns in datasets have no empty strings
test_that("No empty strings in key columns", {
  datasets <- list(cleaned_data, neighbourhood_counts, yearly_counts, monthly_counts)
  for (dataset in datasets) {
    expect_true(all(sapply(dataset, function(col) if (is.character(col)) all(col != "") else TRUE)))
  }
})
