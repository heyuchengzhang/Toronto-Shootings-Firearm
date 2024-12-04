#### Preamble ####
# Purpose: Create model
# Author: Heyucheng Zhang
# Date: 30 November 2024
# Contact: heyucheng.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 00-simulate_data.R, 02-download_data.R, 03-clean_data.R and 04-test_analysis_data.R
# Other Information: Code is appropriately styled using styler


#### Workspace setup ####
library(tidyverse)
library(arrow)

#### Read data ####
yearly_counts <- read_parquet("data/02-analysis_data/yearly_counts.parquet")

# Create time index
yearly_counts$Time_Index <- seq_len(nrow(yearly_counts))

#### Model data for Deaths ####
death_model <- lm(Total_Deaths ~ Time_Index, data = yearly_counts)
summary(death_model)

#### Save Deaths model ####
saveRDS(
  death_model,
  file = "models/death_model.rds"
)

#### Model data for Injuries ####
injury_model <- lm(Total_Injuries ~ Time_Index, data = yearly_counts)
summary(injury_model)

#### Save Injuries model ####
saveRDS(
  injury_model,
  file = "models/injury_model.rds"
)
