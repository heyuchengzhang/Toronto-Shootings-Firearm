#### Preamble ####
# Purpose: Downloads and saves Data
# Author: Heyucheng Zhang
# Date: 30 November 2024
# Contact: heyucheng.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 00-simulate_data.R
# Other Information: Code is appropriately styled using styler


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(sf)

#### Download map data ####
# Get package
package <- show_package("neighbourhoods")

# Get all resources for this package
resources <- list_package_resources("neighbourhoods")

# Filter the resources
datastore_resources <- filter(resources, tolower(format) %in% c("geojson"))

# Load the first datastore resource as a sample
toronto_map_data <- filter(datastore_resources, row_number() == 1) %>% get_resource()

#### Save Map data ####
st_write(toronto_map_data, "data/01-raw_data/toronto_map_data.geojson", driver = "GeoJSON")


#### Download Shootings & Firearm Discharges Raw Data ####
# Get package
package <- show_package("4bc5511d-0ecf-487a-9214-7b2359ad8f61")

# Get all resources for this package
resources <- list_package_resources("4bc5511d-0ecf-487a-9214-7b2359ad8f61")

# Filter the resources
datastore_resources <- filter(resources, tolower(format) %in% c("csv", "geojson"))

# Load the first datastore resource as a sample
raw_data <- filter(datastore_resources, row_number() == 1) %>% get_resource()

#### Save raw data ####
write_csv(raw_data, "data/01-raw_data/raw_data.csv")
