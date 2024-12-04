#### Preamble ####
# Purpose: The Shiny web application about Toronto Shooting Incidents Explorer
# Author: Heyucheng Zhang
# Date: 30 November 2024
# Contact: heyucheng.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 01-download_data.R
# Other Information: Code is appropriately styled using styler


#### Workspace setup ####
library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(leaflet)
library(here)

#### Shiny web application ####
# Define UI
ui <- fluidPage(
  titlePanel("Toronto Shooting Incidents Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yearInput", "Select Year:",
        choices = c("All", as.character(2004:2024))
      ),
      selectInput("neighbourhoodInput", "Select Neighbourhood:",
        choices = c("All"), # Placeholder, updated dynamically
        multiple = TRUE
      ),
      actionButton("update", "Update View")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("dataView")),
        tabPanel("Map", leafletOutput("mapView"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Load and preprocess data
  shooting_data <- read_csv(here("data/01-raw_data/raw_data.csv")) |>
    mutate(OCC_DATE = as.Date(OCC_DATE)) |>
    mutate(YEAR = format(OCC_DATE, "%Y")) |>
    filter(LONG_WGS84 != 0 & LAT_WGS84 != 0)

  # Update neighbourhood choices dynamically
  observe({
    neighbourhoods <- unique(shooting_data$NEIGHBOURHOOD_158)
    updateSelectInput(session, "neighbourhoodInput", choices = c("All", neighbourhoods))
  })

  # Reactive filtered data
  filtered_data <- eventReactive(input$update, {
    data <- shooting_data
    if (input$yearInput != "All") {
      data <- data |>
        filter(YEAR == input$yearInput)
    }
    if ("All" %in% input$neighbourhoodInput) {
      data
    } else {
      data |>
        filter(NEIGHBOURHOOD_158 %in% input$neighbourhoodInput)
    }
  })

  # Render data table
  output$dataView <- renderTable({
    filtered_data()
  })

  # Render map
  output$mapView <- renderLeaflet({
    data <- filtered_data()
    if (nrow(data) > 0) {
      leaflet(data) |>
        addTiles() |>
        addCircleMarkers(
          ~LONG_WGS84, ~LAT_WGS84,
          popup = ~ paste("Date:", OCC_DATE, "<br>Injuries:", INJURIES, "<br>Deaths:", DEATH),
          radius = 4,
          fillColor = ifelse(data$DEATH > 0, "red", "blue"),
          color = NULL,
          fillOpacity = 0.7
        )
    } else {
      leaflet() |>
        addTiles()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
