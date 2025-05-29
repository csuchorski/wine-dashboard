library(shiny)
library(plotly)
library(dplyr)
library(bslib)
library(fontawesome)
library(flexdashboard)

function(input, output, session) {
  wine_data <- read.csv("data/XWines_Full_100K_wines.csv") |>
    select(-WineID, -Code, -RegionID, -WineryID, -Website)
  wine_types <- sort(unique(wine_data$Type))
  
  updateCheckboxGroupInput(
    session = session,
    inputId = "checkGroupTypes",
    choices = wine_types,
    selected = wine_types
  )
  
  filtered_data <- reactive({
    req(input$checkGroupTypes)
    wine_data |> filter(Type %in% input$checkGroupTypes)
  })
  
  clicked_country <- reactive({
    event <- event_data("plotly_click", source="map")
    if (!is.null(event)) {
      event$key
    } else {
      NULL
    }
  })
  
  output$map_plot <- renderPlotly({
    df <- filtered_data() |>
      group_by(Country) |>
      summarise(avg_ABV = mean(ABV, na.rm = TRUE))
    
    plot_geo(df, source="map") |>
      add_trace(
        locations = ~Country,
        locationmode = 'country names',
        z = ~avg_ABV,
        text = ~Country,
        key = ~Country,
        colors = "Reds"
      ) |>
      layout(title = "Average Wine ABV by Country")
  })
  
  output$wine_table <- renderDataTable({
    data <- filtered_data()
    country <- clicked_country()
    
    if (!is.null(country)) {
      data <- data |> filter(Country == country)
    }
    
    data
    })
  
  wine_count <- reactive({
    nrow(filtered_data())
  })
  

  output$wine_count_box <- renderUI({
    value_box(
      title = "Total Wines",
      value = format(wine_count(), big.mark = ","),
      showcase = if (wine_count() > 50000 ) icon("wine-glass") else icon("wine-glass-empty"),
      showcase_layout = "left center",
      theme = if (wine_count() > 50000) "success" else "warning"
    )
  })
  
  max_abv <- {max(wine_data$ABV)}
  mean_abv <- reactive({mean(filtered_data()$ABV)})
  
  
  output$ABV_gauge <- renderGauge({
    gauge(
      value = mean_abv(), 
      min = 0, 
      max = max_abv, 
      symbol = '%',
      sectors = gaugeSectors(
        danger = c(0.8 * max_abv, max_abv), 
        warning = c(0.3 * max_abv, 0.6 * max_abv), 
        success  = c(0, 0.3 * max_abv)
      )
    )
  })
  
  
}
