library(shiny)
library(plotly)
library(dplyr)

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
}
