#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output, session) {
  wine_data <- read.csv("data/XWines_Full_100K_wines.csv")
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
  
  output$map_plot <- renderPlotly({
    df <- filtered_data() |>
      group_by(Country) |>
      summarise(avg_ABV = mean(ABV, na.rm = TRUE))
    
    plot_geo(df) |>
      add_trace(
        locations = ~Country,
        locationmode = 'country names',
        z = ~avg_ABV,
        text = ~Country,
        colors = "Reds"
      ) |>
      layout(title = "Average Wine ABV by Country")
  })
}
