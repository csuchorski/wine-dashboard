library(shiny)
library(plotly)
library(dplyr)
library(bslib)
library(fontawesome)
library(flexdashboard)
library(ggplot2)
library(dplyr)
library(viridis)
library(arrow)

function(input, output, session) {
  wine_data <- read_parquet("data/wine_data.parquet") |>
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
  
  
  
  output$summary_hist_abv <- renderPlotly({
    plot_ly(
      data = filtered_data(),
      x = ~ABV,
      type = "histogram",
      marker = list(color = "#2C3E50")
    ) %>%
      layout(
        title = "Distribution of ABV",
        xaxis = list(title = "ABV (%)"),
        yaxis = list(title = "Count")
      )
  })
  
  output$summary_violin_type <- renderPlotly({
    plot_ly(
      data = filtered_data(),
      y = ~ABV,
      x = ~Type,
      split = ~Type,
      type = "violin",
      box = list(visible = TRUE),
      meanline = list(visible = TRUE)
    ) %>%
      layout(
        title = "ABV by Wine Type",
        xaxis = list(title = "Type"),
        yaxis = list(title = "ABV (%)")
      )
  })
  
  output$summary_heatmap_body_acidity <- renderPlotly({
    # Aggregate counts
    agg <- filtered_data() %>%
      dplyr::count(Body, Acidity)
    
    # Create a matrix for heatmap: rows = Body, columns = Acidity
    bodies <- sort(unique(agg$Body))
    acidities <- sort(unique(agg$Acidity))
    
    # Create a matrix of counts
    count_matrix <- matrix(
      0, 
      nrow = length(bodies), 
      ncol = length(acidities), 
      dimnames = list(bodies, acidities)
    )
    for (i in seq_len(nrow(agg))) {
      row <- agg[i, ]
      count_matrix[as.character(row$Body), as.character(row$Acidity)] <- row$n
    }
    
    plot_ly(
      x = acidities,
      y = bodies,
      z = count_matrix,
      type = "heatmap",
      colorscale = "Viridis"
    ) %>%
      layout(
        title = "Wine Count by Body and Acidity",
        xaxis = list(title = "Acidity"),
        yaxis = list(title = "Body")
      )
  })
  
  
  
  output$summary_bar_grapes <- renderPlotly({
    grapes_raw <- filtered_data()$Grapes
    grapes_clean <- gsub("\\[|\\]|'", "", grapes_raw)
    grapes <- unlist(strsplit(grapes_clean, ","))
    grapes <- trimws(grapes)
    grapes <- grapes[grapes != ""]
    grape_counts <- sort(table(grapes), decreasing = TRUE)
    top_grapes <- head(grape_counts, 10)
    df <- data.frame(
      grape = names(top_grapes),
      count = as.numeric(top_grapes)
    )
    df$grape <- factor(df$grape, levels = df$grape)
    plot_ly(
      data = df,
      x = ~grape,
      y = ~count,
      type = "bar",
      marker = list(color = "#18BC9C")
    ) %>%
      layout(
        title = "Top 10 Most Common Grapes",
        xaxis = list(title = "Grape", categoryorder = "array", categoryarray = df$grape),
        yaxis = list(title = "Count")
      )
  })
  
  
  
  output$summary_bar_country <- renderPlotly({
    country_counts <- filtered_data() %>%
      group_by(Country) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(10)
    
    country_counts$Country <- factor(country_counts$Country, levels = country_counts$Country)
    
    plot_ly(
      data = country_counts,
      x = ~Country,
      y = ~n,
      type = "bar",
      marker = list(color = "#E74C3C")
    ) %>%
      layout(
        title = "Top 10 Countries by Number of Wines",
        xaxis = list(title = "Country", categoryorder = "array", categoryarray = country_counts$Country),
        yaxis = list(title = "Number of Wines")
      )
  })
  
  
  
}
