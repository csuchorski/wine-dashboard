#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(bslib)
library(fontawesome)


red_light_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#d7263d",
  danger = "#ff0000"
)

page_fillable(
  theme = red_light_theme,
  page_sidebar(
    sidebar = sidebar(
      tags$div(
        style = "text-align:center; margin-bottom: 20px;",
        img(src = "logo.jpg", height = "80px")
      ),
      checkboxGroupInput(
        inputId = "checkGroupTypes",
        label = h3("Wine Types"),
        choices = NULL
      )
    ),
    navset_tab(
      nav_panel("About",
                card(
                  tags$div(
                    style = "text-align:center; margin-bottom: 20px;",
                    img(src = "logo.jpg", height = "80px")
                  ),
                  
                  tags$h2(style = "text-align:center; color:#2C3E50; margin-bottom: 10px;", "Wine Explorer"),
                  
                  tags$p(
                    style = "font-size: 18px; text-align: center;",
                    "Wine Explorer is an interactive Shiny app designed to help you discover, analyze, and compare wines from around the world."
                  ),
                  
                  tags$hr(),
                  
                  tags$h4("Key Features"),
                  tags$ul(
                    tags$li("Interactive map showing wine count by country"),
                    tags$li("Filter wines by type"),
                    tags$li("Explore detailed wine data in tables and charts"),
                    tags$li("Find the perfect wine for you"),
                  ),
                  
                  tags$hr(),
                  
                  tags$h4("Data Sources"),
                  tags$p(
                    "The data used in this app comes from the ",
                    tags$a(href = "https://github.com/rogerioxavier/X-Wines", "text"),
                  ),
                  
                  tags$hr(),
                  
                  tags$h4("How to Use"),
                  tags$ol(
                    tags$li("Select wine types in the sidebar to filter the dataset."),
                    tags$li("Navigate between tabs to explore maps, tables, and visualizations."),
                    tags$li("Click on countries in the map to focus on wines from that region.")
                  ),
                  
                  tags$hr(),
                  
                  tags$h4("Credits & Contact"),
                  tags$p("Created by:"),
                  tags$ul(
                    tags$li(
                      "Cezary Suchorski 123456",
                    ),
                    tags$li(
                      "Michał Żarnowski 160277",
                    )
                  ),
                  
                  tags$hr(),
                  
                  tags$p(
                    style = "font-size: 12px; color: #888;",
                    "This app was built with R, Shiny, and Plotly. Last updated: May 2025."
                  )
                )
      )
      ,
      nav_panel("Map & table", (
        layout_column_wrap(
          width = 1,
          card(
            plotlyOutput("map_plot"),
            max_height = "520px"
          ),

          card(
            dataTableOutput("wine_table"),
            max_height = "520px"
          )
        )
      )),
      nav_panel("Exploration",
                layout_columns(
                  card(uiOutput("wine_count_box")),
                  card(plotlyOutput("summary_hist_abv")),
                  card(plotlyOutput("summary_violin_type")),
                  card(plotlyOutput("summary_heatmap_body_acidity")),
                  card(plotlyOutput("summary_bar_grapes")),
                  card(plotlyOutput("summary_bar_country")),
                  col_widths = c(6,6)
                )
      ),
      nav_panel("Some more...",
                card(plotlyOutput("pairing_heatmap", height = "900px"))
      )
      )
    )
)
