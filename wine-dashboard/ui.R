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

page_fillable(
  theme = bs_theme(bootswatch = "flatly"),
  page_sidebar(
    sidebar = sidebar(
      checkboxGroupInput(
        inputId = "checkGroupTypes",
        label = h3("Wine Types"),
        choices = NULL
      )
    ),
    navset_tab(
      nav_panel("About",
                card(
                  h2("About page")
                )),
      nav_panel("Map & table", (
        layout_column_wrap(
          width = 1,
          card(
            plotlyOutput("map_plot", height = "100%")
          ),

          card(
            dataTableOutput("wine_table")
          )
        )
      )), 
      nav_panel("ABV", layout_columns(
          card(),
          card(
            card(uiOutput("wine_count_box")),
            card(
              "Average ABV",
              flexdashboard::gaugeOutput("ABV_gauge")
            ),
            card()
          ),
        col_widths = c(8,4)
      )), 
      nav_panel("C", "Page C content"),
      nav_panel("Find perfect wine for you",
        card(
          sliderInput("Price", "Price range", min = 0, max = 100, value = c(10, 50)),
          sliderInput("ABV", "ABV range", min = 0, max = 20, value = c(10, 15)),
          sliderInput("Rating", "Rating range", min = 0, max = 5, value = c(3, 5))
        ),
        layout_columns(
          card(
            plotlyOutput("wine_recommendation_plot")
          ),
          card(
            DT::DTOutput("wine_recommendation_table")
          )
        )
        
)
)
)
)
