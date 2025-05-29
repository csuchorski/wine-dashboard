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
              gaugeOutput("ABV_gauge")
            ),
            card()
          ),
        col_widths = c(8,4)
      )), 
      nav_panel("C", "Page C content") 
    )
  )
)
