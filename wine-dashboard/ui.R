#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

# Define UI for application that draws a histogram
page_fluid(
  page_sidebar(
  title = "Wine dashboard",
  sidebar = sidebar(
    checkboxGroupInput("checkGroupTypes", label = h3("Checkbox group"), 
                       choices = NULL
                       ),
  ),
  plotlyOutput("map_plot")
)
)
