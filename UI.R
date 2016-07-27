# Shiny UI
library(shiny)
library(stringr)

suburb_options <- str_replace(list.files(path = "suburbs/"), ".csv", "")

shinyUI(fluidPage(
  selectInput("select_suburb", label = h3("Select a Suburb"),
              choices = suburb_options,
              selected = 1),
  plotOutput("suburb_plot")
))

