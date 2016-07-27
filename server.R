# Shiny Server

shinyServer(function(input, output) {
  output$suburb_plot <- renderPlot()
})