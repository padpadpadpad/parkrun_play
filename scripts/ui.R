# shiny app with leaflet to add graphs to the side
# ui
library(leaflet)

ui <- fluidPage(
  br(),
  column(8,leafletOutput("map", height="600px")),
  column(4,br(),br(),br(),br(),plotOutput("plot", height="300px")),
  br()
)