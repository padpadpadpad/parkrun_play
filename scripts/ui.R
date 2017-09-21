# shiny app with leaflet to add graphs to the side
# ui
library(leaflet)
library(shinydashboard)

header <- dashboardHeader(
  title = "Parkruns UK"
)

body <- dashboardBody(
  fluidRow(
    column(width = 7,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("map", height = 500)
           )),
    column(width = 5,
           box(width = NULL, plotOutput("plot", height = 220))
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
    
           