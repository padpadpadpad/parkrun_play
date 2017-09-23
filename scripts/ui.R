# shiny app with leaflet to add graphs to the side
# ui
library(leaflet)
library(shinydashboard)



header <- dashboardHeader(
  title = "Parkruns UK"
)

body <- dashboardBody(
  fluidRow(
    column(width = 8,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("map", height = 610)
           )),
    column(width = 4,
           box(width = NULL, title = 'Selected parkrun elevation profile:',
               plotOutput("elev_plot", height = 240),
               status = 'success'
               ),
           box(width = NULL, title = 'How hilly is the selected parkrun?',
               status = 'success',
               plotOutput("elev_dist", height = 240))
    )
  )
)

dashboardPage(
  skin = 'green',
  header,
  dashboardSidebar(disable = TRUE),
  body
)
    
           