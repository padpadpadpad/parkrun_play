# shiny app with leaflet to add graphs to the side
# server
library(leaflet)
library(dplyr)
library(ggplot2)

# read in data
leaflet_df <- readRDS('../data/leaflet_df.rds')

# change names so there are no spaces parkruns
lat_lon <- readRDS('../data/latlon_df.rds')

server <- function(input, output){
  
  # create a reactive value that will store the click position
  clicked_data <- reactiveValues(clickedMarker=NULL)
  
  # render leaflet map
  output$map <- renderLeaflet({
    leaflet(leaflet_df) %>%
      addTiles() %>%
      setView(lat = 54.14, lng = -1.68, zoom = 4) %>%
      fitBounds(-14, 50, 6, 59) %>%
      addMarkers(., lng = ~ lng, lat = ~ lat, layerId = ~ parkrun, popup = ~ parkrun)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    clicked_data$clickedMarker <- input$map_marker_click
  })
  
  lat_lon_df <- reactive({
    if(is.null(clicked_data$clickedMarker)){lat_lon[lat_lon$parkrun == 'Aberdeen',]}
      else{lat_lon[lat_lon$parkrun == clicked_data$clickedMarker$id,]}
    })
  
  # make a plot depending on the selected point
  output$plot <- renderPlot({
    p <- lat_lon_df()
    plot <- ggplot(p) +
      geom_ribbon(aes(ymin = min(p$elevation) - 0.5, ymax = elevation, x = distance)) +
      xlab('Distance (km)') +
      ylab('Elevation (m)') +
      theme_bw() +
      ylim(min(p$elevation) - 0.5, min(p$elevation) + 250) +
      ggtitle(paste(gsub('_', ' ', p$parkrun), 'parkrun', sep = ' '))
    print(plot)
  })
  
}

