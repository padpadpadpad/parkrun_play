# playing with leaflet

# load in packages
library(leaflet)
library(mapview)
library(dplyr)
library(tidyr)
library(ggplot2)

# load in necessary data
leaflet_df <- readRDS('data/leaflet_df.rds')
lat_lon <- readRDS('data/latlon_df.rds')

# change names so there are no spaces parkruns
lat_lon <- mutate(lat_lon, parkrun = gsub(' ', '_', parkrun))
leaflet_df <- mutate(leaflet_df, parkrun = gsub(' ', '_', parkrun))

w_pixel = 400
h_pixel = 600

# function for single ggplot of elevation profile
elev_prof <- function(x, w_pixel, h_pixel) {
  ele_min <- min(lat_lon[lat_lon$parkrun == x,]$elevation)
  x2 <- gsub('_', ' ', x)
  ggplot(lat_lon[lat_lon$parkrun == x,]) +
    geom_ribbon(aes(ymin = ele_min - 0.5, ymax = elevation, x = distance)) +
    xlab('Distance (km)') +
    ylab('Elevation (m)') +
    theme_bw() +
    theme(aspect.ratio = w_pixel/h_pixel) +
    ylim(ele_min - 0.5, ele_min + 250) +
    ggtitle(paste(x2, 'parkrun', sep = ' '))
}

# function for creating multiple plots
point_plots <- function(x) {
  
  ele_min <- min(lat_lon[lat_lon$parkrun == x,]$elevation)
  p1 <- ggplot(lat_lon[lat_lon$parkrun == x,]) +
    geom_ribbon(aes(ymin = ele_min - 0.5, ymax = elevation, x = distance)) +
    xlab('Distance (km)') +
    ylab('Elevation (m)') +
    theme_bw() +
    ylim(ele_min - 0.5, ele_min + 250)
  
  p2 <- ggplot(leaflet_df) +
    geom_histogram(aes(total_elevation_gain), bins = 20, fill = 'black', col = 'black', alpha = 0.5) +
    theme_bw() +
    geom_vline(aes(xintercept = leaflet_df[leaflet_df$parkrun == x,]$total_elevation_gain), linetype = 2) +
    xlab('total elevation gain (m)')
  p <- gridExtra::grid.arrange(p1, p2, ncol = 2, top = x)
}

#  create list of plots
#plots <- lapply(leaflet_df$parkrun, point_plots) # this errors for reasons I dont know
elev_profs <- lapply(leaflet_df$parkrun, elev_prof, w_pixel = 400, h_pixel = 600)

#  one for just elevation profiles - This is pretty cool
m = leaflet(leaflet_df) %>%
  addTiles() %>%
  setView(lat = 54.14, lng = -1.68, zoom = 4) %>%
  fitBounds(-14, 50, 6, 59) %>%
  addMarkers(., lng = ~lng, lat = ~ lat, popup = popupGraph(elev_profs, type = 'png'))

m

# one for multiple plots - does not work - DONT RUN
m2 = leaflet(leaflet_df) %>%
  addTiles() %>%
  setView(lat = 54.14, lng = -1.68, zoom = 4) %>%
  fitBounds(-14, 50, 6, 59) %>%
  addMarkers(., lng = ~lng, lat = ~ lat, popup = popupGraph(plots, type = 'png', height  = 200, width = 400))

m2
