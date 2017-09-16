leaflet_df <- readRDS('~/Desktop/park_runs/leaflet_df.rds')

lat_lon <- readRDS('~/Desktop/park_runs/latlon_df.rds')

# plot graph

# parkruns
lat_lon <- mutate(lat_lon, parkrun = gsub(' ', '_', parkrun))
leaflet_df <- mutate(leaflet_df, parkrun = gsub(' ', '_', parkrun))

# create image for each parkrun
parkruns <- unique(lat_lon$parkrun)

for(i in 1:length(parkruns)){
  plot <- ggplot(lat_lon[lat_lon$parkrun == parkruns[i],]) +
    geom_ribbon(aes(ymin = 0, ymax = ele_norm, x = distance)) +
    xlab('Distance (km)') +
    ylab('Elevation (m)')
  ggsave(file.path('~/Desktop/park_runs/imgs', paste('elev_prof_', parkruns[i], '.png', sep = '')), plot, height = 3, width = 5)
}

img <- httr::GET("https://api.github.com/repos/padpadpadpad/parkrun_play/contents/imgs")
httr::stop_for_status(img)

# list of files
img <- unlist(lapply(httr::content(img), "[[", "download_url"), use.names = T)

# add path to file to leaflet_df
leaflet_df <- leaflet_df %>%
  mutate(., parkrun2 = parkrun) %>%
  group_by(., parkrun) %>%
  mutate(., elev_prof = img[grepl(parkrun2, img)]) %>%
  ungroup() %>%
  select(., -parkrun2)

m = leaflet(leaflet_df) %>%
  addTiles() %>%
  setView(lat = 54.14, lng = -1.68, zoom = 4) %>%
  fitBounds(-14, 50, 6, 59) %>%
  addMarkers(., lng = ~lng, lat = ~ lat, popup = popupImage(imgs))

m
