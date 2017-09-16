# getting segment polyline and information

library(rStrava)
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(purrr)
library(ggjoy)
library(leaflet)

help.search('token', package = 'rStrava')

app_name <- 'stRava'
app_client_id <- '12962'
app_secret <- 'e58f0335795fb08674d2a412c9d35efb0d740761'

# create token
my_token <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))
stoken <- my_token
# google key
GoogleApi <- 'AIzaSyDgUnXCaOSQ90xZ9wUYGP0E9BUtIkm2gkU'

# trelissick parkrun
x <- 13802495

# get segment data
segment <- get_segment(my_token, id = id)

# bind segment info together
segments <- read.csv("~/Desktop/park_runs/raw/park_run_segmentIDs.csv", stringsAsFactors = FALSE) %>%
  filter(., !is.na(segment_id) & !is.na(Parkrun))

compile_segment <- function(x, columns, stoken){
  temp <- rStrava::get_segment(stoken, id = x)
  temp <- data.frame(unlist(temp), stringsAsFactors = F)
  temp$ColNames <- rownames(temp)
  temp <- tidyr::spread(temp, ColNames, unlist.temp.)
  return(temp)
}

get_LatLon <- function(x, .id_col){
  if('map.summary_polyline' %in% names(x)){y <- decode_Polyline(x$map.summary_polyline)}
  if('map.polyline' %in% names(x)){y <- decode_Polyline(x$map.polyline)}
  
  y[,.id_col] <- unique(x[,.id_col])
  return(y)
}

get_all_LatLon <- function (id_col, parent_data){
  id_col_fac <- as.factor(parent_data[, id_col])
  temp <- split(parent_data, id_col_fac)
  temp_data <- suppressWarnings(plyr::ldply(temp, get_LatLon, .id_col = id_col))
  dat <- temp_data[, c(3, 2)]
  dat <- tidyr::separate(dat, latlon, c("lat", "lon"), sep = ",")
  dat <- dplyr::mutate_at(dat, c("lat", "lon"), as.numeric)
  return(dat)
}

get_dists <- function(dat_in, lon = 'lon', lat = 'lat', split_col = 'activity'){
  
  dat <- dat_in[,c(split_col, lon, lat)]
  names(dat) <- c('activity', 'lon', 'lat')
  
  # distances by activity
  out <- split(dat, dat$activity)
  out <- lapply(out, function(x){
    
    x <- x[, c('lon', 'lat')]
    x <- sapply(2:nrow(x), function(y){geosphere::distm(x[y-1,], x[y,])/1000})
    
    return(c(0, cumsum(x)))
    
  })
  
  out <- as.numeric(unlist(out))
  return(out)
  
}

d_parkrun <- segments$segment_id %>% purrr::map_df(., .f = compile_segment, stoken = my_token) %>%
  mutate(., parkrun = segments$Parkrun)

# make some columns numeric
d_parkrun <- mutate_at(d_parkrun, c('total_elevation_gain'), as.numeric)

# get lat lon, distance and elevation of many runs ####
lat_lon <- get_all_LatLon(id_col = 'parkrun', parent_data = d_parkrun) %>%
  mutate(distance = get_dists(., split_col = 'parkrun'),
         elevation = rgbif::elevation(latitude = lat, longitude = lon, key = GoogleApi)[,'elevation']) %>%
  group_by(parkrun) %>%
  # normalise elevation to make them plottable
  mutate(., ele_norm = elevation - min(elevation) + 1) %>%
  ungroup()

# number of rows per park run
num_points <- group_by(lat_lon, parkrun) %>%
  summarise(., num = n())

head(lat_lon)

ggplot(lat_lon) +
  geom_ribbon(aes(ymin = 0, ymax = ele_norm, x = distance)) +
  facet_wrap(~ parkrun)

ggplot(lat_lon) +
  geom_line(aes(distance, elevation, group = parkrun), alpha = 0.25) +
  theme_bw()

ggplot(filter(d_parkrun, total_elevation_gain < 400)) +
  geom_histogram(aes(total_elevation_gain), bins = 20, fill = 'blue', col = 'black', alpha = 0.7) +
  theme_bw() +
  geom_vline(aes(xintercept = filter(d_parkrun, parkrun == 'Trelissick')$total_elevation_gain), linetype = 2) +
  xlab('total elevation gain (m)')

ggplot(filter(d_parkrun, total_elevation_gain < 195)) +
  geom_density(aes(total_elevation_gain), fill = 'blue', col = 'black', alpha = 0.7) +
  theme_bw() +
  geom_vline(aes(xintercept = filter(d_parkrun, parkrun == 'Trelissick')$total_elevation_gain), linetype = 2) +
  xlab('total elevation gain (m)')

leaflet_df <- select(d_parkrun, parkrun, start_latitude, start_longitude, total_elevation_gain) %>%
  rename(lat = start_latitude, lng = start_longitude) %>%
  mutate_at(., c('lat', 'lng'), as.numeric)

saveRDS(leaflet_df, '~/Desktop/park_runs/leaflet_df.rds')
saveRDS(lat_lon, '~/Desktop/park_runs/latlon_df.rds')


m = leaflet(leaflet_df) %>%
  addTiles() %>%
  setView(lat = 54.14, lng = -1.68, zoom = 4) %>%
  fitBounds(-14, 50, 6, 59) %>%
  addMarkers(., lng = ~lng, lat = ~ lat, popup = ~parkrun)
