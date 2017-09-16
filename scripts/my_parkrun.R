# Trying to look at park runs through time ####

# load packages
library(rStrava)
library(ggplot2)
library(ggmap)
library(tidyr)
library(dplyr)
library(viridis)

# initial setup for APIs ####

# Strava key
app_name <- 'stRava'
app_client_id <- '12962'
app_secret <- 'e58f0335795fb08674d2a412c9d35efb0d740761'

# Google elevation API key
GoogleAPI <- 'AIzaSyDgUnXCaOSQ90xZ9wUYGP0E9BUtIkm2gkU'

# create token
my_token <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

# function
get_all_laps <- function(x, stoken, units = 'metric'){
  
  # get the activity, split speeds are not in the actframe
  act <- get_activity(x, stoken)
  
  # split type
  sptyp <- paste0('splits_', units)
  sptyp <- gsub('imperial$', 'standard', sptyp)
  
  # get speed per split,  convert from m/s to km/hr
  splt <- lapply(act[[sptyp]], function(x) x[['average_speed']]) %>% 
    do.call('rbind', .) %>% 
    data.frame(spd = ., split = 1:length(.))
  splt$spd <- 3.6 * splt$spd
  splt2 <- lapply(act[[sptyp]], function(x) x[['elapsed_time']]) %>% 
    do.call('rbind', .) %>% 
    data.frame(elapsed_time = .)
  if(units == 'imperial'){
    # m/s to mph
    splt$spd <- splt$spd * 0.621371
  }
  return(cbind(splt, splt2))
}

# download activities
my_acts <- get_activity_list(my_token, id = '2140248')
MyActs <- compile_activities(my_acts)

info <- get_activity(MyActs$id[1], my_token)

# filter for Trelissick park run - could be spelt differently so will filter on different conditions
trel_park_run <- mutate(MyActs, name = tolower(name)) %>%
  filter(., name %in% c('trelissick park run', 'trellissick park run'))

# select desired columns
trel_park_run <- select(trel_park_run, c(achievement_count, athlete_count, average_heartrate, average_speed, distance, elapsed_time, id, kudos_count, map.summary_polyline, max_heartrate, max_speed, moving_time, start_date, suffer_score, total_elevation_gain, upload_id))

# transform some columns
trel_park_run <- mutate_at(trel_park_run, c('average_heartrate', 'suffer_score', 'max_heartrate', 'max_speed', 'moving_time'), as.numeric) %>%
  mutate(.,
         id = 1:n(),
         elapsed_time = elapsed_time/60,
         moving_time = moving_time/60,
         average_speed = average_speed,
         max_speed = max_speed,
         elevation_per_km = total_elevation_gain/distance,
         date = gsub("T.*$", '', start_date) %>%
           as.POSIXct(., format = '%Y-%m-%d'),
         EUdate = format(date, '%d/%m/%Y'),
         month = format(date, "%m"),
         day = format(date, "%d"),
         year = format(date, "%Y")) %>%
  mutate_at(., c('month', 'day', 'year'), as.numeric) %>%
  data.frame()

# plot time vs date
ggplot(trel_park_run) +
  geom_point(aes(date, elapsed_time)) +
  theme_bw() 
  
# get lat lon of one run ####
lat_lon <- get_all_LatLon(id_col = 'upload_id', parent_data = trel_park_run) %>%
  full_join(., trel_park_run, by = 'upload_id') %>%
  select(., c(upload_id, date, EUdate, year, lat, lon, activity)) %>%
  filter(., activity == 1) %>%
  mutate(distance = get_dists(.),
         n = 1:n())

# make new bbox
bbox <- ggmap::make_bbox(lat_lon$lon, lat_lon$lat, f = 0.5)

map <- suppressWarnings(suppressMessages(get_map(location = bbox, source = 'google', maptype = 'terrain')))

le_plot <- ggmap(map, darken = 0.2) +
  coord_fixed(ratio = 1) +
  theme(axis.title = element_blank()) +
  geom_path(aes(x = lon, y = lat, frame = n, cumulative = TRUE), col = 'red', data = lat_lon, size = 2, alpha = 0.5) +
  coord_cartesian() +
  ggforce::theme_no_axes()

animation::ani.options(interval = 1/10)
ride_2017 <- gganimate::gganimate(le_plot, title_frame = FALSE, '~/Desktop/Trelissick_park_run.gif', ani.width = 800/2, ani.height=600/2)

# get laps of parkruns
parkrun_laps <- trel_park_run$id %>%
  purrr::map_df(., get_all_laps, stoken = my_token, .id = 'id')
parkrun_laps <- group_by(parkrun_laps, id) %>%
  mutate(., total = sum(elapsed_time)) %>%
  ungroup() %>%
  mutate(., rank = dense_rank(total)) %>%
  merge(., select(trel_park_run, id, date), by = 'id')

ggplot(parkrun_laps) +
  geom_bar(aes(split, spd, fill = factor(rank), group = id), stat = 'identity', position = 'dodge') +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE)

ggplot(parkrun_laps) +
  geom_point(aes(date, elapsed_time, col = factor(rank))) +
  theme_bw() +
  facet_wrap(~ split) +
  scale_color_viridis(discrete = TRUE)
