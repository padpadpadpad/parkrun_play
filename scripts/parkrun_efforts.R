#devtools::install_github('fawda123/rStrava')
#devtools::install_github("timelyportfolio/listviewer")

mise::mise(vars = TRUE, console = TRUE)

library(rStrava)
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(purrr)
library(ggjoy)

help.search('token', package = 'rStrava')

app_name <- 'stRava'
app_client_id <- '12962'
app_secret <- 'e58f0335795fb08674d2a412c9d35efb0d740761'

# create token
my_token <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

# functions
compile_effort <- function(x){
  temp <- data.frame(unlist(x)) %>%
    mutate(ColNames = rownames(.)) %>%
    spread(., ColNames, unlist.x.)
  desired_cols <- c('athlete.id', 'distance', 'elapsed_time', 'moving_time', 'name', 'start_date', 'start_date_local')
  # check which columns arent present
  cols_not_present <- desired_cols[! desired_cols %in% colnames(temp)] %>%
    data.frame(cols = .) %>%
    mutate(., value = NA)
  if(nrow(cols_not_present) >= 1){cols_not_present <- spread(cols_not_present, cols, value)}
  if(nrow(cols_not_present) == 1){temp <- cbind(temp, cols_not_present)}
  temp <- select(temp, athlete.id, distance, elapsed_time, moving_time, name, start_date, start_date_local)
  return(temp)
}

compile_segment_efforts <- function(x, stoken){
  temp1 <- get_efforts_list(stoken, id = x)
  temp2 <- suppressWarnings(purrr::map_df(temp1, compile_effort))
  return(temp2)
}

# bind segment info together
segments <- read.csv("~/Desktop/park_runs/raw/park_run_segmentIDs.csv", stringsAsFactors = FALSE) %>%
  filter(., !is.na(segment_id) & !is.na(Parkrun))

# get leaderboard for parkruns
# test run through
#test <- segments[1:5,]

d_parkrun <- segments$segment_id %>% purrr::map_df(., .f = compile_segment_efforts, stoken = my_token, .id = 'id')

# make some columns numeric
d_parkrun <- mutate_at(d_parkrun, c('distance', 'moving_time', 'elapsed_time'), as.numeric) 

segments <- mutate(segments, id = 1:n()) %>%
  rename(., parkrun = Parkrun)

# filter and merge with ids
d <- filter(d_parkrun, distance > 4750 & distance < 5200 & elapsed_time < 60 * 60) %>%
  merge(., select(segments, id, parkrun), by = 'id')

saveRDS(d, '~/Desktop/park_runs/parkrun_efforts.rds')

# plot as a joyplot
ggplot(filter(d, elapsed_time < 3000)) +
  geom_joy(aes(elapsed_time, parkrun)) +
  theme_bw() +
  xlab('Time (s)') +
  ylab('')

# create date columnn
MaenMurderData$date <- gsub("T.*$", '', MaenMurderData$start_date) %>%
  as.POSIXct(., format = '%Y-%m-%d')
MaenMurderData <- mutate(MaenMurderData, month = format(date, "%m"),
                         day = format(date, "%d"),
                         year = format(date, "%Y"))

MaenMurderData[c('month', 'day', 'year')] <- lapply(MaenMurderData[c('month', 'day', 'year')], as.numeric)


