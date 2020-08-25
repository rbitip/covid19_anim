library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(gganimate)


here <- here::here
source(here("funcs.R"))

filename <- "us-counties-20200824.csv"
# name for nytimes data

# data loading, fixing ----

data <- read.csv(file = here("nyt-data", filename), stringsAsFactors = FALSE)
fips_lookup <- read.csv(file=here("fips-lookup.csv"), stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")

# add column of numberified date, to track timesteps for the animation
data$date.day <- ymd(data$date)

# required for left join for no fips no county rows
# can I do this without knowing the index of that header?
names(fips_lookup)[2] <- "state"

data$fips[data$county=="New York City"] <- 36061
# the county KC, MO is in mostly - just to find lat/long
data$fips[data$county=="Kansas City"] <- 29095

# add fips for data with no fips and no county
data <- left_join(data, fips_lookup[8:58,1:2], by = "state", suffix = c("", "x"))
data$fips[data$county=="Unknown"] <- data$fipsx[data$county=="Unknown"]

# give lat and long for all data with a fips
data <- left_join(data, fips_lookup[,c(1, 3, 4)], by = "fips")

# remove data points with no lat, since it'll throw an error while plotting
data <- data[!is.na(data$lat),]


# this snippet is to add extra data points to the data so that there is data for each location after they are introduced to prevent flickering
dates <- unique(data$date)
prev <- "2020-01-22"
for(date in unique(data$date)){
  if(date == "2020-01-22"){
    next
  }
  previous_fips <- unique(data$fips[data$date==prev])
  current_fips <- unique(data$fips[data$date==date])
  interest_fips <- previous_fips[!(previous_fips %in% current_fips)]
  
  adds <- data[data$date == prev & data$fips %in% interest_fips, ]
  if(nrow(adds) > 0){
    adds$date <- date
    adds$date.day <- ymd(date)
    data <- rbind(data, adds)
  }
  
  prev <- date
}

data <- data %>%
  filter(date.day >= ymd("2020-02-01")) %>%
  # mutate(cases = cases / 1000)
  mutate(cases = cases)

country <- data %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day)

country_deltas <- country %>%
  mutate(diff = cases - lag(cases, n=1)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2) + lag(diff, n=3) + lag(diff, n=4) + lag(diff, n=5) + lag(diff, n=6))/7) %>%
  select(date, roll) %>%
  rename(cases = roll)

states <- data %>%
  group_by(date.day, state) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day)

# pl <- ggplot(data = state_time_data, aes(x=date, y=cases, color=state)) + 
#   geom_line() +
#   scale_y_continuous(trans="log10")

library(scales)

# linear cases
# pl <- ggplot(data = country, aes(x=date, y=cases)) + 
#   # theme_classic(base_size=11) + 
#   theme_bw(base_size=11) + 
#   geom_line() + 
#   scale_x_date(breaks = scales::pretty_breaks(n=8), limits = c(ymd("2020-02-01"), ymd("2020-09-01")), minor_breaks = NULL) + 
#   scale_y_continuous(limits=c(0,6e6), breaks = seq(from=0, to=6, by=1)*1e6, labels = function(x){paste0(x/1e6, "m")}, minor_breaks = NULL)

# diffs

pl <- ggplot(data = country_deltas, aes(x=date, y=cases)) +
  # theme_classic(base_size=11) +
  theme_bw(base_size=11) +
  geom_line() 
  # scale_x_date(breaks = scales::pretty_breaks(n=8), limits = c(ymd("2020-02-01"), ymd("2020-09-01")), minor_breaks = NULL) +
  # scale_y_continuous(limits=c(0,6e6), breaks = seq(from=0, to=6, by=1)*1e6, labels = function(x){paste0(x/1e6, "m")}, minor_breaks = NULL)

print(pl)

# linear
# pla <- ggplot(data = country, aes(x=date, y=cases)) + 
#   theme_bw(base_size=11*3/2) + 
#   geom_point(color="red", size=3) + 
#   geom_line(color="black", size=1) +
#   scale_x_date(breaks = scales::pretty_breaks(n=8), limits = c(ymd("2020-02-01"), ymd("2020-09-01")), minor_breaks = NULL) + 
#   scale_y_continuous(limits=c(0,6e6), breaks = seq(from=0, to=6, by=1)*1e6, labels = function(x){paste0(x/1e6, "m")}, minor_breaks = NULL) +
#   ggtitle(label = "USA Cumulative Cases") + theme(plot.title = element_text(hjust=0.5)) +
#   transition_reveal(date) + 
#   ease_aes("linear")

# log10
# pla <- ggplot(data = country, aes(x=date, y=cases)) + 
#   theme_bw(base_size=11*3/2) + 
#   geom_point(color="red", size=3) + 
#   geom_line(color="black", size=1) +
#   scale_x_date(breaks = scales::pretty_breaks(n=8), limits = c(ymd("2020-02-01"), ymd("2020-09-01")), minor_breaks = NULL) + 
#   scale_y_continuous(breaks = 10^seq(from=0, to=7, by=1), labels = c(1, 10, 100, "1k", "10k", "100k", "1m", "10m"), minor_breaks = NULL, trans="log10") +
#   ggtitle(label = "USA Cumulative Cases") + theme(plot.title = element_text(hjust=0.5)) +
#   transition_reveal(date) + 
#   ease_aes("linear")
  
# linear deltas
# pla <- ggplot(data = country_deltas, aes(x=date, y=cases)) +
#   theme_bw(base_size=11*3/2) +
#   geom_point(color="red", size=3) +
#   geom_line(color="black", size=1) +
#   scale_x_date(breaks = scales::pretty_breaks(n=8), limits = c(ymd("2020-02-01"), ymd("2020-09-01")), minor_breaks = NULL) +
#   scale_y_continuous(limits=c(0,7e4), breaks = seq(from=0, to=7, by=1)*1e4, labels = function(x){paste0(x/1e3, "k")}, minor_breaks = NULL) +
#   ggtitle(label = "USA Daily Cases (weekly rolling average)") + theme(plot.title = element_text(hjust=0.5)) +
#   transition_reveal(date) +
#   ease_aes("linear")

# log10 deltas
# pla <- ggplot(data = country_deltas, aes(x=date, y=cases)) +
#   theme_bw(base_size=11*3/2) +
#   geom_point(color="red", size=3) +
#   geom_line(color="black", size=1) +
#   scale_x_date(breaks = scales::pretty_breaks(n=8), limits = c(ymd("2020-02-01"), ymd("2020-09-01")), minor_breaks = NULL) +
#   scale_y_continuous(breaks = 10^seq(from=0, to=5, by=1), labels = c(1, 10, 100, "1k", "10k", "100k"), minor_breaks = NULL, trans="log10") +
#   ggtitle(label = "USA Daily Cases (weekly rolling average)") + theme(plot.title = element_text(hjust=0.5)) +
#   transition_reveal(date) +
#   ease_aes("linear")

# linear, states
pla <- ggplot(data = states, aes(x=date, y=cases, color=state)) +
  theme_bw(base_size=11*3/2) +
  geom_point(size=2) +
  geom_line(size=1) +
  scale_x_date(breaks = "month", limits = c(ymd("2020-02-01"), ymd("2020-09-01")), minor_breaks = NULL) +
  scale_y_continuous(breaks = 10^seq(from=0, to=6, by=1), labels = c(1, 10, 100, "1k", "10k", "100k", "1m"), minor_breaks = NULL, trans="log10") +
  ggtitle(label = "USA States Cumulative Cases") + theme(plot.title = element_text(hjust=0.5)) + 
  guides(color=guide_legend(ncol=2)) +
  transition_reveal(date) +
  ease_aes("linear")

# anim_save(filename = here("sample-output", paste0("USA-anim-test-20200824.mp4")),
          # animation=pla, renderer=av_renderer(), nframes=length(unique(data$date))/12,
          # fps=2, detail=2, width=1920, height=1080)
anim_save(filename = here("sample-output", paste0("states-20200824-log10.mp4")),
          animation=pla, renderer=av_renderer(), duration = 10,
          fps=24, detail=1, width=1920*2/3, height=1080*2/3)
