library(here)
library(lubridate)
library(dplyr)
library(usmap)
library(ggplot2)
library(gganimate)

here <- here::here
source(here("funcs.R"))

filename <- "us-counties-20200422.csv"
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

# a wrapped around the USMAPS transform function to prevent strange errors where the output would have different number of rows from the input
data <- safe_transform(select(data, long, lat, everything()))

# define some parameters to make the plot prettier
min_cases <- 1
max_cases <- max(data$cases)
point_mult <- 3 * 1.26

# NOTE: the base_size in theme_void() is suppose to scale all the elements of the plot to desired
# because the scaling of plot elements in the animation =/= that for saving static plots.
# however, the plot subtitle (which is where the current date at each time step is printed)
# is defined manually.

pl <- plot_usmap() + theme_void(base_size = 22) +
  transition_time(date.day) +
  ease_aes("constant") +
  geom_point(data=data, aes(x=long.1, y=lat.1, size=cases), alpha=0.5, color="dark red", stroke=1, fill=NA, shape=21) +
  geom_point(data=data, aes(x=long.1, y=lat.1, size=cases), alpha=0.2, color="red") +
  
  labs(size="cases") +
  scale_size(range=c(1, 30*point_mult), breaks=c(1, 1000, 10000, 50000), labels=c(min_cases, "1k", "10k", "50k")) +
  theme(legend.position="right", legend.justification="center") +
  
  ggtitle("Reported Covid-19 Cases in the USA by County", subtitle = "{month(frame_time, label=TRUE)} {day(frame_time)}") +
  theme(plot.title = element_text(face="bold",
                                  margin=margin(10,0,10,0),
                                  hjust=.5),
        plot.subtitle = element_text(hjust=.5, size=40, color="red"))

# if you want to ggsave (static plot), you have to remove transition_time and ease_aes calls
# ggsave(filename = "anim-test.png", plot=pl, width=12.80, height=7.20, dpi=100)

# anim_save(filename = "anim-test.gif", animation=pl, renderer=gifski_renderer(), nframes=length(unique(data$date)), fps=1, detail=2, width=720, height=480, end_pause=5)
anim_save(filename = here("sample-output", "anim-test.mp4"), animation=pl, renderer=av_renderer(), nframes=length(unique(data$date)), fps=2/3, detail=2, width=1920, height=1080)
