library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)


here <- here::here
source(here("funcs.R"))

filename <- "us-counties-20200414.csv"
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

cum_data <- data %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day)

pl1 <- ggplot(data = cum_data, aes(x=date, y=cases)) + 
  geom_point(color="red") + 
  scale_x_date(date_breaks = "1 week") + 
  ggtitle("Cumulative USA Reported Covid-19 Cases")


pl2 <- ggplot(data = cum_data, aes(x=date, y=cases)) + 
  geom_point(color="red") + 
  scale_x_date(date_breaks = "1 week") + 
  scale_y_continuous(trans="log10") + 
  ggtitle("Cumulative USA Reported Covid-19 Cases")

pl <-  grid.arrange(pl1, pl2, nrow=1)

print(pl)
