library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)


here <- here::here
source(here("funcs.R"))

filename <- "us-counties-20200624.csv"
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
  mutate(cases = cases / 1000)

cum_data <- data %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day)

pl1 <- ggplot(data = cum_data, aes(x=date, y=cases)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(labels = unit_format(unit = "K")) + 
  ggtitle("Cumulative USA Reported Covid-19 Cases")


pl2 <- ggplot(data = mutate(cum_data, cases = cases*1000), aes(x=date, y=cases)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(trans = "log10") + 
  ggtitle("Cumulative USA Reported Covid-19 Cases - log scale")

pl <-  grid.arrange(pl1, pl2, nrow=1)

print(pl)

# bad boys ----

# kentucky <- data %>%
#   filter(state == "Kentucky") %>%
#   group_by(date.day) %>%
#   summarize(sum(cases)) %>%
#   rename(cases = `sum(cases)`, date = date.day) %>%
#   mutate(diff = cases - lag(cases, n=1)) %>%
#   mutate(roll = (diff + lag(diff) + lag(diff, n=2) + lag(diff, n=3) + lag(diff, n=4) + lag(diff, n=5) + lag(diff, n=6))/7)
# 
# plKY <- ggplot(data = kentucky, aes(x=date, y=roll)) + 
#   geom_point(color="red") + 
#   scale_x_date(date_breaks = "1 month") + 
#   scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
#   ggtitle("7-day avg. New Cases - KY")
# 
# plot(plKY)

str <- "Weekly Confirmed New Cases (3-day moving avg)"

arizona <- data %>%
  filter(state == "Arizona") %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day) %>%
  mutate(diff = cases - lag(cases, n=7)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2))/3)

plAZ <- ggplot(data = arizona, aes(x=date, y=roll)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(labels = unit_format(unit = "K"), name = "change in cases") + 
  ggtitle(paste0(str, " - AZ"))

# plot(plAZ)

california <- data %>%
  filter(state == "California") %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day) %>%
  mutate(diff = cases - lag(cases, n=7)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2))/3)

plCA <- ggplot(data = california, aes(x=date, y=roll)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(labels = unit_format(unit = "K"), name = "change in cases") + 
  ggtitle(paste0(str, " - CA"))

# plot(plCA)

florida <- data %>%
  filter(state == "Florida") %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day) %>%
  mutate(diff = cases - lag(cases, n=7)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2))/3)

plFL <- ggplot(data = florida, aes(x=date, y=roll)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(labels = unit_format(unit = "K"), name = "change in cases") + 
  ggtitle(paste0(str, " - FL"))

# plot(plFL)

texas <- data %>%
  filter(state == "Texas") %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day) %>%
  mutate(diff = cases - lag(cases, n=7)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2))/3)


plTX <- ggplot(data = texas, aes(x=date, y=roll)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(labels = unit_format(unit = "K"), name = "change in cases") + 
  ggtitle(paste0(str, " - TX"))

# plot(plTX)

# KY, CA, FL, TX
pl <-  grid.arrange(plCA, plAZ, plFL, plTX, nrow=2)
print(pl)

# gooder bois ----

newyork <- data %>%
  filter(state == "New York") %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day) %>%
  mutate(diff = cases - lag(cases, n=7)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2))/3)

plNY <- ggplot(data = newyork, aes(x=date, y=roll)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(labels = unit_format(unit = "K"), name = "change in cases") + 
  ggtitle(paste0(str, " - NY"))

# plot(plNY)

illinois <- data %>%
  filter(state == "Illinois") %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day) %>%
  mutate(diff = cases - lag(cases, n=7)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2))/3)

plIL <- ggplot(data = illinois, aes(x=date, y=roll)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(labels = unit_format(unit = "K"), name = "change in cases") + 
  ggtitle(paste0(str, " - IL"))

# plot(plIL)

washington <- data %>%
  filter(state == "Washington") %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day) %>%
  mutate(diff = cases - lag(cases, n=7)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2))/3*1000)

plWA <- ggplot(data = washington, aes(x=date, y=roll)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(name = "change in cases") + 
  ggtitle(paste0(str, " - WA"))

# plot(plWA)

DC <- data %>%
  filter(state == "District of Columbia") %>%
  group_by(date.day) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`, date = date.day) %>%
  mutate(diff = cases - lag(cases, n=7)) %>%
  mutate(roll = (diff + lag(diff) + lag(diff, n=2))/3*1000)

plDC <- ggplot(data = DC, aes(x=date, y=roll)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 month") + 
  scale_y_continuous(name = "change in cases") + 
  ggtitle(paste0(str, " - DC"))

# plot(plDC)

# newyork, illinois, washington, DC

pl <-  grid.arrange(plNY, plIL, plWA, plDC, nrow=2)
print(pl)
