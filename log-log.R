library(here)
library(lubridate)
library(dplyr)
library(reshape2)
library(magrittr)
# library(usmap)
library(ggplot2)
# library(gganimate)

here <- here::here

filename <- "time_series_covid19_confirmed_US-20200412.csv"

# data loading, fixing/cleaning ----

data <- read.csv(file = here("jh-data", filename), stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
data <- melt(data, id.vars=c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_", "Combined_Key"), variable.name="date", value.name="cases")
data$date <- sapply(data$date, function(x){substring(x, 2)})
data$date.day <- mdy(data$date)
data <- rename(data, date = date.day, date.string = date, state = Province_State)

max_date <- max(data$date)
min_date <- min(data$date)
desired_dates <- seq(max_date, min_date, by = -as.difftime(days(7)))

weekly <- data %>%
  filter(date %in% desired_dates) %>%
  group_by(date) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`) %>%
  mutate(growth = cases - lag(cases, default=cases[1]))

# weekly_delta <- data.frame(cases = tail(weekly$cases, -1), growth = diff(weekly$cases))
# lag is the way to get diff for this if using something like dplyr

pl <- ggplot(data=weekly, aes(x=cases, y=growth)) + 
  geom_point() + 
  scale_y_continuous(trans="log10") + 
  scale_x_continuous(trans="log10") + 
  ylab("weekly growth")

print(pl)

weekly_states <- data %>%
  filter(date %in% desired_dates) %>%
  filter(state %in% c("California", "Washington", "New York", "Illinois", "Florida")) %>%
  group_by(date, state) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`) %>%
  group_by(state) %>%
  mutate(growth = cases - lag(cases, default=cases[1])) %>%
  filter(growth > 0)

pl <- ggplot(data=weekly_states, aes(x=cases, y=growth, color=state)) + 
  geom_line() +
  scale_y_continuous(trans="log10") +
  scale_x_continuous(trans="log10") +
  ylab("weekly growth")

print(pl)