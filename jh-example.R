library(here)
library(lubridate)
library(dplyr)
library(reshape2)
library(magrittr)
# library(usmap)
library(ggplot2)
# library(gganimate)

here <- here::here

filename <- "time_series_covid19_confirmed_US-20200422.csv"

# data loading, fixing/cleaning ----

data <- read.csv(file = here("jh-data", filename), stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
data <- melt(data, id.vars=c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_", "Combined_Key"), variable.name="date", value.name="cases")
data$date <- sapply(data$date, function(x){substring(x, 2)})
data$date.day <- mdy(data$date)
data <- rename(data, date = date.day, date.string = date, state = Province_State)

# selecting data ----

# to get get data just for snohomish county, the way to do it without dplyr would be:
# data$cases[data$Admin2=="Snohomish"]
# with dplyr, instead you can do:
# filter(data, Admin2 == Snohomish)
# which is quite a bit cleaner

data1 <- data %>%
  # filter for these two counties
  filter(Admin2 %in% c("Snohomish", "King")) %>%
  # the following sums all data by date to make one plot
  group_by(date) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`)

# plotting ----

pl <- ggplot(data=data1, aes(x=date, y=cases)) +
  geom_point()

print(pl)

# second example ----

data2 <- data %>%
  # filter for these three states
  # cases = 0 makes the logplot throw warnings
  filter(state %in% c("California", "Oregon", "Washington"), cases > 0) %>%
  # in essence, with group_by state the id variables you want
  group_by(state, date) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`)

pl <- ggplot(data=data2, aes(x=date, y=cases, color=state)) +
  geom_point() +
  scale_y_continuous(trans="log10") +
  annotation_logticks()

print(pl)
