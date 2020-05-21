library(here)
library(lubridate)
library(dplyr)
library(reshape2)
library(magrittr)
# library(usmap)
library(ggplot2)
library(cowplot)
# library(gganimate)

here <- here::here

filename <- "time_series_covid19_confirmed_US-20200521.csv"

# data loading, fixing/cleaning ----

data <- read.csv(file = here("jh-data", filename), stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
data <- melt(data, id.vars=c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_", "Combined_Key"), variable.name="date", value.name="cases")
data$date <- sapply(data$date, function(x){substring(x, 2)})
data$date.day <- mdy(data$date)
data <- rename(data, date = date.day, date.string = date, state = Province_State)

# diff over N days

N = 7

max_date <- max(data$date)
min_date <- min(data$date)
desired_dates <- seq(max_date, min_date, by = -as.difftime(days(N)))

# it's N-day not weekly but it still makes sense
# weekly has columns: date, cases, growth, relgrowth (growth/cases)
weekly <- data %>%
  filter(date %in% desired_dates) %>%
  group_by(date) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`) %>%
  mutate(growth = cases - lag(cases, default=cases[1])) %>%
  mutate(relgrowth = growth / cases)

# daily has columns: date, cases
daily <- data %>%
  group_by(date) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`)

# logistic model ----

nlsmod <- nls(daily$cases ~ SSlogis(as.numeric(daily$date), ph1, ph2, ph3), data = daily)
summary(nlsmod)
alpha <- coef(nlsmod)
f_logis <- function(d) as.numeric(alpha[1]/(1 + exp(-(as.numeric(d) - alpha[2]) / alpha[3])))
f_logis_10 <- function(d) log10(f_logis(d))

logis <- daily %>%
  select(date) %>%
  mutate(cases = f_logis(date))

logis_weekly <- logis %>%
  filter(date %in% desired_dates) %>%
  group_by(date) %>%
  summarize(sum(cases)) %>%
  rename(cases = `sum(cases)`) %>%
  mutate(growth = cases - lag(cases, default=cases[1])) %>%
  mutate(relgrowth = growth / cases)

# plotting ----

# plot logistic fit over data - linear

p0 <- ggplot(data = daily, aes(x=date, y=cases)) + 
  geom_point() + 
  stat_function(fun=f_logis, color="red") +
  theme_light()

print(p0)

# plot logistic fit over data - log
# starting after March because cases are noisy before March and growth is ~ 0
# have to use f_logis_10 because stat_function is plotted after log scale is applied:
# can use logis and logis_weekly instead
p1 <- ggplot(data = filter(daily, date > ymd("20200301")), aes(x=date, y=cases)) + 
  geom_point() + 
  stat_function(fun=f_logis_10, color="red") +
  scale_y_continuous(trans="log10") +
  theme_light()

print(p1)

# plot of N-day growth vs total cases
p2 <- ggplot(data=weekly, aes(x=cases, y=growth)) + 
  geom_point() + 
  scale_y_continuous(trans="log10") + 
  scale_x_continuous(trans="log10") + 
  ylab(paste0(N, "-day growth in cases")) + 
  theme_light()

print(p2)

# plot of cases and N-day growth vs time

p3 <- ggplot(data = daily, aes(x=date, y=cases)) + 
  geom_point() + 
  geom_point(data = weekly, aes(x=date, y=growth), color="red") + 
  scale_y_continuous(trans="log10", 
                     sec.axis = sec_axis(~ . * 1, name=paste0(N, "-day growth in cases"))) + 
  theme_light()

print(p3)

# plot of relgrowth to cases for data and for logistic model

p4 <- ggplot(data = weekly, aes(x=cases, y=relgrowth)) + 
  geom_point() + 
  geom_line(data = logis_weekly, aes(x=cases, y=relgrowth)) + 
  scale_y_continuous(trans="log10") +
  scale_x_continuous(trans="log10") + 
  theme_light()
  
print(p4)