# covid19_anim

Home to PEMY's COVID-19 visualizations. The first point of interest is the time-lapse of reported COVID-19 cases spreading through the US. This animation is rendered in map-anim.R, with an example in the folder sample-output. In addition, there are some scripts that create other visualizations.

## requirements

here, lubridate, dplyr, usmap, ggplot2, gganimate, gridExtra, magrittr

Credit and thanks given to the New York Times (https://github.com/nytimes/covid-19-data) and John Hopkins's CSSEGISandData (https://github.com/CSSEGISandData/COVID-19) for their publicly available data, which are used for these visualizations.

## directory

Note: NYT refers to the New York Times. JH refers to John Hopkins.

* map-anim.R: reformat NYT data for plotting and animating a time-lapse of reported COVID-19 cases throughout the US using gganimate.
* jh-example.R: script file showing examples of dplyr to format/select data from the JH data and plotting.
* log-log.R: visualize weekly deltas in cases vs cumulative cases with log-log scales using the JH data.
* cases-vis.R: visualize cumulative US cases over time, with linear-linear or log-linear scales.
* funcs.R: helper functions for map-anim.R.
* fips-lookup.csv: lookup table for fips location codes to find lat-long for map-anim.R.
* nyt-data/: store NYT data.
* jh-data/: store JH data.
* sample-output/: store example video anim-test.mp4, rendered in map-anim.R
