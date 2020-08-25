library(transformr)

pl2 <- ggplot(data = daily, aes(x=date.day, y=cases)) + 
  theme_light() + 
  geom_line(color="Red", size=5) +
  scale_x_date() +
  scale_y_continuous(breaks = seq(0, 1.5e6, 0.25e6), labels = c("0", "250k", "500k", "750k", "1m", "1.25m", "1.5m")) + 
  # transition_reveal(date.day)

anim_save(filename = here("sample-output", paste0("anim-test2-", movie_date, ".mp4")),
          animation=pl2, renderer=av_renderer(), nframes=length(unique(data$date)), fps=2, detail=2, width=1920, height=1080)
