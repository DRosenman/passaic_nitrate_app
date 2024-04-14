plot_isco_nitrate_sample_at_two_bridges <- function(event) {
  ggplot(data = sample_nitrate_data <- sample_nitrate_data %>%
           filter(Event == event) %>%
           mutate(difference = USGS - ISCO)) +
    geom_point(aes(x = Datetime, y = ISCO, color = "ISCO")) +
    geom_point(aes(x = Datetime, y = USGS, color = "USGS")) +
    geom_line(aes(x = Datetime, y = difference, color = "difference")) +
    geom_point(aes(x = Datetime, y = difference)) +
    scale_colour_manual(name = "", values = c("ISCO" = "blue", "USGS" = "red", "difference" = "green"), labels = c("USGS - ISCO", "USGS", "ISCO")) +
    labs(x = "Datetime",
         y = "Nitrate (mg/L)",
         title = glue::glue("Passaic Nitrate at Two Bridges: Event {event}")) +
    scale_x_datetime(date_breaks = "1 day") # , limits = c(as.POSIXct("2018-06-11"), as.POSIXct("2018-06-14")))
}

# save_plot_isco_nitrate_sample_at_two_bridges <- function(event) {
#   plot_isco_nitrate_sample_at_two_bridges(event)
#   cowplot::ggsave2(filename = glue::glue("www/plot_isco_nitrate_sample_at_two_bridges_{event}.png"))
#   ggsave(filename = glue::glue("www/plot_isco_nitrate_sample_at_two_bridges_{event}.jpg"), width = 6, height = 2)
#  }
# 
#  for(i in 1:4) {
#    save_plot_isco_nitrate_sample_at_two_bridges(i)
#  }
# 
#  for(i in 1:4) {
#    p2b_plot <- plot_isco_nitrate_sample_at_two_bridges(i)
#    saveRDS(p2b_plot, glue::glue("data/plot_isco_nitrate_sample_at_two_bridges_{i}.RDS"))
#  }


