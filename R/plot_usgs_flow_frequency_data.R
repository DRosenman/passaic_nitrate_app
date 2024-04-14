plot_usgs_flow_frequency_data <- function() {
  ggplot(flow_station_summaries, aes(x = yearly_flow, color = Site)) +
    stat_ecdf() +
    coord_flip() +
    scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750)) +
    scale_y_continuous(
      breaks = c(0, 0.01, 0.05, 0.10, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
      labels = c(0, 1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    ) +
    labs(x = "Flow ft/s",
         y = "Percentile",
         title = "Daily Average Flow (by year)")
}
