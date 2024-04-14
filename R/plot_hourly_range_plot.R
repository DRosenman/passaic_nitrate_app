plot_hourly_range_plot <- function(df) {
  ggplot(data = df, aes(x = factor(Hour), y = nitrate)) +
    geom_boxplot() +
    theme(legend.position = "none") +
    xlab("Hour (EST)") +
    ylab("Nitrate, mg/L") +
    scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1))
}