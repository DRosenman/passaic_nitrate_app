plot_monthly_range_plot <- function(df) {
  ggplot(data = df, aes(x = factor(Month), y = nitrate)) +
    geom_boxplot() +
    theme(legend.position = "none") +
    xlab("Month") +
    ylab("Nitrate, mg/L") +
    scale_x_discrete(labels = month.abb) +
    scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1))
}
