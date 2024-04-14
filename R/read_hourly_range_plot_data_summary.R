read_hourly_range_plot_data_summary <- function(range_plot_df) {
  range_plot_df %>% 
    group_by(Hour) %>%
    summarise(Measurements = n(), Mean = mean(nitrate, na.rm = TRUE), Median = median(nitrate, na.rm = TRUE), 
              Minimum = min(nitrate, na.rm = TRUE), Maximum = max(nitrate, na.rm = TRUE), `25%` = quantile(nitrate, .25, na.rm = TRUE), 
              `75%` = quantile(nitrate, .75, na.rm = TRUE), SD = sd(nitrate, na.rm = TRUE))
}
