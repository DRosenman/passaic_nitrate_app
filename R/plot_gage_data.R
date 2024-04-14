plot_gage_data <- function(gage_df, point_size = 1, max_days_before_simplifying = 30) {
  if(as.integer(max(gage_df$date_time) - min(gage_df$date_time)) > max_days_before_simplifying) {
    gage_df <- gage_df %>% group_by(Date = as_datetime(as.Date(date_time))) %>% 
      summarise(mean = mean(gage_height, na.rm = TRUE), 
                min = min(gage_height, na.rm = TRUE), 
                max = max(gage_height, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_longer(cols = c("mean", "min", "max"),
                   names_to = "stat",
                   values_to = "gage_height")
  }
  
  ggplot(data = gage_df, aes(x = Date, y = gage_height)) + 
    geom_point(size = point_size) +
    ylab("gage height, ft") + 
    xlab("Date") +
    background_grid(major = "xy", minor = "xy") + 
    ggtitle("Gage Height")
}
