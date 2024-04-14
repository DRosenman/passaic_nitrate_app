plot_nitrate_data <- function(nitrate_df, rain_df, add_spline_curve = FALSE, add_best_fit_line = FALSE, point_size = 1, max_days_before_simplifying = 30) {
  if(as.integer(max(nitrate_df$date_time) - min(nitrate_df$date_time)) > max_days_before_simplifying) {
    nitrate_df <- nitrate_df %>% group_by(date_time = as_datetime(as.Date(date_time))) %>% 
      summarise(mean = mean(value, na.rm = TRUE), 
                min = min(value, na.rm = TRUE), 
                max = max(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_longer(cols = c("mean", "min", "max"),
                   names_to = "stat",
                   values_to = "value")
  }

  plot <-  ggplot() +
    geom_bar(data = rain_df, aes(x = date_time, y = - 1 * value), width = 0.1, color = 'blue', stat = "identity", na.rm = TRUE) +
    geom_point(data = nitrate_df, aes(x = date_time, y = value), size = point_size) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7), limits = c(-2,7), sec.axis = sec_axis(~.*-1, breaks = c(0,1,2), name = "rain in./day")) +
    ylab("nitrate, mg/L") +
    xlab("Date") +
    background_grid(major = "xy", minor = "xy") +
    ggtitle("Passaic")
  
  if (add_spline_curve) {
    plot <- plot + geom_smooth(data = nitrate_df, aes(x = date_time, y = value), color = "green")
    
  }
  if (add_best_fit_line) {
    plot <- plot + geom_smooth(data = nitrate_df, aes(x = date_time, y = value), method = "lm", color = "red") #+
    
  }
  
  # Assuming date_time and Date columns are in Date or POSIXt format
  return(plot)
  
}

# nitrate <- read_nitrate_data()
# rain <- read_rain_data()
# 
# p1 <- plot_nitrate(nitrate, rain)
# p2 <- plot_nitrate2(nitrate %>% filter(date_time  < '2009-08-15') , rain %>% filter(date_time < '2009-08-15'))
# 
# p1 + p2 + plot_layout(ncol = 1)
