plot_nitrate_pompton_data <- function(nitrate_df, add_spline_curve = FALSE, add_best_fit_line = FALSE, point_size = 1, max_days_before_simplifying = 30) {
  if(as.integer(max(nitrate_df$date_time) - min(nitrate_df$date_time)) > max_days_before_simplifying) {
    nitrate_df <- nitrate_df %>% group_by(date_time = as_datetime(as.Date(date_time))) %>% 
      summarise(mean = mean(nitrate_left, na.rm = TRUE), 
                min = min(nitrate_left, na.rm = TRUE), 
                max = max(nitrate_left, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_longer(cols = c("mean", "min", "max"),
                   names_to = "stat",
                   values_to = "nitrate_left")
  }
 
  plot <-  ggplot() +
    geom_point(data = nitrate_df, aes(x = date_time, y = nitrate_left), size = point_size) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7), limits = c(0,7)) +
    ylab("nitrate, mg/L") +
    xlab("Date") +
    background_grid(major = "xy", minor = "xy") +
    ggtitle("Pompton")
  
  if (add_spline_curve) {
    plot <- plot + geom_smooth(data = nitrate_df, aes(x = date_time, y = nitrate_left), color = "green")
    
  }
  if (add_best_fit_line) {
    plot <- plot + geom_smooth(data = nitrate_df, aes(x = date_time, y = nitrate_left), method = "lm", color = "red") #+
    
  }
  
  # Assuming date_time and Date columns are in Date or POSIXt format
  return(plot)
  
}



