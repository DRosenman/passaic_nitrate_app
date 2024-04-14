plot_tmdl_time_series_data <- function(df) {
  parameter <- df$parameter[[1]]
  if(all(c("max", "min") %in% df$Stat)) {
   ggplot(data = df) + 
      geom_line(aes(x = Date, y = y, color = Stat)) + 
      scale_colour_manual(name = '', values = c("max" = "blue", "min" = "red"), labels = c("Max", "Min"))  +
      xlab("Date") +
      ylab(parameter) +
      background_grid(major = "xy", minor = "xy")
      
  }  else {
    stat <- df$Stat[[1]]
    y_lab <- glue::glue("{parameter} ({stat})")
    ggplot(data = df) + 
      geom_line(aes(x = Date, y = y)) + 
      xlab("Date") +
      ylab(y_lab) +
      background_grid(major = "xy", minor = "xy")
  }
} 
