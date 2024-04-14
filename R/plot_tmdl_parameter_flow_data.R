plot_tmdl_parameter_flow_data <- function (df) {
  if (all(c("min","max") %in% df$Stat)) {
    y_param <- names(df)[4]
    names(df)[4] <- "y"
    ggplot(data = df) + geom_point(aes(flow, y, color = Stat)) + 
      scale_colour_manual(name = "", values = c(max = "blue", 
                                                min = "red"), labels = c("Max", "Min")) + xlab("flow cm/s") + 
      ylab(y_param) + background_grid(major = "xy", minor = "xy")
  }
  else {
    y_param <- names(df)[4]
    names(df)[4] <- "y"
    stat <- df$Stat[[1]]
    y_axis_label <- glue::glue("{y_param} ({stat})")
    
    ggplot(data = df, aes(flow, y)) + geom_point() + xlab("flow cm/s") + 
      ylab(y_axis_label) + background_grid(major = "xy", minor = "xy")
  }
}
