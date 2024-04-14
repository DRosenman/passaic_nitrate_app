plot_annual_range_plot <- function(df) {
  ggplot(data = df, aes(x = factor(Year), y = nitrate)) +
    geom_boxplot() +
    theme(legend.position="none") +
    xlab("Year") +
    ylab("Nitrate, mg/L") +
    scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1))
}




plot_annual_range_plot_plotly <- function(df) {
  # Convert 'Year' to a factor if it isn't already
  df$Year <- as.factor(df$Year)
  df <- df %>% sample_frac(sample_frac)
  # Create the box plot
  fig <- plot_ly(data = df, x = ~Year, y = ~nitrate, type = "box",
                 boxpoints = "outliers", marker = list(color = 'blue'))  # Set a specific color for all boxes
  
  # Update layout to add labels and adjust y-axis breaks
  fig <- fig %>% layout(
    title = "Annual Nitrate Levels",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Nitrate, mg/L",
                 dtick = 1, range = c(0, 10)),
    showlegend = FALSE
  )
  
  # Return the plotly figure
  fig
}


#data <- read_range_plot_data()
# plot_annual_range_plot_plotly(data)
