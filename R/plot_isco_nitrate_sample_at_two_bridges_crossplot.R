plot_isco_nitrate_sample_at_two_bridges_crossplot <- function(event) {
  ggplot(data = sample_nitrate_data %>% filter(Event == event), aes(x = USGS, y = ISCO)) +
    geom_point() +
    labs(x = "Nitrate USGS (mg/L)",
         y = "Nitrate ISCO (mg/L)",
         title = glue::glue("Event {event} Crossplot"))
    
}
