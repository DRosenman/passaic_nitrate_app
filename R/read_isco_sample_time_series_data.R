read_isco_sample_time_series_data <- function(sample_event, sample_stations = sample_data_stations,
                                         sample_parameters = sample_data_parameters) {
  sample_data %>% 
    filter(Event == sample_event,
           Parameter %in% sample_parameters,
           `Station Name` %in% sample_stations
           ) %>% 
    left_join(sample_sp %>% select(`Site ID` = SITE_NO, lng = Long, lat = Lat), by = join_by(`Site ID`)) 
  
   
}

# test <- read_isco_sample_time_series_data(1,unique(sample_data$`Station Name`), unique(sample_data$Parameter)) %>% as_tibble()
# microbenchmark::microbenchmark(
#   sample_parameter_plot(parameter = test$Parameter[1], test),
#   purrr::map(unique(test$Parameter), \(x) sample_parameter_plot(x, test))
#   )
