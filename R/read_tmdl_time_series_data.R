read_tmdl_time_series_data <- function(station, parameter, data_type, stat, date_range = c("1999-10-01", "2003-12-01")) {
  if(stat == "max and min") {
    tmdl_model_data %>% 
      filter(Station == .env$station) %>% 
      collect() %>% filter(Date >= ymd(.env$date_range[[1]]), Date <= ymd(.env$date_range[[2]]),
                           parameter == .env$parameter, data_type == .env$data_type) %>% 
      select(parameter, Date, data_type, max, min) %>%
      pivot_longer(cols = c("max", "min"), names_to = "Stat", values_to = "y") %>% 
      select(parameter, Date, data_type, y, Stat)
  } else {
    tmdl_model_data %>% 
      filter(Station == .env$station) %>% 
      collect() %>% filter(Date >= ymd(.env$date_range[[1]]), Date <= ymd(.env$date_range[[2]]),
                           parameter == .env$parameter, data_type == .env$data_type) %>% 
      select(parameter, Date, data_type, y = .env$stat) %>% 
      mutate(Stat = .env$stat)
  }
  
}



