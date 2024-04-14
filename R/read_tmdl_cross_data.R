read_tmdl_cross_data <- function(station, parameter_x, parameter_y, data_type, stat, date_range = c("1999-10-01", "2003-12-01")) {
  if(parameter_x == parameter_y) {
    if(stat == "max and min") {
      tmdl_model_data %>% 
        filter(Station == .env$station) %>% 
        collect() %>% filter(Date >= ymd(.env$date_range[[1]]), Date <= ymd(.env$date_range[[2]]),
                             parameter == .env$parameter_x, data_type == .env$data_type) %>% 
        select(Date, parameter, data_type, max, min) %>%
        pivot_longer(cols = c("max", "min"), names_to = "Stat", values_to = parameter_x) %>% 
        select(Date, data_type, Stat, .env$parameter_x)
        #pivot_wider(names_from = 'parameter', values_from = 'y') %>% 
        #select(Date, data_type, Stat, .env$parameter_x)
    } else {
      data <- tmdl_model_data %>% 
        filter(Station == .env$station) %>% 
        collect() %>% filter(Date >= ymd(.env$date_range[[1]]), Date <= ymd(.env$date_range[[2]]),
                             parameter == .env$parameter_x, data_type == .env$data_type) %>% 
        mutate(Stat = .env$stat) %>% 
        select(Date, data_type, Stat, .env$stat)
      names(data)[[4]] <- parameter_x
      data
    }
  } else {
    if(stat == "max and min") {
      tmdl_model_data %>% 
        filter(Station == .env$station) %>% 
        collect() %>% filter(Date >= ymd(.env$date_range[[1]]), Date <= ymd(.env$date_range[[2]]),
                             parameter %in% c(.env$parameter_x, .env$parameter_y), data_type == .env$data_type) %>% 
        select(Date, parameter, data_type, max, min) %>%
        pivot_longer(cols = c("max", "min"), names_to = "Stat", values_to = "y") %>% 
        pivot_wider(names_from = 'parameter', values_from = 'y') %>% 
        select(Date, data_type, Stat, .env$parameter_x, .env$parameter_y)
    } else {
      tmdl_model_data %>% 
        filter(Station == .env$station) %>% 
        collect() %>% filter(Date >= ymd(.env$date_range[[1]]), Date <= ymd(.env$date_range[[2]]),
                             parameter %in% c(.env$parameter_x, .env$parameter_y), data_type == .env$data_type) %>% 
        mutate(Stat = .env$stat) %>% 
        select(Date, parameter, data_type, Stat, y = .env$stat) %>% 
        pivot_wider(names_from = 'parameter', values_from = 'y') %>% 
        select(Date, data_type, Stat, .env$parameter_x, .env$parameter_y)
    }
  }
  
  
}



