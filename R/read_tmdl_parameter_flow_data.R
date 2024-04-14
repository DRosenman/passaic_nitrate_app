read_tmdl_parameter_flow_data <- function(station, parameter, data_type, stat, date_range) {
  if (stat == "max and min") {
    tmdl_model_data %>% 
      filter(Station == station) %>% 
      collect() %>% filter(Date >= ymd(date_range[[1]]), Date <= ymd(date_range[[2]]),
                           parameter %in% c("flow", .env$parameter), data_type == .env$data_type) %>% 
      select(Date, parameter, min, max) %>% 
      group_by(parameter) %>% 
      pivot_longer(cols = c(min, max), names_to = "Stat", values_to = "Value") %>% 
      pivot_wider(names_from = parameter, values_from = Value)
    
  } else {
    tmdl_model_data %>% 
        filter(Station == station) %>% 
        collect() %>% filter(Date >= ymd(date_range[[1]]), Date <= ymd(date_range[[2]]),
                             parameter %in% c(.env$parameter, "flow"), data_type == .env$data_type) %>% 
        select(Date, parameter, .env$stat) %>% 
        pivot_wider(names_from = "parameter", values_from = .env$stat) %>% 
      mutate(Stat = .env$stat) %>% 
      select(Date, Stat, flow, .env$parameter)
      
    }
  
}

