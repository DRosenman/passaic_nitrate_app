sample_data_summary <- function(events = "All", stations = "All", parameters = "All") {
  if ("All" %in% stations) {
    stations <- sort(unique(sample_data$`Station Name`))
  }
  
  if ("All" %in% events) {
    events <- c(1,2,3,4)
  }
  
  if ("All" %in% parameters) {
    parameters <- sort(unique(sample_data$Parameter))
  }
  
  sample_data %>% 
    filter(`Station Name` %in% stations, Event %in% events, Parameter %in% parameters) %>% 
    group_by(`Station Name`, Event, Parameter) %>% 
    summarize(Measurements = n(), Mean = mean(Result, na.rm = TRUE),
              Median = median(Result, na.rm = TRUE),
              Max = max(Result, na.rm = TRUE), 
              Min = min(Result, na.rm = TRUE),
              `25%` = quantile(Result, .25, na.rm = TRUE),
              `75%` = quantile(Result, .50, na.rm = TRUE),
              SD = sd(Result, na.rm = TRUE)) %>% 
    arrange(`Station Name`, Parameter, Event)
  
}
