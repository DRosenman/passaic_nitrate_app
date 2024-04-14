read_isco_overall_sample_time_series_data <- function(parameter, stations) {
  all_sample_data_series %>%
    filter(Parameter == parameter, `Station Name` %in% stations)
}

