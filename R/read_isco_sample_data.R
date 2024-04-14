read_isco_sample_data <- function(sample_event, sample_interval, sample_parameter, slider_type = c("single", "six"),
                                  station_1 = NULL, station_2 = NULL, station_3 = NULL, station_4 = NULL, station_5 = NULL, station_6 = NULL) { #slider_type single = 'Uniformly', 'six' = 'One Slider Per Station'
  sample_event <- as.integer(sample_event)
  slider_type = rlang::arg_match(slider_type)
  if (slider_type == "single") {
    sample_data %>%
      filter(Event == sample_event, Interval == sample_interval, Parameter == sample_parameter) %>%
      left_join(sample_sp %>% select(`Site ID` = SITE_NO, lng = Long, lat = Lat))
  } else if (slider_type == "six") {
    sample_data %>%
      filter(
          Event == sample_event, Parameter == sample_parameter,
          (`Station Name` == "Passaic River at Eagle Rock" & Interval == station_1) | (`Station Name` == "Passaic River at Pine Brook" & Interval == sample_station_2) | 
            (`Station Name` == "Passaic River at Two Bridges" & Interval == station_3) | 
            (`Station Name` == "Pompton River at Two Bridges" & Interval == station_4) | 
            (`Station Name` == "Rockaway River on Bloomfield Ave. Montville" & Interval == station_5) |
            (`Station Name` == "Whippany River at Pine Brook" & Interval == station_6)
        ) %>%
      left_join(sample_sp %>% select(`Site ID` = SITE_NO, lng = Long, lat = Lat))
    }
 
}

#initial_isco_sample_data <- dput(read_isco_sample_data(sample_event = 1, sample_interval = 1, sample_parameter = "NO3+NO2 (mg/L)", slider_type = "single"))
