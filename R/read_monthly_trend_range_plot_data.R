read_monthly_trend_range_plot_data <- function(location = c("Passaic", "Pompton"), month, years = nitrate_data_years) {
  location <- rlang::arg_match(location)
  if (location == "Passaic") {
    nitrate_data %>% filter(Year %in% years, Month == as.numeric(month))
  } else if (location == "Pompton") {
    nitrate_data %>%
      filter(Year %in% years, Month == as.numeric(month)) %>%
      select(-nitrate) %>%
      dplyr::rename(nitrate = nitrate_left)
  }
}
