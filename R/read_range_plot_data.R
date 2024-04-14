read_range_plot_data <- function(location = c("Passaic", "Pompton"), years = nitrate_data_years, frac_to_keep = 1) {
  location <- rlang::arg_match(location)
  if(location == "Passaic") {
    if(all(nitrate_data_years %in% years)) {
      df <- nitrate_data
    } else {
      df <- nitrate_data %>% filter(Year %in% as.integer(years))
    }
  } else { #where location = 'Pompton'
    if(all(nitrate_data_years %in% years)) {
      df <- nitrate_data %>% select(agency_cd, site_no, date_time, nitrate = nitrate_left,
                              code, time_zone, Year, Month, Day, Hour)
    } else {
      df <- nitrate_data %>% filter(Year %in% as.integer(years)) %>% select(agency_cd, site_no, date_time, nitrate = nitrate_left,
                                                                      code, time_zone, Year, Month, Day, Hour)
    }
  }
  if(is.null(frac_to_keep) | frac_to_keep == 1){
    return(df)
  } else {
    return(df %>% sample_frac(size = frac_to_keep))
  }

}










# read_range_plot_data <- function(location = c("Passaic", "Pompton"), years = nitrate_data_years, sample_frac = .25) {
#   location <- rlang::arg_match(location)
#   if(location == "Passaic") {
#     if(all(nitrate_data_years %in% years)) {
#       return(nitrate_data) 
#     } else {
#       nitrate_data %>% filter(Year %in% as.integer(years))
#     }
#   } else { #where location = 'Pompton'
#     if(all(nitrate_data_years %in% years)) {
#       nitrate_data %>% select(agency_cd, site_no, date_time, nitrate = nitrate_left, 
#                               code, time_zone, Year, Month, Day, Hour)
#     } else {
#       nitrate_data %>% filter(Year %in% as.integer(years)) %>% select(agency_cd, site_no, date_time, nitrate = nitrate_left, 
#                                                                       code, time_zone, Year, Month, Day, Hour)
#     }
#   } 
#   
# }
