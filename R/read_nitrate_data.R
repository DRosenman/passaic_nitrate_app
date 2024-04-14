read_nitrate_data <- function(date_range = c("2009-07-29", "2019-11-21")) {
  nitrate_data %>% filter(between(as.Date(date_time), as_date(date_range[[1]]), as_date(date_range[[2]])), nitrate >= 0) %>% 
    dplyr::rename(value = nitrate) %>% 
    mutate(parameter = "nitrate")
}




read_nitrate_data_dt <- function(date_range = c("2009-07-29", "2019-11-21")) {
  lazy_dt(nitrate_data) %>% filter(between(as.Date(date_time), as_date(date_range[[1]]), as_date(date_range[[2]])), nitrate >= 0) %>% 
    dplyr::rename(value = nitrate) %>% 
    mutate(parameter = "nitrate") %>% 
    as_tibble()
}
