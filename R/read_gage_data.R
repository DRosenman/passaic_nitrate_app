read_gage_data <- function(date_range = c("2009-07-29", "2019-11-21")) {
  gage_data %>% filter(between(Date, ymd(date_range[[1]]), ymd(date_range[[2]])))
}
