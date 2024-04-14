read_isco_sample_discharger_data <- function(sample_event, sample_parameter) {
  if (sample_parameter %ni% c("Chl a (ug/L)", "NH3 (mg/L)", "TSS  (mg/L)") && sample_event != 3) {
    if (sample_event == 1) {
      sample_intake %>%
        filter(Date == ymd("2018-06-14"), Parameter == sample_parameter) %>%
        left_join(sample_discharger_locations)
    } else if (sample_event == 2) {
      sample_intake %>%
        filter(Date == ymd("2018-06-28"), Parameter == sample_parameter) %>%
        left_join(sample_discharger_locations)
    } else if (sample_event == 4) {
      sample_intake %>% filter(Date %in% ymd(c("2018-08-07", "2018-08-08", "2018-08-08")), Parameter == sample_parameter)
    }
  } else {
    NULL
  }
}
# 
# initial_isco_sample_discharger_data <- read_isco_sample_discharger_data(1, 'NO3+NO2 (mg/L)')
# #fst::write.fst(initial_isco_sample_discharger_data, 'data/initial_isco_sample_discharger_data.fst', compress = 100)
