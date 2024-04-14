read_usgs_flow_frequency_summary <- function() {
  flow_station_summaries %>% ungroup() %>% mutate(Site = factor(Site), Water_Year = factor(Water_Year),
                                                  yearly_flow = round(yearly_flow, 1)) %>% dplyr::rename(Station = Site, `Avg Flow (ft/s)` = yearly_flow) %>% 
    pivot_wider(names_from = "Station", values_from = `Avg Flow (ft/s)`)
}
