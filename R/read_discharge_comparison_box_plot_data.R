read_discharge_comparison_box_plot_data <- function(by = c("Year", "Month"), range = 2002:2018) {
  by <- rlang::arg_match(by)
  if(by == 'Year') {
    discharge_data %>%
      filter(Parameter == "Nitrogen, Nitrate Total (as N)", Year %in% range) %>%
      group_by(Location, Year) %>%
      summarise(Quantity.Avg = mean(Quantity.Avg, na.rm = TRUE)) %>%
      filter(!is.nan(Quantity.Avg)) %>%
      left_join(discharge_sections, by = join_by(Location))
  } else { #by = 'Month'
    discharge_data %>%
      filter(Parameter == "Nitrogen, Nitrate Total (as N)", Month %in% range) %>%
      group_by(Location, Month, Year) %>%
      summarise(Quantity.Avg = mean(Quantity.Avg, na.rm = TRUE)) %>%
      filter(!is.nan(Quantity.Avg)) %>%
      left_join(discharge_sections, by = join_by(Location))
  }
  
  
  
}

# read_discharge_comparison_box_plot_data2 <- function(by = c("Year", "Month"), range = 2002:2018) {
#   by <- rlang::arg_match(by)
#   if(all(2002:2018 %in% range) || all(1:12 %in% range)) {
#     discharge_data %>%
#       filter(Parameter == "Nitrogen, Nitrate Total (as N)") %>%
#       group_by_("Location", by) %>%
#       summarise(Quantity.Avg = mean(Quantity.Avg, na.rm = TRUE)) %>%
#       filter(!is.nan(Quantity.Avg)) %>%
#       left_join(discharge_sections)
#   } else {
#     if (by == "Year") {
#       discharge_data %>%
#         filter(Parameter == "Nitrogen, Nitrate Total (as N)") %>%
#         filter_at(by, any_vars(. %in% as.numeric(range))) %>%
#         group_by_("Location", by) %>%
#         summarise(Quantity.Avg = mean(Quantity.Avg, na.rm = TRUE)) %>%
#         filter(!is.nan(Quantity.Avg)) %>%
#         arrange(desc(Location)) %>%
#         left_join(discharge_sections)
#     } else {
#       discharge_data %>%
#         filter(Parameter == "Nitrogen, Nitrate Total (as N)") %>%
#         filter_at(by, any_vars(. %in% as.numeric(range))) %>%
#         group_by_("Location", "Month", "Year") %>%
#         summarise(Quantity.Avg = mean(Quantity.Avg, na.rm = TRUE)) %>%
#         filter(!is.nan(Quantity.Avg)) %>%
#         arrange(desc(Location)) %>%
#         left_join(discharge_sections)
#     }
#   }
# }
