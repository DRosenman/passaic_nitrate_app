library(shinycssloaders)
library(patchwork)
library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)
library(plotly)
library(dataRetrieval)
library(tools)
library(readr)
library(fst)
library(leaflet)
library(readxl)
library(openxlsx)
library(cowplot)
library(httr)
library(purrr)
library(stringr)
library(tidyr)
library(readxl)
library(stringr)
library(lubridate)
library(rlang)
library(arrow)
library(dtplyr)
library(shinyWidgets)
library(rlang)
`%ni%` <- Negate(`%in%`)

# file_load_code <- function(file) {
#   path <- glue::glue("data/{file}")
#   if(str_detect(file, "fst")) {
#     name <- str_remove(file, "\\.fst")
#     return(glue::glue("{name} <- fst::read.fst('{path}')"))
#     
#   } else {
#     name <- str_remove(file, "\\.RDS")
#     return(glue::glue("{name} <- readRDS('{path}')"))
#   }
#   
#   
# }
# 
# files <- list.files("data")
# 
# code_all <- map_chr(files, file_load_code)

tmdl_model_data <- open_dataset("data/tmdl")
all_sample_data_series <- fst::read.fst('data/all_sample_data_series.fst')
data_collection_shp <- readRDS('data/data_collection_shp.RDS')
discharge_data <- fst::read.fst('data/discharge_data.fst')
discharge_levels <- readRDS('data/discharge_levels.RDS')
discharge_locations <- readRDS('data/discharge_locations.RDS')
discharge_map <- readRDS('data/discharge_map.RDS')
discharge_params <- readRDS('data/discharge_params.RDS')
discharge_sections <- fst::read.fst('data/discharge_sections.fst')
event_details <- readRDS('data/event_details.RDS')


flow_frequency_data <- fst::read.fst('data/flow_frequency_data.fst')
flow_station_summaries <- fst::read.fst('data/flow_station_summaries.fst')
gage_data <- fst::read.fst('data/gage_data.fst')
intakes <- fst::read.fst('data/intakes.fst')
model_nodes <- readRDS('data/model_nodes.RDS')
model_nodes_df <- fst::read.fst('data/model_nodes_df.fst')
model_nodes_labels <- readRDS('data/model_nodes_labels.RDS')
nitrate_data_display <- fst::read.fst("data/nitrate_data2.fst")
nitrate_data <- fst::read.fst('data/nitrate_data.fst') %>% as_tibble()

old_tmdl_df_names <- readRDS('data/old_tmdl_df_names.RDS')
parameters <- readRDS('data/parameters.RDS')
quakes1 <- fst::read.fst('data/quakes1.fst')
rain_data <- fst::read.fst('data/rain_data.fst')
rec_water_data <- fst::read.fst('data/rec_water_data.fst')
sample_data <- fst::read.fst('data/sample_data.fst')
sample_data_parameters <- readRDS('data/sample_data_parameters.RDS')
sample_data_stations <- c("Passaic River at Eagle Rock (PA-5)" = "Passaic River at Eagle Rock", "Passaic River at Pine Brook (PA-6)" = "Passaic River at Pine Brook",
                          "Passaic River at Two Bridges (PA-7)" = "Passaic River at Two Bridges", "Pompton River at Two Bridges (POM-3)" = "Pompton River at Two Bridges",
                          "Rockaway River on Bloomfield Ave. Montville (RO-2)" = "Rockaway River on Bloomfield Ave. Montville",
                          "Whippany River at Pine Brook (WI-3)" = "Whippany River at Pine Brook")
sample_discharger_flow <- fst::read.fst('data/sample_discharger_flow.fst')
sample_discharger_locations <- readRDS('data/sample_discharger_locations.RDS')
sample_high <- readRDS('data/sample_high.RDS')
sample_highest <- readRDS('data/sample_highest.RDS')
sample_intake <- readRDS('data/sample_intake.RDS')
sample_low <- readRDS('data/sample_low.RDS')
sample_nitrate_data <- readRDS('data/sample_nitrate_data.RDS')
sample_sp <- readRDS('data/sample_sp.RDS')
stations <- readRDS('data/stations.RDS')
tmdl_location_info <- fst::read.fst('data/tmdl_location_info.fst')
tmdl_stations <- readRDS('data/tmdl_stations.RDS')
treatment_plant <- fst::read.fst('data/treatment_plant.fst')
initial_isco_sample_data <- fst::read.fst("data/initial_isco_sample_data.fst")  %>% 
  mutate(`Station Name` = str_c(`Station Name`, ", ", `TMDL ID`))
initial_isco_sample_discharger_data <- fst::read.fst("data/initial_isco_sample_discharger_data.fst")
initial_isco_sample_color_palette <- colorNumeric(palette = "Blues", domain = initial_isco_sample_data$Result)
initial_isco_sample_discharger_color_palette <- colorNumeric(palette = "OrRd", domain = initial_isco_sample_discharger_data$Result)

tmdl_parameters_all <- c("B-Algae Light", "B-Algae Nut", "Depth m", "DO DEF mg/l", "DO mg/l", 
                     "flow", "Flow  Out cms", "NH3+NO3 mg/l", "NH4 mg/l", "NO3 mg/l", 
                     "OPO4 mg/l", "OPO4+Phyt mg/l", "SOD g/m2/day", "Temp ?c", "Tot Chla ug/l", 
                     "Volume cm")

timeseries_parameters <- tmdl_parameters_all[tmdl_parameters_all %in% c("Actual DT days", "B-Algae Light", "B-Algae Nut", "BOD 1 Decay /d", 
                           "data_type", "Depth m", "Detritus mg/l", "DO DEF mg/l", "DO Max mg/l", 
                           "DO mg/l", "DO Min mg/l", "DO Sat %", "DO Sat Conc", "Hydra KA /day", 
                           "KA per day", "Light Bottom", "Light Limit", "Light Top", "N Limit", 
                           "NH3+NO3 mg/l", "NH4 mg/l", "NO3 mg/l", "OPO4 mg/l", "OPO4+Phyt mg/l", 
                           "Phyt Death /day", "Phyt DO Consump", "Phyt DO Prod.", "Phyt Nutr Limit", 
                           "Phyto Growth /d", "Phyto mg/l Carb", "Res Time days", "SOD g/m2/day", 
                           "Temp ?c", "Tot Chla ug/l", "Tot Disp. m2/se", "Tot Light", "Tot N mg/l", 
                           "Tot P mg/l", "TotInorgN mg/l", "uCBOD-1 mg/l", "Vel. m/sec", 
                           "Volume cm")] #sort(old_tmdl_df_names[-c(1,2,46,47)])

tmdl_cross_parameters <- tmdl_parameters_all[tmdl_parameters_all %in% c("Actual DT days", "B-Algae Light", "B-Algae Nut", "BOD 1 Decay /d", 
                           "data_type", "Depth m", "Detritus mg/l", "DO DEF mg/l", "DO Max mg/l", 
                           "DO mg/l", "DO Min mg/l", "DO Sat %", "DO Sat Conc", "flow", 
                           "Flow  Out cms", "Hydra KA /day", "KA per day", "Light Bottom", 
                           "Light Limit", "Light Top", "N Limit", "NH3+NO3 mg/l", "NH4 mg/l", 
                           "NO3 mg/l", "OPO4 mg/l", "OPO4+Phyt mg/l", "Org-N mg/l", "Org-P - Phyt mg", 
                           "Org-P mg/l", "P Limit", "Periphyton ug/l", "Phyt Death /day", 
                           "Phyt DO Consump", "Phyt DO Prod.", "Phyt Nutr Limit", "Phyto Growth /d", 
                           "Phyto mg/l Carb", "Res Time days", "SOD g/m2/day", "Temp ?c", 
                           "Tot Chla ug/l", "Tot Disp. m2/se", "Tot Light", "Tot N mg/l", 
                           "Tot P mg/l", "TotInorgN mg/l", "uCBOD-1 mg/l", "Vel. m/sec", 
                           "Volume cm")]


nitrate_data_years <- seq(from = min(nitrate_data$Year, na.rm = TRUE), to = max(nitrate_data$Year, na.rm = TRUE))


isco_nitrate_at_two_bridges_plots <- map(c("data/plot_isco_nitrate_sample_at_two_bridges_1.RDS", "data/plot_isco_nitrate_sample_at_two_bridges_2.RDS", "data/plot_isco_nitrate_sample_at_two_bridges_3.RDS", "data/plot_isco_nitrate_sample_at_two_bridges_4.RDS"), readRDS)
