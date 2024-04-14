map_isco_sampling <- function() {
  leaflet() %>%
    addProviderTiles(group = "ESRI (default)", providers$Esri) %>%
    addProviderTiles(group = "ESRI NatGeoWorldMap", providers$Esri.NatGeoWorldMap) %>%
    addProviderTiles(group = "ESRI WorldImagery", providers$Esri.WorldImagery) %>%
    addProviderTiles(group = "OpenStreetMap", providers$OpenStreetMap) %>%
    addMarkers(
      group = "Dischargers (not sampled)", lng = discharge_map@coords[, 1], lat = discharge_map@coords[, 2], popup = discharge_map@data$FAC_LABEL, label = discharge_map@data$FAC_LABEL, 
      layerId = discharge_map@data$FAC_LABEL,
      icon = leaflet_icons["square"]
    ) %>% # label = discharge_map@data$FAC_LABEL
    addMarkers(group = "Intakes", icon = leaflet_icons["star"], lng = c(-74.23467, -74.27120), lat = c(40.88504, 40.89998), 
               popup = c("Passaic Intake", "Pompton Intake"), label = c("Passaic Intake", "Pompton Intake")) %>%
    fitBounds(lng1 = -74.3468, lat1 = 40.8979, lng2 = -74.269, lat2 = 40.81) %>%
    addLayersControl(
      baseGroups = c("ESRI (default)", "ESRI NatGeoWorldMap", "ESRI WorldImagery", "OpenStreetMap"),
      overlayGroups = c("Dischargers (not sampled)", "Intakes"),
      options = layersControlOptions(collapsed = F)
    ) %>%
    hideGroup("Dischargers (not sampled)") %>%
    hideGroup("Intakes") %>% 
    clearControls() %>%
    clearControls() %>%
    addCircleMarkers(
      radius = 10, fillOpacity = 1, opacity = 1, stroke = TRUE, lng = initial_isco_sample_data$lng, lat = initial_isco_sample_data$lat,
      popup = paste0(initial_isco_sample_data$Parameter, ": ", initial_isco_sample_data$Result), label = initial_isco_sample_data$`Station Name`, 
      color = "black", weight = 2, fillColor = initial_isco_sample_color_palette(initial_isco_sample_data$Result)
    ) %>%
    addRectangles(
      fillOpacity = 1, opacity = 1, stroke = TRUE, lng1 = initial_isco_sample_discharger_data$lon, lat1 = initial_isco_sample_discharger_data$lat, lng2 = initial_isco_sample_discharger_data$lon + .0025, lat2 = initial_isco_sample_discharger_data$lat - .0025,
      popup = paste0(initial_isco_sample_discharger_data$Parameter, ": ", initial_isco_sample_discharger_data$Result),
      label = initial_isco_sample_discharger_data$`Station Name`, color = "black", weight = 2,
      fillColor = initial_isco_sample_discharger_color_palette(initial_isco_sample_discharger_data$Result), group = "rectangles"
    ) %>%
    addLegend("bottomright",
              pal = initial_isco_sample_color_palette, values = initial_isco_sample_data$Result,
              title = str_c("Ambient ", initial_isco_sample_data$Parameter[1]), opacity = 1
    ) %>%
    addLegend("bottomleft",
              pal = initial_isco_sample_discharger_color_palette, values = initial_isco_sample_discharger_data$Result,
              title = str_c("Discharger ", initial_isco_sample_data$Parameter[1]), opacity = 1
    )

  
  
  

  
  
}

  
